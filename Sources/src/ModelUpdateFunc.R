# library(GA)
#library(DEoptim)
# 
# library(rgenoud)
library(rlist)
#library(foreach)
#library(doParallel)
#library(Rmpi)
library(doMPI);
# library(snow);
# library(doSNOW);

#library(GA)
#library(DEoptim)
#library(Rmpi)
#library(rgenoud)
#library(rlist)
#library(parallel)
#library(foreach)
#library(doParallel)
#library(doMPI);
#library(snow)
#library(doSNOW)
#library(future)
library(bigsnpr)
#library(Rmpi)
#library(parallelly)
#library(doFuture)
library(listenv)
library(nloptr)


analyzeParamSpace=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name)
{
  models = testData@Models
  #########################
  gamma2 = 0.3
  lambda = 0
  #########################

  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  model.data.dir = paste(model.data.dir,"modelParams",ratName,sep="/")

  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=count)
  exportDoMPI(cl, c("src.dir","model.data.dir","model.src"),envir=environment())
  registerDoMPI(cl)
  
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"))
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }
  
  chunkSize = length(gridMat[,1])/getDoParWorkers()
  #chunkSize = 150
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  print(sprintf("gridMat len=%i, getDoParWorkers=%i",length(gridMat[,1]),getDoParWorkers()))
   
  resMat <- 
      foreach(idx = 1:length(gridMat[,1]), .packages="nloptr", .options.mpi=opts) %dopar% {
            #start_idx=sequences[i]
            #idx = start_idx+j
            alpha = gridMat[idx,1]
            gamma1 = gridMat[idx,2]
            iter = gridMat[idx,3]
            model = gridMat[idx,4]
            cat(sprintf("idx= %i,alpha=%.10f,gamma1=%.10f\n", idx,alpha,gamma1))
            #cat(sprintf('Rat is %s, model is %s\n', ratName,model))


            modelName = strsplit(model,"\\.")[[1]][1]
            #cat(sprintf('rat=%s, iter=%i,modelName = %s\n', ratName,iter,modelName))
            creditAssignment = strsplit(model,"\\.")[[1]][2]
            cat(sprintf('rat=%s, iter=%i,modelName=%s,creditAssignment = %s\n', ratName,iter,modelName,creditAssignment))


            #cat(sprintf('rat=%s, iter=%i,model = %s\n', ratName,iter,modelName))
            modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=2)
            argList<-getArgList(modelData,ratdata)

            #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
            res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
                         fn = negLogLikFunc,ratdata=ratdata,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 2)
            #cat("Here2")
            if(res$convergence < 0)
            {
              modelData = setModelParams(modelData, c(NA,NA,gamma2,lambda))
              c(iter,modelName,NA, NA,modelData@gamma2,modelData@lambda,NA,idx,alpha,gamma1)

            }else
            {
              modelData = setModelParams(modelData, c(res$par,gamma2,lambda))
              if(creditAssignment == "qlearningAvgRwd"||creditAssignment == "aca4")
              {
                #cat("Here")
                lik1 <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2)
                lik1 <- sum(lik1[(1:800)])*-1
                #reprint(lik1)
                if (is.infinite(lik1)) {
                  lik1= 1000000
                }else if (is.nan(lik1)) {
                  #print(sprintf("Alpha = %f", alpha))
                  lik1 = 1000000
                }else if (is.na(lik1)) {
                  #print(sprintf("Alpha = %f, Gamma1=%f", alpha,gamma1))
                  lik1 = 1000000
                }
                #cat(sprintf('Iter=%i, alpha = %.10f, gamma1 = %.15f, gamma2 = %f, lik1=%f\n', iter,modelData@alpha, modelData@gamma1,0.1,lik1))
                c(iter,modelName,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda,lik1,idx,alpha,gamma1)

              }
              else{
                #cat(sprintf('Success: alpha = %f, gamma = %f\n', modelData@alpha, modelData@gamma1))
                c(iter,modelData@alpha, modelData@gamma1)

              }
        

            }
            

      }
   resMat <- Reduce(rbind,resMat)
   print(resMat)
   rat = ratdata@rat
   save(resMat, file = paste0(model.data.dir,"/",rat,"_",name, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMat.Rdata"))   
}


generateParamResMat=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count)
{
  
  #################################

  gamma2 = 0.3
  lambda = 0

  ########################### 

  #models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  model.data.dir = paste(model.data.dir,"modelParams",ratName,sep="/")
 
  resMatList <- listenv()

  for(i in c(1:10))
  {
    setwd(model.data.dir)
    pattern=paste0(ratName,"_mParams",i,"_.*Rdata")
    resMat=list.files(".", pattern=pattern, full.names=FALSE)
    load(resMat)
    resMatList[[i]] <- resMat
  }
  resMat <- Reduce(rbind,resMatList)
  #save(resMat, file = paste0(model.data.dir,"/",rat,"_",name, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMat.Rdata")) 

  df <- as.data.frame(resMat)
  cols.num <- c(1,3,4,5,6,7)
  df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")

  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  
  iter = unique(as.numeric(resMat[,1]))


  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=count)
  exportDoMPI(cl, c("src.dir","model.data.dir","model.src"),envir=environment())
  registerDoMPI(cl)
  
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"))
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }


  chunkSize = ceiling(length(models)*length(iter)/getDoParWorkers())
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 
   
  minDfModels <- foreach(model = models, .inorder=TRUE, .options.mpi=opts) %:% 
    foreach(it = iter, .inorder=TRUE) %dopar%
    {
      print(sprintf("it=%i,model=%s",it,model))
      df_it <- df[which(df[,1]==it & df[,2]==model),]
      min_lik1 = 1000000
      minmodel <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
      modelData <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
      for(idx in 1:length(df_it[,1]))
      {
        modelData@alpha = df_it[idx,3]
        modelData@gamma1 = df_it[idx,4]
        modelData@gamma2 = gamma2
        modelData@lambda = lambda
        
        argList <- getArgList(modelData, ratdata)
        lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2)
        lik1 <- sum(lik[c(1:it)])*-1
        lik2 <- sum(lik[-c(1:800)])*-1
        df_it[idx,7]= lik1
        
        if (is.infinite(lik1)) {
          #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
          lik1= 1000000
          next
        }else if (is.nan(lik1)) {
          #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
          lik1 = 1000000
          next
        }else if (is.na(lik1)) {
          #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
          lik1 = 1000000
          next
        }
        if(lik1 < min_lik1)
        {
          min_lik1=lik1
          min_lik2=lik2
          minmodel@alpha = df_it[idx,3]
          minmodel@gamma1 = df_it[idx,4]
          minmodel@gamma2 = gamma2
          minmodel@lambda = lambda
        }    
      }
      #cat(sprintf("it=%i,model=%s, min_lik1=%i",it,model,min_lik1))
      c(model,it,minmodel@alpha,minmodel@gamma1,minmodel@gamma2,minmodel@lambda,min_lik1,min_lik2)
      
    }
  minDflist <- unlist(minDfModels, recursive = FALSE)
  minDfModels <- Reduce(rbind,minDflist)
  print(minDfModels)
  save(minDfModels, file = paste0(model.data.dir,"/",ratdata@rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_minDfModels.Rdata")) 


}


getModelParams=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count)
{
  models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  model.data.dir = paste(model.data.dir,"modelParams",ratName,sep="/")
   
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=count)

  initpop <-  matrix(0,40,4)
  initpop[,1] <- runif(40, 1e-3, 1e-2)
  initpop[,2] <- seq_log(1e-9, 1e-3, 40)
  initpop[,3] <- 1
    
  exportDoMPI(cl, c("src.dir","model.data.dir","model.src"),envir=environment())
  registerDoMPI(cl)
    
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"))
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }
   
   
    opts <- list(initEnvir=initWorkers) 
  
  for(i in 1:length(models))
  {
    model = models[i] 
    print(sprintf('Model is %s\n', model))
    modelName = strsplit(model,"\\.")[[1]][1]
    creditAssignment = strsplit(model,"\\.")[[1]][2]
    iter=floor(length(ratdata@allpaths[,1])/100)
      #print(iter)
     resMat <- 
       foreach(j=c(1:iter), .combine='rbind', .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"), .inorder=TRUE) %dopar%{
          
	  if(j==iter)
          {
           rowEnd = length(ratdata@allpaths[,1])
          }else{
           rowEnd = j*100
          }
          cat(sprintf('model = %s, rowEnd = %i\n', model,rowEnd))
          modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=2)
          argList<-getArgList(modelData,ratdata)
          np.val = length(argList$lower) * 10
          
          myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
          
          if(creditAssignment == "qlearningAvgRwd")
          {
            myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200, initialpop = initpop)
          }
          out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=rowEnd,modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
          modelData = setModelParams(modelData, unname(out$optim$bestmem))
          if(creditAssignment == "qlearningAvgRwd"||creditAssignment == "aca4")
          {
            lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2)
            lik <- sum(lik[-c(1:800)])*-1
            cat(sprintf('Success: alpha = %f, gamma1 = %f, gamma2 = %f\n', modelData@alpha, modelData@gamma1,modelData@gamma2))
            c(rowEnd,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda,lik)

          }
          else{
            cat(sprintf('Success: alpha = %f, gamma = %f\n', modelData@alpha, modelData@gamma1))
            c(rowEnd,modelData@alpha, modelData@gamma1)

          }
          
        }   
        modelRes <- setNames(list(resMat),model)
        rat = ratdata@rat
        save(modelRes, file = paste0(model.data.dir,"/",rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_",paste(modelName,creditAssignment,sep="."),"_ParamRes.Rdata")) 

        #paramTest = list.append(paramTest,modelRes)

    
   }
  
  rat = ratdata@rat
  #save(paramTest, file = paste0(model.data.dir,"/",rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_modelParamRes.Rdata")) 
  
  if(setup.hpc)
  {
    closeCluster(cl)
  }
  
}



getModelResults=function(ratdata, testingdata, sim, src.dir, model.src, setup.hpc,count)
{
  ratName = ratdata@rat
  #end_index <- getEndIndex(ratName, ratdata@allpaths, sim, limit = 0.95)
  #start_index = round(end_index/2)
  #if(start_index >= end_index){
  #  print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
  #  return()
  #}
  
  models = testData@Models
  
  #forloops = length(models) * length(creditAssignment)
  
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)

  exportDoMPI(cl, c("src.dir","model.src")) 
  #exportDoMPI(cl, c("getEndIndex", "convertTurnTimes","negLogLikFunc","src.dir"))
  registerDoMPI(cl)

  initpop <-  matrix(0,40,4)
  initpop[,1] <- runif(40, 1e-3, 1e-2)
  initpop[,2] <- seq_log(1e-9, 1e-3, 40)
  initpop[,3] <- 1
    
  initWorkers <-  function() {
    source(paste(src.dir, "ModelClasses.R", sep = "/"))
    source(paste(model.src, "PathModel.R", sep = "/"))
    source(paste(model.src, "TurnModel.R", sep = "/"))
    source(paste(model.src, "HybridModel1.R", sep = "/"))
    source(paste(model.src, "HybridModel2.R", sep = "/"))
    source(paste(model.src, "HybridModel3.R", sep = "/"))
    source(paste(model.src, "HybridModel4.R", sep = "/"))
    source(paste(src.dir, "BaseClasses.R", sep = "/"))
    source(paste(src.dir,"exportFunctions.R", sep="/"))
      
      #attach(myEnv, name="sourced_scripts")
  }
    
  opts <- list(initEnvir=initWorkers)
  print("Spawned cluster")
  time <- system.time(
  resList <-
      foreach(model=models, .combine='rbind',.options.mpi=opts,.packages = c("rlist","DEoptim","doMPI"), .inorder=TRUE) %dopar% {
          #envir = ls() 
        cat('model =',model,'\n',sep = '')
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=sim)
        argList<-getArgList(modelData,ratdata)
        cat('Create new cluster\n') 
          #cl2 <- getDoMpiCluster()
        np.val = (length(argList$lower))*10
        myList <- DEoptim.control(NP=np.val, F=0.9, CR = 0.8,trace = FALSE, itermax = 200)
        
        if(creditAssignment == "qlearningAvgRwd")
          {
            myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200, initialpop = initpop)
          }
        
        out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=800,modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
        cat('model = ',model, ', bestmem=',unname(out$optim$bestmem),'\n',sep = '')
        cat('model = ',model, ', bestval=',unname(out$optim$bestval),'\n',sep = '')
        list(res=unname(out$optim$bestmem),	model=model)	
      }
  )
  print(time)
  
  #modelData = updateModelData(ratdata,resMatrix, models)
  allmodelRes = getAllModelResults(ratdata, resList,testingdata, sim) 
  #save(allmodelRes,  file = paste0(ratdata@rat,"_allmodelRes.Rdata"))
  
  if(setup.hpc)
  {
    #stopCluster(cl)	
    #stopImplicitCluster()
    closeCluster(cl)
    #mpi.finalize()
  }
  else
  {
    stopCluster(cl)
    #stopImplicitCluster()
  }
  
  
  return(allmodelRes)
}



getModelResultsSeq <- function(ratdata, testingdata, sim, src.dir) {
  models <- testingdata@Models
  creditAssignment <- testingdata@creditAssignment
  
  cl2 <- makeCluster(5)
  models <- testingdata@Models
  creditAssignment <- testingdata@creditAssignment
  
  forloops <- length(models) * length(creditAssignment)
  
  if (setup.hpc) {
    cl <- makeCluster(5, type = "PSOCK")
  }
  else {
    cl <- makeCluster(3)
    # registerDoParallel(cl)
  }
  
  
  
  clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes", "negLogLikFunc", "src.dir"))
  clusterEvalQ(cl, source(paste(src.dir, "ModelClasses.R", sep = "/")))
  clusterEvalQ(cl, source(paste(src.dir, "TurnModel.R", sep = "/")))
  clusterEvalQ(cl, source(paste(src.dir, "HybridModel1.R", sep = "/")))
  clusterEvalQ(cl, source(paste(src.dir, "HybridModel2.R", sep = "/")))
  clusterEvalQ(cl, source(paste(src.dir, "HybridModel3.R", sep = "/")))
  clusterEvalQ(cl, source(paste(src.dir, "HybridModel4.R", sep = "/")))
  clusterEvalQ(cl, source(paste(src.dir, "BaseClasses.R", sep = "/")))
  clusterEvalQ(cl, library("TTR"))
  clusterEvalQ(cl, library("rlist"))
  clusterEvalQ(cl, library("DEoptim"))
  
  clusterCall(cl, function() {
    library(doParallel)
    NULL
  })
  
  registerDoParallel(cl)
  
  resMatrix <- matrix("", 0, 2)
  
  time <- system.time(
    for (model in models)
    {
      for (method in creditAssignment)
      {
        modelData <- new("ModelData", Model = model, creditAssignment = method, sim = sim)
        argList <- getArgList(modelData, ratdata)
        np.val <- length(argList$lower) * 10
        myList <- DEoptim.control(NP = np.val, F = 8, CR = 0.9, trace = FALSE, itermax = 200, cluster = cl)
        out <- do.call("DEoptim", list.append(argList, fn = negLogLikFunc, myList))
        resMatrix <- rbind(resMatrix, unname(out$optim$bestmem))
      }
    }
  )
  print(time)
  ## END IF
  
  # modelData = updateModelData(ratdata,resMatrix, models)
  allmodelRes <- getAllModelResults(ratdata, resMatrix, testingdata, sim)
  
  stopCluster(cl)
  
  return(allmodelRes)
}

getAllModelResults <- function(ratdata, resList, testingdata, sim) {
  # res = callOptimize(modelData,ratdata,allModels)
  models <- testingdata@Models
  #methods <- testingdata@creditAssignment
  allmodelRes <- new("AllModelRes")
  for (i in 1:length(resList[,1]))
  {
    modelName = strsplit(resList[i,]$model,"\\.")[[1]][1]
    creditAssignment = strsplit(resList[i,]$model,"\\.")[[1]][2]

    modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = sim)
    #index <- length(methods) * (i - 1) + j
    print(sprintf("modelName=%s,creditAssignment=%s",modelName,creditAssignment))
    print(resList[i,]$res)
    modelData <- setModelParams(modelData, resList[i,]$res)
     #debug(setModelResults)
    modelData <- setModelResults(modelData, ratdata, allModels)
    allmodelRes <- addModelData(allmodelRes, modelData)
    
  }
 
  return(allmodelRes)
}

readModelParams <- function(ratdata,res.dir,testingdata, sim){
  
  print(sprintf("Inside readModelParams"))
  models <- testingdata@Models
  methods <- testingdata@creditAssignment
  allmodelRes <- new("AllModelRes")
  setwd(res.dir)
  rat=ratdata@rat
  paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
  print(paramTestData)
  load(paramTestData)
  
  
  for (i in 1:length(models))
  {
    for (j in 1:length(methods))
    {
      modelData <- new("ModelData", Model = models[i], creditAssignment = methods[j], sim = sim)
      index <- length(methods) * (i - 1) + j
      resMatrix <- paramTest[[index]][[1]]
      rowlen <- length(resMatrix[,1])
      #modelData <- setModelParams(modelData, resMatrix[rowlen, ])
      modelData@alpha <- resMatrix[rowlen, 2]
      modelData@gamma1 <- resMatrix[rowlen, 3]
      if(ncol(resMatrix) == 5)
      {
        modelData@gamma2 <- resMatrix[rowlen, 4]
        modelData@lambda <- resMatrix[rowlen, 5]
      }
      #debug(setModelResults)
      modelData <- setModelResults(modelData, ratdata, allModels)
      allmodelRes <- addModelData(allmodelRes, modelData)
    }
  }
  
  return(allmodelRes)
  
}

readModelParamsNew <- function(ratdata,param.model.data.dir,testingdata, sim){
  
  print(sprintf("Inside readModelParams"))
  models <- testingdata@Models
  
  setwd(param.model.data.dir)
  print(sprintf("param.model.data.dir=%s",param.model.data.dir))
  ratName = ratdata@rat
  load(list.files(".", pattern=paste0(ratName,".*minDfModels.Rdata"), full.names=FALSE))
  allModelRes <- minDfModels
  modelParamsList <- minDfModels[which(as.numeric(minDfModels[,2])==  length(ratdata@allpaths[,1])),]

  allmodelRes <- new("AllModelRes")
  #setwd(res.dir)
  #print(res.dir)
  rat=ratdata@rat
  #paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
  #print(paramTestData)
  #load(paramTestData)
  
  
  for (i in 1:length(models))
  {
    modelName = strsplit(models[i],"\\.")[[1]][1]
    creditAssignment = strsplit(models[i],"\\.")[[1]][2]
    modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = sim)
    print(sprintf("modelName=%s,creditAssignment=%s",modelName,creditAssignment))

    modelData@alpha <- as.numeric(modelParamsList[which(modelParamsList[,1]==modelName),3])
    modelData@gamma1 <- as.numeric(modelParamsList[which(modelParamsList[,1]==modelName),4])
    if(creditAssignment == "qlearningAvgRwd")
    {
      modelData@gamma2 <- as.numeric(modelParamsList[which(modelParamsList[,1]==modelName),5])
      modelData@lambda <- as.numeric(modelParamsList[which(modelParamsList[,1]==modelName),6])
    }
    modelData <- setModelResults(modelData, ratdata, allModels)
    allmodelRes <- addModelData(allmodelRes, modelData)

  }
  
  return(allmodelRes)
  
}