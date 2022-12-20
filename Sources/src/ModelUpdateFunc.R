# library(GA)
library(DEoptim)
# library(Rmpi)
# library(rgenoud)
library(rlist)
#library(foreach)
#library(doParallel)
# library(doMPI);
# library(snow);
# library(doSNOW);

#library(GA)
#library(Rmpi)
#library(rgenoud)
#library(parallel)
#library(foreach)
#library(doParallel)
#library(doMPI);
#library(snow);
#library(doSNOW);
library(bigsnpr)
library(NMOF)
library(doFuture)
library(TurnsNew)
library("listenv")


analyzeParamSpaceWrapper = function(ratdata,testData,src.dir, model.src)
{
  
  registerDoFuture()
  cl <- makeCluster(6)
  #plan(cluster, workers = cl)
  plan(list(tweak(cluster, workers = cl), tweak(cluster, workers = cl)))
  
  alpha_seq = seq_log(1e-3, 1e-2,5)
  gamma1_seq = seq_log(1e-8, 1e-4, 2)
  iters=c(300,800,1100,1500,length(ratdata@allpaths[,1]))
  #models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  models = testData@Models
  gridMat<- expand.grid(alpha_seq,gamma1_seq,iters,models,stringsAsFactors = FALSE)
  
  sequences = seq(0,300, length.out=7)
  
  i=1
  resList <- listenv()
  time2 <-system.time(
  for(i in c(1:6)){
    start_idx=sequences[i]+1
    end_idx=sequences[i+1]
    
    resList[[i]] %<-% {
    print(sprintf("start_idx=%i,end_idx=%i",start_idx,end_idx))
    X <- analyzeParamSpace(ratdata,testData,src.dir, model.src, gridMat[start_idx:end_idx,])
    }
  }
  
  )
  

  resMat <- Reduce(rbind,resList)
  print(time2)
  print(resMat)
}



analyzeParamSpace=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir)
{
  
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  model.data.dir = "C:/Projects/Rats-Credits/Data"
  
  cl <- makeCluster(5, outfile = "")
  #registerDoParallel(cl)
  clusterExport(cl, varlist = c("model.src","src.dir","negLogLikFunc","checkSimLearns","getModelResults","getAllModelResults"))
  clusterEvalQ(cl, source(paste(src.dir,"ModelClasses.R", sep="/")))
  clusterEvalQ(cl, source(paste(model.src,"PathModel.R", sep="/")))
  clusterEvalQ(cl, source(paste(model.src,"TurnModel.R", sep="/")))
  clusterEvalQ(cl, source(paste(model.src,"HybridModel1.R", sep="/")))
  clusterEvalQ(cl, source(paste(model.src,"HybridModel2.R", sep="/")))
  clusterEvalQ(cl, source(paste(model.src,"HybridModel3.R", sep="/")))
  clusterEvalQ(cl, source(paste(model.src,"HybridModel4.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"BaseClasses.R", sep="/")))
  clusterExport(cl, varlist = c("ratdata"),envir=environment())
  #clusterEvalQ(cl, library("TTR"))
  #clusterEvalQ(cl, library("dplyr"))
  #clusterEvalQ(cl, library("DEoptim"))

  clusterCall(cl, function() {
    library(doParallel)
    NULL
  })

  registerDoParallel(cl)

  
  alpha_seq = seq_log(1e-3, 0.9,1)
  gamma1_seq = seq_log(1e-8, 1e-4, 1)
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
  models = testData@Models
  gridMat<- expand.grid(alpha_seq,gamma1_seq,iters,models,stringsAsFactors = FALSE)
  #start_idx <- 35201
  #end_idx <- 39600
  
  #gridMat <- gridMat[start_idx:end_idx,]
  
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
  # registerDoFuture()
  # cl <- makeCluster(5)
  # plan(cluster, workers = cl)
  #resMat <- listenv()
  print(availableCores())
  chunkSize = length(gridMat[,1])/10

  resMat <- 
    foreach(idx = 1:length(gridMat[,1]),  .packages="nloptr") %dopar% {
      initWorkers()
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
      #cat(sprintf('rat=%s, iter=%i,creditAssignment = %s\n', ratName,iter,creditAssignment))
      
      
      #cat(sprintf('rat=%s, iter=%i,model = %s\n', ratName,iter,modelName))
      modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=2)
      argList<-getArgList(modelData,ratdata)
      
      #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
      #cat("Here1")
      res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
                    fn = negLogLikFunc,ratdata=ratdata,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 2)
      
      #cat("Here2")
      modelData = setModelParams(modelData, c(res$par,0.1,0))
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
  

 resMat <- Reduce(rbind,resMat)    
 print(time1)



  # #load("C:/Users/matta/Downloads/rat_113_20220921_103450_resMat.Rdata")
  # load("C:/Users/matta/Downloads/rat_112_20220921_142518_resMat.Rdata")
  # 
  # df <- as.data.frame(resMat)
  # cols.num <- c(1,3,4,5,6,7)
  # df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  # models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  # # minDfModels <- data.frame(model=character(),
  # #                           iter=integer(),
  # #                           alpha=double(),
  # #                           gamma1=double(),
  # #                           gamma2=double(),
  # #                           lambda=double(),
  # #                           stringsAsFactors=FALSE)
  # 
  # iter = c(300,800,1100,1500,length(ratdata@allpaths[,1]))
  # 
  # minDfModels <- foreach(model = models,.combine='rbind', .inorder=TRUE) %:% 
  #   foreach(it = iter,.combine='rbind', .inorder=TRUE) %dopar%
  # {
  #   df_it <- df[which(df[,1]==it & df[,2]==model),]
  #   min_lik1 = 1000000
  #   minmodel = modelData <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
  #   for(idx in 1:length(df_it[,1]))
  #   {
  #     modelData@alpha = df_it[idx,3]
  #     modelData@gamma1 = df_it[idx,4]
  #     modelData@gamma2 = 0.1
  #     modelData@lambda = 0
  #     argList <- getArgList(modelData, ratdata)
  #     lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2)
  #     lik1 <- sum(lik[c(1:800)])*-1
  #     lik2 <- sum(lik[-c(1:800)])*-1
  #     df_it[idx,7]= lik1
  #     
  #     if (is.infinite(lik1)) {
  #       lik1= 1000000
  #     }else if (is.nan(lik1)) {
  #       #print(sprintf("Alpha = %f", alpha))
  #       lik1 = 1000000
  #     }else if (is.na(lik1)) {
  #       #print(sprintf("Alpha = %f, Gamma1=%f", alpha,gamma1))
  #       lik1 = 1000000
  #     }
  #     if(lik1 < min_lik1)
  #     {
  #       min_lik1=lik1
  #       min_lik2=lik2
  #       minmodel@alpha = df_it[idx,3]
  #       minmodel@gamma1 = df_it[idx,4]
  #       minmodel@gamma2 = 0.1
  #       minmodel@lambda = 0
  #     }
  #       
  #     
  #     
  #     
  #    }
  #   c(model,it,minmodel@alpha,minmodel@gamma1,minmodel@gamma2,minmodel@lambda,min_lik1,min_lik2)
  #     
  # }
  # 
  # print(minDfModels)
  # 
  # save(minDfModels, file = paste0(model.data.dir,"/",ratdata@rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMat.Rdata")) 
  # 
  # return(minDfModels)
  return(resMat)
}

generateParamResMat=function(ratdata,model.data.dir)
{
  
  #models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  #model.data.dir = paste(model.data.dir,"modelParams",ratName,sep="/")
  
  cl <- makeCluster(5, outfile = "")
  clusterExport(cl, varlist = c("model.src","src.dir","negLogLikFunc","checkSimLearns","getModelResults","getAllModelResults"))
  clusterExport(cl, varlist = c("ratdata"),envir=environment())

  clusterCall(cl, function() {
    library(doParallel)
    NULL
  })
  
  registerDoParallel(cl)
  
  
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
  
  setwd(model.data.dir)
  resMatList <- listenv()
  
  for(i in c(1:10))
  {
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
  

  iter = unique(as.numeric(resMat[,1]))
  
  minDfModels <- foreach(model = models, .inorder=TRUE) %:% 
    foreach(it = iter, .inorder=TRUE) %dopar%
    {
      initWorkers()
      print(sprintf("it=%i,model=%s",it,model))
      df_it <- df[which(df[,1]==it & df[,2]==model),]
      min_lik1 = 1000000
      minmodel = modelData <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
      for(idx in 1:length(df_it[,1]))
      {
        modelData@alpha = df_it[idx,3]
        modelData@gamma1 = df_it[idx,4]
        modelData@gamma2 = 0.1
        modelData@lambda = 0
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
          minmodel@gamma2 = 0.1
          minmodel@lambda = 0
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


getModelParams=function(ratdata,testData)
{
  models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  model.data.dir = "C:/Projects/Rats-Credits/Data"
  

  initpop <-  matrix(0,40,4)
  initpop[,1] <- runif(40, 1e-3, 1e-2)
  initpop[,2] <- seq_log(1e-9, 1e-3, 40)
  initpop[,3] <- 1
  
  for(i in 1:length(models))
  {
    model = models[i] 
    print(sprintf('Model is %s\n', model))
    modelName = strsplit(model,"\\.")[[1]][1]
    creditAssignment = strsplit(model,"\\.")[[1]][2]
    iter=floor(length(ratdata@allpaths[,1])/100)
    print(iter)
    resMat <- matrix(0,iter,6)
    for(j in c(1:iter))
    {
        
      if(j==iter)
      {
        rowEnd = length(ratdata@allpaths[,1])
      }else{
        rowEnd = j*100
      }
      cat(sprintf('j=%i,model = %s, rowEnd = %i\n', j,model,rowEnd))
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
        lik <- sum(lik[c(1:800)])*-1
        cat(sprintf('Success: alpha = %f, gamma1 = %f, gamma2 = %f\n', modelData@alpha, modelData@gamma1,modelData@gamma2))
        resMat[j,]=c(rowEnd,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda,lik)
        
      }
      else{
        cat(sprintf('Success: alpha = %f, gamma = %f\n', modelData@alpha, modelData@gamma1))
        resMat[j,]=c(rowEnd,modelData@alpha, modelData@gamma1)
        
      }
      
    }   
    
    
    
    rat = ratdata@rat
    save(resMat, file = paste0(model.data.dir,"/",rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMat.Rdata")) 
    
    #paramTest = list.append(paramTest,modelRes)
    
    
  }
  
}

getModelResults=function(ratdata, testingdata, sim, src.dir, model.src, setup.hpc)
{
  ratName = ratdata@rat
  # end_index <- getEndIndex(ratName, ratdata@allpaths, sim, limit = 0.95)
  # start_index = round(end_index/2)
  # if(start_index >= end_index){
  #   print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
  #   return()
  # }
  
  models = testingdata@Models
  creditAssignment = testingdata@creditAssignment
  print(models)
  time <- system.time(
      resMatrix <-
        foreach(model = models, .combine = "rbind",.inorder=TRUE) %do% {
          
          modelName = strsplit(model,"\\.")[[1]][1]
          print(modelName)
          creditAssignment = strsplit(model,"\\.")[[1]][2]
          print(creditAssignment)
          #print(sprintf("Model=%s, iter=%i",modelName,iter))
          modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = 2)
          argList <- getArgList(modelData, ratdata)
          nvars <- length(argList$lower)
          np.val <- nvars * 10
          argList$half_index=800
          initpop <-  matrix(0,40,4)
          initpop[,1] <- runif(40, 1e-3, 1e-2)
          initpop[,2] <- seq_log(1e-9, 1e-3, 40)
          initpop[,3] <- 1
          print("Running deoptim")
          myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200,initialpop = initpop)
          out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=argList[[4]],modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)          
          if(out$optim$bestval < 100000)
          {
            print(out$optim$bestmem)
            return(out$optim$bestmem)
          }
          else{
            print("Optim bestval=1e6")
          }
          
          
        }
      
      # print(time)
  )
  print(time)
    

  
  #modelData = updateModelData(ratdata,resMatrix, models)
  allmodelRes = getAllModelResults(ratdata, resMatrix,testingdata, sim) 
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
        myList <- DEoptim.control(NP = np.val, F = 2, CR = 0.9, trace = FALSE, itermax = 200, cluster = cl)
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
      #debug(setModelResults)
      modelData <- setModelResults(modelData, ratdata, allModels)
      allmodelRes <- addModelData(allmodelRes, modelData)
    }
  }
  
  return(allmodelRes)
}

readModelParamsNew <- function(ratdata,param.model.data.dir,testData, sim){
  
  print(sprintf("Inside readModelParams"))
  models <- testData@Models
  testSuite = testData@testSuite
  
  setwd(param.model.data.dir)
  print(sprintf("param.model.data.dir=%s, testSuite=%s",param.model.data.dir,testSuite))
  ratName = ratdata@rat
  load(list.files(".", pattern=paste0(ratName,"_",testSuite,".*resMatList.Rdata"), full.names=FALSE))
  modelParamsList <- resMat[which(as.numeric(resMat[,1])==  length(ratdata@allpaths[,1])),]
  
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
    
    modelData@alpha <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),3])
    modelData@gamma1 <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),4])
    modelData@gamma2 <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),5])
    modelData@lambda <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),6])
    
    modelData <- setModelResults(modelData, ratdata, allModels)
    allmodelRes <- addModelData(allmodelRes, modelData)
    
  }
  
  return(allmodelRes)
  
}


printModelParams <- function(testingdata,allmodelResList){
  
  print(sprintf("Inside printModelParams"))
  models <- testingdata@Models
  methods <- testingdata@creditAssignment
  allmodelRes <- new("AllModelRes")
  mat <- matrix("",7,6)
  colnames(mat) <- models
  
  for(i in c(3:6))
  {
    allModelRes = allmodelResList[[i]]
    for (m in 1:length(models))
    {
      modelData <- slot(slot(allModelRes,models[m]),"aca2")
      alpha = modelData@alpha
      gamma1 = modelData@gamma1
      mat[i,m] = paste0("\u03b1=",round(alpha,2),",\u03b3=",round(gamma1,2))
    }
  }
  mat<-cbind(c("rat1","rat2","rat3","rat4","rat5", "rat6","rat7"),mat)
  #pdf("modelparams.pdf", height=11, width=8.5)
  grid.table(mat)
  #dev.off()
  
}

checkSimLearns=function(ratdata, modelData, testModel,sim,limit)
{
  #end_index = -1
  allpaths <- ratdata@allpaths
  simLearns = FALSE
  sessions <- allpaths[,5]
  uniqueSessIds <- unique(sessions)
  counter = 0
  
  for(sess in uniqueSessIds)
  {
    sessIdx <- which(allpaths[,5]==sess)
    rewards_sess <- allpaths[sessIdx,3]
    successRate <- sum(rewards_sess)/length(sessIdx)
    if(successRate >= limit)
    {
      end_index = sessIdx[length(sessIdx)]
      counter=counter+1
    }
  }
  # if(counter>=2)
  # {
  #   simLearns = TRUE
  # }
  
  # if(simLearns)
  # {
  #   probMat <- TurnsNew::getProbMatrix(ratdata, modelData, testModel, sim)
  #   if((length(which(probMat[,4] > 0.8)) > 10 && length(which(probMat[,10] > 0.8)) > 10))
  #   {
  #     simLearns = TRUE
  #   }else{
  #     simLearns = FALSE
  #   }
  #   
  # }
  
  return(simLearns)
}

checkSimLearns2=function(ratdata,modelData,testModel,sim){
  
  probMat <- TurnsNew::getProbMatrix(ratdata, modelData, testModel, sim)
  if(length(which(probMat[,4] > 0.8)) > 10 && length(which(probMat[,10] > 0.8)) > 10)
  {
    learns = TRUE
  }else{
    learns = FALSE
  }
  
  return(learns)
  
}

negLogLikFunc <- function(par, ratdata, half_index, modelData, testModel, sim) {
  alpha <- par[1]
  Model <- modelData@Model
  creditAssignment <- modelData@creditAssignment
  
  gamma1 <- par[2]
  
  modelData@alpha <- alpha
  modelData@gamma1 <- gamma1
  
  if(alpha < 0 || gamma1 < 0)
  {
    return(1000000)
  }
  
  #simLearns = checkSimLearns(ratdata@allpaths,sim=sim,limit=0.8)
  
  lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, testModel, sim)
  lik <- lik[1:half_index]
  negLogLik <- (-1) * sum(lik)
  
  probMat <- TurnsNew::getProbMatrix(ratdata, modelData, testModel, sim)
  
  if(!(length(which(probMat[,4] > 0.8)) > 10 && length(which(probMat[,10] > 0.8)) > 10))
  {
    negLogLik = 1000000
  }
  
  # print(sprintf("negLogLik = %f",negLogLik))
  if (is.infinite(negLogLik)) {
    return(1000000)
  }else if (is.nan(negLogLik)) {
    #print(sprintf("Alpha = %f", alpha))
    return(1000000)
  }
  else {
    return(negLogLik)
  }
} 

