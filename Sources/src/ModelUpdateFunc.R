# library(GA)
library(DEoptim)
# library(Rmpi)
# library(rgenoud)
library(rlist)
library(foreach)
library(doParallel)
# library(doMPI);
# library(snow);
# library(doSNOW);

#library(GA)
library(DEoptim)
#library(Rmpi)
#library(rgenoud)
library(rlist)
#library(parallel)
#library(foreach)
#library(doParallel)
#library(doMPI);
#library(snow);
#library(doSNOW);
library(bigsnpr)



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
  setRngDoMPI(cl, seed=1234)

  initpop <-  matrix(0,40,4)
	initpop[,1] <- rep(0.1,40)
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
	initpop[,1] <- rep(0.1,40)
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

readModelParamsNew <- function(ratdata,res.dir,testingdata, sim){
  
  print(sprintf("Inside readModelParams"))
  models <- testingdata@Models
  


  allmodelRes <- new("AllModelRes")
  setwd(res.dir)
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
    paramTestData=list.files(".", pattern=paste0(rat,".*",modelName,".",creditAssignment,"_ParamRes.Rdata"), full.names=FALSE)
    print(paramTestData)
    load(paramTestData)
    rowlen <- length(modelRes[[1]][,1])
    modelData@alpha <- modelRes[[1]][rowlen, 2]
    modelData@gamma1 <- modelRes[[1]][rowlen, 3]
    if(ncol(modelRes[[1]]) >= 5)
    {
      modelData@gamma2 <- modelRes[[1]][rowlen, 4]
      modelData@lambda <- modelRes[[1]][rowlen, 5]
    }
    modelData <- setModelResults(modelData, ratdata, allModels)
    allmodelRes <- addModelData(allmodelRes, modelData)

  }
  
  return(allmodelRes)
  
}



# negLogLikFunc <- function(par, ratdata, half_index, modelData, testModel, sim) {
#   alpha <- par[1]
#   Model <- modelData@Model
#   creditAssignment <- modelData@creditAssignment
  
#   gamma1 <- par[2]
#   #gamma2 <- par[3]
#   # reward = par[4]
#   # reward = 1+reward*9
#   reward <- 1
#   #
#   modelData@alpha <- alpha
#   modelData@gamma1 <- gamma1
#   #modelData@gamma2 <- gamma2
   
#   if(length(par)==4)
#   {
#     modelData@gamma2 <- par[3]
#     modelData@lambda <- par[4]
#   }

#   simLearns = checkSimLearns(ratdata@allpaths,sim=sim,limit=0.8)
#   if(simLearns)
#   {
#    lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, testModel, sim)
#    lik <- lik[1:half_index]
#    negLogLik <- (-1) * sum(lik)
#   }
#   else
#   {
#    negLogLik = 1000000
#   }

#   probMat <- TurnsNew::getProbMatrix(ratdata, modelData, testModel, sim)
 
#   if(!(length(which(probMat[,4] > 0.8)) > 100 && length(which(probMat[,10] > 0.8)) > 100))
#   {
#     negLogLik = 1000000
#   }


#   # print(sprintf("negLogLik = %f",negLogLik))
#   if (is.infinite(negLogLik)) {
#     return(1000000)
#   } else if (is.nan(negLogLik)) {
#     print(sprintf("Alpha = %f", alpha))
#     return(1000000)
#   }
#   else {
#     return(negLogLik)
#   }
# }
