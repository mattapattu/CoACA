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

getModelResults=function(ratdata, testingdata, sim, src.dir, model.src, setup.hpc)
{
  ratName = ratdata@rat
  end_index <- getEndIndex(ratName, ratdata@allpaths, sim, limit = 0.95)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  models = testingdata@Models
  creditAssignment = testingdata@creditAssignment
  
  forloops = length(models) * length(creditAssignment)
  
  if(setup.hpc)
  {
    cl <- startMPIcluster(6,bcast=FALSE)
    exportDoMPI(cl, "src.dir") 
    #exportDoMPI(cl, c("getEndIndex", "convertTurnTimes","negLogLikFunc","src.dir"))
    registerDoMPI(cl)
    
    initWorkers <-  function() {
      #myEnv <- new.env()
      #cl2 <- startMPIcluster(5,verbose=TRUE)
      #exportDoMPI(cl2, c("getEndIndex", "convertTurnTimes","negLogLikFunc","src.dir"))
      #ignore <- foreach(icount(getDoParWorkers()), .options.mpi=opts) %dopar% NULL
      #registerDoMPI(cl2)
      #cl2 <- getDoMpiCluster()
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
      resMatrix <-
        foreach(model=models, .combine='rbind',.options.mpi=opts,.packages = c("rlist","DEoptim","doMPI")) %:%
        foreach(method=creditAssignment, .combine='rbind') %dopar% {
          #envir = ls() 
          cat('model =',model,'\n',sep = '')
          modelData =  new("ModelData", Model=model, creditAssignment = method, sim=sim)
          argList<-getArgList(modelData,ratdata)
          cat('Create new cluster\n') 
          #cl2 <- getDoMpiCluster()
          np.val = length(argList$lower)*10
          myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
          out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=argList[[4]],modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
          unname(out$optim$bestmem)		
        }
    )
    print(time)
  }
  else
  {
    time <- system.time(
      resMatrix <-
        foreach(model = models, .combine = "rbind") %:%
        foreach(method = creditAssignment, .combine = "rbind") %do% {
          modelData <- new("ModelData", Model = model, creditAssignment = method, sim = sim)
          argList <- getArgList(modelData, ratdata)
          nvars <- length(argList$lower)
          cl2 <- makeCluster(5)
          clusterExport(cl2, varlist = c("src.dir","model.src"))
          clusterCall(cl2, function() {
            source(paste(src.dir, "ModelClasses.R", sep = "/"))
            source(paste(model.src, "PathModel.R", sep = "/"))
            source(paste(model.src, "TurnModel.R", sep = "/"))
            source(paste(model.src, "HybridModel1.R", sep = "/"))
            source(paste(model.src, "HybridModel2.R", sep = "/"))
            source(paste(model.src, "HybridModel3.R", sep = "/"))
            source(paste(model.src, "HybridModel4.R", sep = "/"))
            source(paste(src.dir, "BaseClasses.R", sep = "/"))
            NULL
          })
          registerDoParallel(cl2)
          np.val <- length(argList$lower) * 10
          myList <- DEoptim.control(NP = 30, F = 0.8, CR = 0.9, trace = FALSE, itermax = 200)
          out <- do.call("DEoptim", list.append(argList, fn = negLogLikFunc, myList))
          stopCluster(cl2)
          if(out$optim$bestval < 100000)
          {
            return(out$optim$bestmem)
          }
          else
          {
            return()
          }
          
        }
      
      # print(time)
    )
    print(time)
    
  }
  
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

getAllModelResults <- function(ratdata, resMatrix, testingdata, sim) {
  # res = callOptimize(modelData,ratdata,allModels)
  models <- testingdata@Models
  methods <- testingdata@creditAssignment
  allmodelRes <- new("AllModelRes")
  for (i in 1:length(models))
  {
    for (j in 1:length(methods))
    {
      modelData <- new("ModelData", Model = models[i], creditAssignment = methods[j], sim = sim)
      index <- length(methods) * (i - 1) + j
      modelData <- setModelParams(modelData, resMatrix[index, ])
      #debug(setModelResults)
      modelData <- setModelResults(modelData, ratdata, allModels)
      allmodelRes <- addModelData(allmodelRes, modelData)
    }
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


printModelParams <- function(testingdata,allmodelResList){
  
  print(sprintf("Inside printModelParams"))
  models <- testingdata@Models
  methods <- testingdata@creditAssignment
  allmodelRes <- new("AllModelRes")
  mat <- matrix("",7,6)
  colnames(mat) <- models
  
  for(i in c(1:7))
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

negLogLikFunc <- function(par, ratdata, half_index, modelData, testModel, sim) {
  
  alpha <- par[1]
  Model <- modelData@Model
  creditAssignment <- modelData@creditAssignment
  
  gamma1 <- par[2]
  #gamma2 <- par[3]
  # reward = par[4]
  # reward = 1+reward*9
  reward <- 1
  #
  modelData@alpha <- alpha
  modelData@gamma1 <- gamma1
  #modelData@gamma2 <- gamma2
  
  simLearns = checkSimLearns(ratdata@allpaths,sim=sim,limit=0.8)
  if(simLearns)
  {
    lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, testModel, sim)
    lik <- lik[1:half_index]
    negLogLik <- (-1) * sum(lik)
  }
  else
  {
    negLogLik = 1000000
  }
  
  # print(sprintf("negLogLik = %f",negLogLik))
  if (is.infinite(negLogLik)) {
    return(1000000)
  } else if (is.nan(negLogLik)) {
    print(sprintf("Alpha = %f", alpha))
    return(1000000)
  }
  else {
    return(negLogLik)
  }
}
