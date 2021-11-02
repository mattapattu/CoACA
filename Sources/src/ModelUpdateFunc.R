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

getModelResults <- function(ratdata, testingdata, sim, src.dir, model.src, setup.hpc) {
  ratName = ratdata@rat
  end_index <- getEndIndex(ratName, ratdata@allpaths, sim, limit = 0.95)
  start_index <- round(end_index / 2)
  if (start_index >= end_index) {
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  models <- testingdata@Models
  creditAssignment <- testingdata@creditAssignment
  
  forloops <- length(models) * length(creditAssignment)
  
  # if (setup.hpc) {
  #   # worker.nodes = mpi.universe.size()-1
  #   # print(sprintf("worker.nodes=%i",worker.nodes))
  #   cl <- makeCluster(5, type = "PSOCK")
  # 
  #   # cl <- startMPIcluster(worker.nodes)
  #   # registerDoMPI(cl)
  # }
  # else {
  #   cl <- makeCluster(3)
  #   # registerDoParallel(cl)
  # }
  # 
  # 
  # 
  # clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes", "negLogLikFunc", "src.dir"))
  # clusterEvalQ(cl, source(paste(src.dir, "ModelClasses.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "PathModel.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "TurnModel.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "HybridModel1.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "HybridModel2.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "HybridModel3.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "HybridModel4.R", sep = "/")))
  # clusterEvalQ(cl, source(paste(src.dir, "BaseClasses.R", sep = "/")))
  # clusterEvalQ(cl, library("TTR"))
  # clusterEvalQ(cl, library("rlist"))
  # clusterEvalQ(cl, library("DEoptim"))
  # 
  # clusterCall(cl, function() {
  #   library(doParallel)
  #   NULL
  # })
  # 
  # registerDoParallel(cl)
  
  
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
  ## END IF
  
  # modelData = updateModelData(ratdata,resMatrix, models)
  allmodelRes <- getAllModelResults(ratdata, resMatrix, testingdata, sim)
  
  if (setup.hpc) {
    #stopCluster(cl)
    # stopImplicitCluster()
    closeCluster(cl)
  }
  else {
    #stopCluster(cl)
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


negLogLikFunc <- function(par, ratdata, half_index, modelData, testModel, sim) {
  alpha <- par[1]
  Model <- modelData@Model
  creditAssignment <- modelData@creditAssignment
  
  # if (Model == "Paths") {
  #     Hinit <- matrix(0, 2, 6)
  #     gamma1 <- par[2]
  #     gamma2 <- par[3]
  # 
  #     probMatrix <- Aca3::getProbMatrix(ratdata@allpaths, alpha, gamma1, gamma2, sim)
  #     path4Probs <- probMatrix[which(probMatrix[, 4] > 0), 4]
  #     path4AboveLim <- which(path4Probs >= 0.95)
  #     result <- rle(diff(path4AboveLim))
  #     path4Converged <- any(result$lengths >= 30 & result$values == 1)
  # 
  #     path10Probs <- probMatrix[which(probMatrix[, 10] > 0), 10]
  #     path10AboveLim <- which(path10Probs >= 0.95)
  #     result <- rle(diff(path10AboveLim))
  #     path10Converged <- any(result$lengths >= 30 & result$values == 1)
  # 
  #     if (path4Converged && path10Converged) {
  #       lik <- Aca3::getPathLikelihood(ratdata@allpaths[1:half_index, ], alpha, gamma1, gamma2, sim)
  #     }
  #     else {
  #       lik <- -1000000
  #     }
  #   
  # }
  #else {
  gamma1 <- par[2]
  gamma2 <- par[3]
  # reward = par[4]
  # reward = 1+reward*9
  reward <- 1
  #
  modelData@alpha <- alpha
  modelData@gamma1 <- gamma1
  modelData@gamma2 <- gamma2
  probMatrix <- TurnsNew::getProbMatrix(ratdata, modelData, testModel, sim)
  path4Probs <- probMatrix[which(probMatrix[, 4] > 0), 4]
  path4AboveLim <- which(path4Probs >= 0.95)
  result <- rle(diff(path4AboveLim))
  path4Converged <- any(result$lengths >= 30 & result$values == 1)
  
  path10Probs <- probMatrix[which(probMatrix[, 10] > 0), 10]
  path10AboveLim <- which(path10Probs >= 0.95)
  result <- rle(diff(path10AboveLim))
  path10Converged <- any(result$lengths >= 30 & result$values == 1)
  
  if (path4Converged && path10Converged) {
    # ratdata@allpaths = ratdata@allpaths[1:half_index,]
    lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, testModel, sim)
    lik <- lik[1:half_index]
  }
  else {
    lik <- -1000000
  }
  #}
  
  negLogLik <- (-1) * sum(lik)
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
