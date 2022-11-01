library(doMPI)
library(rlist)
library(nloptr)
library(stringr)

unitTestProbDiffV4=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count)
{
  ## Test settings ###############
  
  StabilityTest = TRUE 
  models = testData@Models
  ##################################
  
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  param.model.data.dir=file.path(model.data.dir, ratName)
  param.model.data.dir=file.path(param.model.data.dir, "modelParams")
  allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

  res.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
   
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')
 
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed)
    
  exportDoMPI(cl, c("src.dir","model.data.dir"),envir=environment())
  registerDoMPI(cl)
    
   initWorkers <-  function() {
      source(paste(src.dir,"../ModelClasses.R", sep="/"))
      source(paste(src.dir,"PathModel.R", sep="/"))
      source(paste(src.dir,"TurnModel.R", sep="/"))
      source(paste(src.dir,"HybridModel1.R", sep="/"))
      source(paste(src.dir,"HybridModel2.R", sep="/"))
      source(paste(src.dir,"HybridModel3.R", sep="/"))
      source(paste(src.dir,"HybridModel4.R", sep="/"))
      source(paste(src.dir,"../BaseClasses.R", sep="/"))
      source(paste(src.dir,"../exportFunctions.R", sep="/"))
      source(paste(src.dir,"../ModelUpdateFunc.R", sep="/"))
      #attach(myEnv, name="sourced_scripts")
    }
   
   #chunkSize = 150
   opts <- list(initEnvir=initWorkers) 
 
   source(paste(src.dir,"../exportFunctions.R", sep="/")) 
   generatedDataList <-  
    foreach(i=1:length(models), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %dopar%
    {
      print(sprintf("model is %s",models[i]))  
      model = models[i] 

      modelName = strsplit(model,"\\.")[[1]][1]
      creditAssignment = strsplit(model,"\\.")[[1]][2]
      trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
       
        #trueModelData = modifyModelData(trueModelData) 
      simLearns = FALSE 
      missedOptimalIter = 0
        
      while(!simLearns)
      {
      
         res = testSimulateData(trueModelData,ratdata,allModels)
         generatedData = res$genData
                  
          #end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.95)
        simLearns = checkSimLearns(generatedData@allpaths,sim=1,limit=0.8) 
        missedOptimalIter=missedOptimalIter+1
          
        if(missedOptimalIter==500)
        {
          cat(sprintf('model = %s, missedOptimalIter = %i, trueAlpha = %f, trueGamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1))
          break
        }
          #cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
      }

        
      if(simLearns)
      {
        
        cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
        generatedData = populateSimRatModel(ratdata,generatedData,modelName)
        generatedData@simModel = trueModelData@Model
        generatedData@simMethod = trueModelData@creditAssignment
        generatedData@simModelData = trueModelData


        probMat_true = res$probMat
        argList <- getArgList(trueModelData, generatedData)
        probMat <-  TurnsNew::getProbMatrix(generatedData, trueModelData, argList[[6]], sim=1)
        y <- probMat[,c(1:12)]-probMat_true[,c(1:12)]
        if(any(y!=0))
        {
          cat(sprintf("Fail! Non-zero element found."))
          print(which(y!=0,arr.ind = T))
          print(y[which(y!=0)])
        }else{
          cat(sprintf("Pass: Both prob matrices are same"))
        }
      }
      generatedData  
    } 

    save(generatedDataList,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_unitTestGenDataset.Rdata"))

}

generateDataV4=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,seed,count, gridMat, name)
{
  ## Test settings ###############
  
  StabilityTest = TRUE 
  ##################################
  models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  param.model.data.dir=file.path(model.data.dir, ratName)
  param.model.data.dir=file.path(param.model.data.dir, "modelParams")

  allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

  res.model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(res.model.data.dir,"Datasets"), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, "Datasets")
   
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')
 
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed)
    
  exportDoMPI(cl, c("model.src","src.dir","model.data.dir","gamma2_Global", "lambda_Global","allModels"), envir=environment())
  registerDoMPI(cl)
    
initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }   
   chunkSize = 2
   #chunkSize = 150
   opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 
 
  
   generatedDataList <-  
   foreach(i=1:length(gridMat[,1]), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %dopar%
   {
        
      model = gridMat[i,1] 
      index = as.integer(gridMat[i,2])
      cat(sprintf('model = %s, index = %i, \n', model,index))

      modelName = strsplit(model,"\\.")[[1]][1]
      creditAssignment = strsplit(model,"\\.")[[1]][2]
      trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
      trueModelData_mod = modifyModelData(trueModelData)
       
        #trueModelData = modifyModelData(trueModelData) 
      simLearns = FALSE 
      missedOptimalIter = 0
        
      while(!simLearns)
      {
        if(StabilityTest)
        {
         #trueModelData_mod = modifyModelData(trueModelData)
         generatedData = simulateData(trueModelData_mod,ratdata,allModels)
        }else{
         generatedData = simulateData(trueModelData,ratdata,allModels)
        }
          
          #end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.95)
        generatedData = populateSimRatModel(ratdata,generatedData,modelName)
        
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)  
        argList<-getArgList(modelData,generatedData)

        simLearns = checkSimLearns(generatedData, modelData = trueModelData_mod, testModel = argList[[6]],sim=1,limit=0.8) 
        missedOptimalIter=missedOptimalIter+1
          
        if(missedOptimalIter==500)
        {
          cat(sprintf('model = %s, missedOptimalIter = %i, trueAlpha = %f, trueGamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1))
          break
        }
        #cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %.10f\n', model,missedOptimalIter,trueModelData_mod@alpha, trueModelData_mod@gamma1)) 
      }

        
      if(simLearns)
      {
        if(StabilityTest)
        {
          cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %.10f\n', model,missedOptimalIter,trueModelData_mod@alpha, trueModelData_mod@gamma1)) 
          generatedData = populateSimRatModel(ratdata,generatedData,modelName)
          generatedData@simModel = trueModelData_mod@Model
          generatedData@simMethod = trueModelData_mod@creditAssignment
          generatedData@simModelData = trueModelData_mod
        }else{
          cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
          generatedData = populateSimRatModel(ratdata,generatedData,modelName)
          generatedData@simModel = trueModelData@Model
          generatedData@simMethod = trueModelData@creditAssignment
          generatedData@simModelData = trueModelData
       }
       generatedData

      }
        
    } 

  allData<-unlist(generatedDataList)  
  rat=ratdata@rat
  print(sprintf("Generated DataList")) 
  if(StabilityTest)
  {
    save(allData,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_Stability_genDataset.Rdata"))
  }else{
    save(allData,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_Conv_genDataset.Rdata"))
  }  
    
}

                      
HoldoutTestV4=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)
{
  
  ## Test settings ###############
  StabilityTest = TRUE 
  DataGenerated = TRUE
  
  ##################################

  testDataName = testData@Name
  models = testData@Models
  ratName = ratdata@rat
  param.model.data.dir=file.path(model.data.dir, ratName)
  param.model.data.dir=file.path(param.model.data.dir, "modelParams")

  gen.data.dir=file.path(model.data.dir, ratName)
  gen.data.dir=file.path(gen.data.dir, "Datasets")
  allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)
  

  res.model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(res.model.data.dir,"holdoutTest"), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, "holdoutTest")

  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  
  print(sprintf("models: %s",toString(models)))
  
  setwd(gen.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  #print(dfData)
  #dfData <- dfData[which(str_detect(dfData,paste0("GenData",genDataList,"_")))]
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    #print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    load(res)
    print(res)
    genDataFiles[[i]] <- allData

    #genDataFiles[[i]] <- get(load(dfData[[i]]))
  }
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed) 
  exportDoMPI(cl, c("model.src","src.dir","model.data.dir","gamma2_Global", "lambda_Global", "genDataFiles"), envir=environment())
  registerDoMPI(cl)
   
  cat(sprintf('Running validation with %d worker(s)\n', getDoParWorkers()))
   
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }
    source(paste(src.dir,"exportFunctions.R", sep="/"))
  
  #modelNum =  length(allData)
  
  #chunkSize = 300
  chunkSize = length(gridMat[,1])/getDoParWorkers()
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  resList<-
      foreach(idx = 1:length(gridMat[,1]), .packages=c("DEoptim","stringr","tictoc"), .options.mpi=opts) %dopar%
      { 
        
        #source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())

        genDataFileNum = as.numeric(gridMat[idx,1])
        genDataNum = as.numeric(gridMat[idx,2])
        testModel = gridMat[idx,3]

        genDataList <- genDataFiles[[genDataFileNum]]
        cat(sprintf("genDataFileNum= %i,genDataNum=%i\n", genDataFileNum,genDataNum))

        if(length(genDataList) < genDataNum)
        {
          print(sprintf("idx=%i,genDataFileNum=%i,genDataNum=%i does not exist ",idx,genDataFileNum,genDataNum))
          #return(c(iter = iter,model=model,NA, NA,NA,NA, NA, NA,NA,NA,genDataFileNum=genDataFileNum,genDataNum=genDataNum))
          return(NULL)
        }else
        {
          generatedData = genDataList[[genDataNum]]
        }

        trueModelData = generatedData@simModelData

         

        modelName = strsplit(testModel,"\\.")[[1]][1]
        creditAssignment = strsplit(testModel,"\\.")[[1]][2]
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
        argList<-getArgList(modelData,generatedData)
        

        #cat(sprintf('rat=%s, model = %s, creditAssignment=%s\n', ratName,modelName,creditAssignment))
            #cat(sprintf('rat=%s, iter=%i,creditAssignment = %s\n', ratName,iter,creditAssignment))


        #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
        #  res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
        #                  fn = negLogLikFunc,ratdata=generatedData,half_index=800,modelData=modelData,testModel = argList[[6]],sim = 1)

        myList <- DEoptim.control(initialpop=initpop, F=0.8, CR = 0.9,trace = FALSE, itermax = 30)
        out <-DEoptim(negLogLikFunc,lower=c(0,0),upper=c(1,1),ratdata=generatedData,half_index=800,modelData=modelData,testModel = argList[[6]],sim = 1,myList)
        
        modelData = setModelParams(modelData, c(out$optim$bestmem,modelData@gamma2,modelData@lambda))
        cat(sprintf('rat=%s, model = %s, creditAssignment=%s\n', ratName,modelName,creditAssignment))
        #modelData = setModelResults(modelData,generatedData,allModels)
        list(model=testModel,trueModel=trueModelData@Model,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda, trueModelData@alpha, trueModelData@gamma1,trueModelData@gamma2,trueModelData@lambda,genDataFileNum=genDataFileNum,genDataNum=genDataNum)
      }
  
  
  resList <- Reduce(rbind,resList)
  rat = ratdata@rat
  save(resList,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_HoldoutResList.Rdata"))
 
  
  if(setup.hpc)
  {
    #stopCluster(cl)
    #stopImplicitCluster()
    closeCluster(cl)
  }
  else
  {
    stopCluster(cl)
    stopImplicitCluster()
  }
  
  #boxplotMse(mat_res,model,rat)
  
  # if(!checkValidation(mat_res,model,rat)){
  #   validated = FALSE
  #   break
  # }
  
}

combineHoldoutResListsV4=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite)
{
      ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside combineHoldoutResLists"))
  ratName = ratdata@rat
  models = testData@Models
  #param.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
  #allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

  res.model.data.dir=paste(model.data.dir,"holdoutTest",ratName,sep="/")
  gen.data.dir=file.path(model.data.dir, ratName)
  gen.data.dir=file.path(gen.data.dir, "Datasets")
  #print(res.model.data.dir) 
  print(src.dir)
  print(model.src)
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  setwd(gen.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  genDataFiles <- list()

  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    #print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    load(res)
    genDataFiles[[i]] <- allData

    #genDataFiles[[i]] <- get(load(dfData[[i]]))
  }

  res.model.data.dir=file.path(model.data.dir, ratName)
  res.model.data.dir=file.path(res.model.data.dir, "holdoutTest")
  setwd(res.model.data.dir)

  resMatList <- listenv()
  for(i in c(1:20))
  {
    pattern=paste0(ratName,"_holdVal",i,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    resMatList[[i]] <- resList
  }
  resList <- Reduce(rbind,resMatList)
  save(resList, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_Stability_resList.Rdata"))


  df <- as.data.frame(resList)
  cols.num <- c(3,4,5,6,7,8,9,10,11,12)
  df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  #anyNA <- any(!complete.cases(df))
  #print(sprintf("anyNA=%s",anyNA))

  #save(df, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_ParamEs_Stability_df.Rdata"))
  
  resList1<-
  foreach(genDataFile = c(1:10), .packages=c("stringr"), .export=c("model.src"),.errorhandling='pass') %:%
    foreach(genDataNum = c(1:60))  %do%
    {
      df_genData = df[which(df[,11]== genDataFile & df[,12]==genDataNum),]
      genData_minlik = 1000000
      minmodel <- new("ModelData", sim = 1)

      trueModel = paste0(df_genData[1,2],".qlearningAvgRwd")
      genDataList <- genDataFiles[[genDataFile]]
      generatedData = genDataList[[genDataNum]]
      #cat(sprintf('rat=%s, genDataFile=%i, genDataNum = %i, trueModel = %s\n', ratName,genDataFile,genDataNum, generatedData@simModel))
      #cat(models)
      #cat("\n")
     modelDataList <- 
      foreach(model = models,.errorhandling='pass') %do%
      {
        #cat(sprintf("model=%s\n", model))
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        df_genData_model = df_genData[which(df_genData[,1]==model),]
        modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = 1)
        modelData@alpha = as.numeric(df_genData_model[3])
        modelData@gamma1 = as.numeric(df_genData_model[4])
        modelData@gamma2 = as.numeric(df_genData_model[5])
        modelData@lambda = as.numeric(df_genData_model[6])   
        argList <- getArgList(modelData, generatedData)

        lik <- TurnsNew::getTurnsLikelihood(generatedData, modelData, argList[[6]], sim=1)
        holdoutLik <- sum(lik[-c(1:800)])*-1

        #print(sprintf("holdoutLik=%f",holdoutLik))
        if (is.infinite(holdoutLik)) {
          #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
          holdoutLik= 1000000
        }else if (is.nan(holdoutLik)) {
          #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
          holdoutLik = 1000000
        }else if (is.na(holdoutLik)) {
          #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
          holdoutLik = 1000000
        }

        if(holdoutLik < genData_minlik)
        {
          genData_minlik = holdoutLik
          minmodel@Model = modelName
          minmodel@creditAssignment = creditAssignment
          minmodel@alpha = as.numeric(df_genData_model[3])
          minmodel@gamma1 = as.numeric(df_genData_model[4])
          minmodel@gamma2 = as.numeric(df_genData_model[5])
          minmodel@lambda = as.numeric(df_genData_model[6])
        }

        print(sprintf('modelName = %s, holdoutLik=%f, alpha=%.10f, gamma1=%.10f,gamma2=%f, lambda=%f,\n', modelName, holdoutLik, minmodel@alpha, minmodel@gamma1, minmodel@gamma2, minmodel@lambda))
        
        modelData
      }
      print(sprintf('selectedModel = %s, genData_minlik=%f\n', minmodel@Model, genData_minlik))

      #print(sprintf("trueModel=%s,minModel=%s",trueModel,minModel))
      #confusionMatrix[trueModel,minModel] = confusionMatrix[trueModel,minModel]+1  
      minModel = paste(minmodel@Model,minmodel@creditAssignment,sep=".")
      #print(sprintf("minModel=%s",minModel))
      list(trueModel=trueModel,minModel=minModel,genDataFile=genDataFile,genDataNum=genDataNum, modelDataList=modelDataList)
    }
    
  
    
  #minDflist <- unlist(minDfModels, recursive = FALSE)
  #minDfModels <- Reduce(rbind,minDflist)
  #print(resList)
  resList1<-Reduce(rbind,resList1)
  save(resList1, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_HoldoutRes.Rdata"))


  #df <- as.data.frame(resList1)
  confusionMatrix <- matrix(0,length(testData@Models),length(testData@Models))
  colnames(confusionMatrix) <- c(testData@Models)
  rownames(confusionMatrix) <- c(testData@Models)
  
  for(i in c(1:length(resList1)))
  {
    #print(sprintf("i=%i",i))

    trueModel = resList1[[i]]$trueModel
    minModel = resList1[[i]]$minModel
    #print(sprintf("trueModel=%s, minModel=%s", trueModel, minModel))
    confusionMatrix[trueModel,minModel] = confusionMatrix[trueModel,minModel]+1 
  }

  print(confusionMatrix)
  save(confusionMatrix, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_confusionMatrix.Rdata"))

}
                                    
multiHoldoutValidation=function(ratdata,testData, src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name,initpop, testSuite, gen.data.dir)
{
    ## Test settings ###############
  StabilityTest = TRUE 
  DataGenerated = TRUE
  
  ##################################

  testDataName = testData@Name
  models = testData@Models
  ratName = ratdata@rat
  creditAssignment = strsplit(models[1],"\\.")[[1]][2]
  # param.model.data.dir=file.path(model.data.dir, ratName)
  # param.model.data.dir=file.path(param.model.data.dir, "modelParams")

  # #gen.data.dir=file.path(model.data.dir, ratName)
  # #gen.data.dir=file.path(gen.data.dir, "Datasets")
  # allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)
  

  res.model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(res.model.data.dir,creditAssignment), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, creditAssignment)
  print(sprintf("multiHoldoutValidation: creditAssignment=%s, dataset:%s",creditAssignment,gen.data.dir))
   
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  
  print(sprintf("models: %s",toString(models)))
  
  setwd(gen.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  #print(dfData)
  #dfData <- dfData[which(str_detect(dfData,paste0("GenData",genDataList,"_")))]
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    #print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    load(res)
    print(res)
    genDataFiles[[i]] <- allData

    #genDataFiles[[i]] <- get(load(dfData[[i]]))
  }
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed) 
  exportDoMPI(cl, c("model.src","src.dir","model.data.dir","gamma2_Global", "lambda_Global", "genDataFiles"), envir=environment())
  registerDoMPI(cl)
   
  cat(sprintf('Running validation with %d worker(s)\n', getDoParWorkers()))
   
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }
    source(paste(src.dir,"exportFunctions.R", sep="/"))
  
  #modelNum =  length(allData)
  
  #chunkSize = 300
  print(sprintf("gridMat len=%i",length(gridMat[,1])))
  chunkSize = length(gridMat[,1])/getDoParWorkers()
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  resList<-
      foreach(idx = 1:length(gridMat[,1]), .packages=c("DEoptim","stringr"), .options.mpi=opts) %dopar%
      { 
        
        #source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())

        genDataFileNum = as.numeric(gridMat[idx,1])
        genDataNum = as.numeric(gridMat[idx,2])
        testModel = gridMat[idx,3]

        genDataList <- genDataFiles[[genDataFileNum]]
        cat(sprintf("genDataFileNum= %i,genDataNum=%i\n", genDataFileNum,genDataNum))

        if(length(genDataList) < genDataNum)
        {
          print(sprintf("idx=%i,genDataFileNum=%i,genDataNum=%i does not exist ",idx,genDataFileNum,genDataNum))
          #return(c(iter = iter,model=model,NA, NA,NA,NA, NA, NA,NA,NA,genDataFileNum=genDataFileNum,genDataNum=genDataNum))
          return(NULL)
        }else
        {
          generatedData = genDataList[[genDataNum]]
        }

        trueModelData = generatedData@simModelData

         

        modelName = strsplit(testModel,"\\.")[[1]][1]
        creditAssignment = strsplit(testModel,"\\.")[[1]][2]
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
        argList<-getArgList(modelData,generatedData)
        

        #cat(sprintf('rat=%s, model = %s, creditAssignment=%s\n', ratName,modelName,creditAssignment))
            #cat(sprintf('rat=%s, iter=%i,creditAssignment = %s\n', ratName,iter,creditAssignment))


        #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
        #  res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
        #                  fn = negLogLikFunc,ratdata=generatedData,half_index=800,modelData=modelData,testModel = argList[[6]],sim = 1)

        myList <- DEoptim.control(initialpop=initpop, F=0.8, CR = 0.9,trace = FALSE, itermax = 30)
        out <-DEoptim(negLogLikFunc,lower=c(0,0),upper=c(1,1),ratdata=generatedData,half_index=800,modelData=modelData,testModel = argList[[6]],sim = 1,myList)
        
        modelData = setModelParams(modelData, c(out$optim$bestmem,modelData@gamma2,modelData@lambda))
        cat(sprintf('rat=%s, model = %s, creditAssignment=%s\n', ratName,modelName,creditAssignment))
        #modelData = setModelResults(modelData,generatedData,allModels)
        list(model=testModel,trueModel=trueModelData@Model,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda, trueModelData@alpha, trueModelData@gamma1,trueModelData@gamma2,trueModelData@lambda,genDataFileNum=genDataFileNum,genDataNum=genDataNum)
      }
  
  
  resList <- Reduce(rbind,resList)
  rat = ratdata@rat
  save(resList,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_HoldoutResList.Rdata"))
 
  
  if(setup.hpc)
  {
    #stopCluster(cl)
    #stopImplicitCluster()
    closeCluster(cl)
  }
  else
  {
    stopCluster(cl)
    stopImplicitCluster()
  }
}


testParamEstimationV4=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)
{
  ## Test settings ###############
  
  StabilityTest = TRUE
  
  ##################################
  models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  param.model.data.dir=file.path(model.data.dir, ratName)
  param.model.data.dir=file.path(param.model.data.dir, "modelParams")

  allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

  gen.data.dir=file.path(model.data.dir, ratName)
  gen.data.dir=file.path(gen.data.dir, "Datasets")

  res.model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(res.model.data.dir,"paramEstTest"), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, "paramEstTest")

   
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  setwd(gen.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  #dfData <- dfData[which(str_detect(dfData,paste0("GenData",genDataList,"_")))]
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    #print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    load(res)
    genDataFiles[[i]] <- allData

    #genDataFiles[[i]] <- get(load(dfData[[i]]))
  }
 
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed)
    
  exportDoMPI(cl, c("model.src","src.dir","model.data.dir","gamma2_Global", "lambda_Global", "genDataFiles"), envir=environment())
  registerDoMPI(cl)
   
  cat(sprintf('Running validation with %d worker(s)\n', getDoParWorkers()))
   
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }
    source(paste(src.dir,"exportFunctions.R", sep="/"))

  
  chunkSize = ceiling(length(gridMat[,1])/(getDoParWorkers()))
  #chunkSize = 300
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize, profile=FALSE) 

  print(sprintf("gridMat len=%i, getDoParWorkers=%i",length(gridMat[,1]),getDoParWorkers()))
  curr.time <- Sys.time() 
  resList1 <- 
      foreach(idx = 1:length(gridMat[,1]), .packages=c("DEoptim","stringr","tictoc"), .options.mpi=opts) %dopar%
      {
        prev.time <- curr.time
        curr.time <- Sys.time() 
        #time.diff <- curr.time-prev.time
        
        genDataFileNum = as.numeric(gridMat[idx,1])
        genDataNum = as.numeric(gridMat[idx,2])
        iter = gridMat[idx,3]

        cat(sprintf("idx=%i,time=%s, iter=%i, genDataFileNum= %i,genDataNum=%i",idx,difftime(curr.time, prev.time, units="sec"),iter,genDataFileNum,genDataNum))
        cat("\n")
        #start_idx=sequences[i]
        #idx = start_idx+j
        #cat(sprintf("idx= %i,alpha=%.10f,gamma1=%.10f\n", idx,alpha,gamma1))
        #cat(toString(gridMat[idx,]))
        #cat("\n")
        #cat(sprintf("idx= %i,name=%s\n", idx,name))
        
        #cat(sprintf("genDataFileNum= %i,genDataNum=%i\n", genDataFileNum,genDataNum))
        genDataList <- genDataFiles[[genDataFileNum]]
        #cat(sprintf("length(genDataList)= %i,genDataNum=%i\n", length(genDataList),genDataNum))

        if(length(genDataList) < genDataNum)
        {
          print(sprintf("idx=%i,genDataFileNum=%i,genDataNum=%i does not exist ",idx,genDataFileNum,genDataNum))
          #return(c(iter = iter,model=model,NA, NA,NA,NA, NA, NA,NA,NA,genDataFileNum=genDataFileNum,genDataNum=genDataNum))
          return(NULL)
        }else
        {
          generatedData = genDataList[[genDataNum]]
        }

        trueModelData = generatedData@simModelData
        
            #cat(sprintf('Rat is %s, model is %s\n', ratName,model))

        modelName = generatedData@simModel
        creditAssignment = generatedData@simMethod              
        #cat(sprintf('rat=%s, iter=%i,model = %s, creditAssignment=%s\n', ratName,iter,modelName,creditAssignment))
            #cat(sprintf('rat=%s, iter=%i,creditAssignment = %s\n', ratName,iter,creditAssignment))


            #cat(sprintf('rat=%s, iter=%i,model = %s\n', ratName,iter,modelName))
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
        argList<-getArgList(modelData,generatedData)

            #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
        tic()         
        myList <- DEoptim.control(initialpop=initpop, F=0.8, CR = 0.9,trace = FALSE, itermax = 30)
        out <-DEoptim(negLogLikFunc,lower=c(0,0),upper=c(1,1),ratdata=generatedData,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 1,myList)
        s <- toc()
        #cat(s$callback_msg)
        cat("\n")
        #modelData = setModelParams(modelData, c(res$par,0.1,0))
        #modelData@alpha = out$optim$bestmem[1]
        #modelData@gamma1 = out$optim$bestmem[2]
        modelData = setModelParams(modelData, c(out$optim$bestmem,modelData@gamma2,modelData@lambda))


        model = paste0(modelName,".",creditAssignment)
        c(iter = iter,model=model,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda, trueModelData@alpha, trueModelData@gamma1,trueModelData@gamma2,trueModelData@lambda,genDataFileNum=genDataFileNum,genDataNum=genDataNum)
      }

  #resList1 <- unlist(resList1, recursive = FALSE)
  resList1 <- Reduce(rbind,resList1)
  rat = ratdata@rat
  save(resList1,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_ParamEstResList1.Rdata"))

   
      
   
  if(setup.hpc)
  {
    closeCluster(cl)
  }
  
}


combineParamEstResListsV4=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite)
{
  
    ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside combineParamEstResLists"))
  ratName = ratdata@rat
  models = testData@Models
  #param.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
  #allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

  res.model.data.dir=file.path(model.data.dir, ratName)
  res.model.data.dir=file.path(res.model.data.dir, "paramEstTest")
  print(res.model.data.dir) 
  print(model.src)
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  resMatList <- listenv()
  setwd(res.model.data.dir)
  for(i in c(1:20))
  {
    
    pattern=paste0(ratName,"_paramEs",i,"_.*_ParamEstResList1.Rdata")
    resList1=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList1)
    print(any(!complete.cases(resList1)))
    load(resList1)
    resMatList[[i]] <- resList1
  }
  resMat <- Reduce(rbind,resMatList)
  save(resMat, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_Stability_resMat.Rdata"))

   
  gen.data.dir=file.path(model.data.dir, ratName)
  gen.data.dir=file.path(gen.data.dir, "Datasets")
  setwd(gen.data.dir)

  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    #print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    load(res)
    genDataFiles[[i]] <- allData

    #genDataFiles[[i]] <- get(load(dfData[[i]]))
  }

 
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed)
    
  exportDoMPI(cl, c("src.dir","model.data.dir", "testSuite"),envir=environment())
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

    
  iters=c(seq(from = 0, to = length(ratdata@allpaths[,1]), by = 400)[-1],length(ratdata@allpaths[,1]))  
  #chunkSize = ceiling(length(models)*length(iters)/getDoParWorkers())
  #print(sprintf("chunkSize=%i",chunkSize))
  opts <- list(initEnvir=initWorkers, profile=TRUE) 

  df <- as.data.frame(resMat)
  cols.num <- c(1,3,4,5,6,7,8,9,10,11,12)
  df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  anyNA <- any(!complete.cases(df))
  print(sprintf("anyNA=%s",anyNA))

  #save(df, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_ParamEs_Stability_df.Rdata"))

 
  minDflist <- foreach(model = models, .inorder=TRUE, .options.mpi=opts, .packages=c("stringr"), .export=c("model.src"), .combine='rbind') %:% 
    foreach(iter = iters, .inorder=TRUE, .combine='rbind') %dopar%
    {
      #print(sprintf("it=%i,model=%s",iter,model))
      modelName = strsplit(model,"\\.")[[1]][1]
      #cat(sprintf('rat=%s, iter=%i,modelName = %s\n', ratName,iter,modelName))
      creditAssignment = strsplit(model,"\\.")[[1]][2]

      df_it <- df[which(df[,1]==iter & df[,2]==model),]
      #minmodel_genDataFileNum = 0
      #minmodel_genDataNum = 0

      genDataFileNumbers = unique(df_it[,11])
      #print(genDataFileNumbers)
      
      res2 <- 
        foreach(fileNb = genDataFileNumbers, .combine='rbind')%do%
        {
          #print(sprintf("it=%i,fileNb=%i",iter,fileNb))
          df_it_fileNb = df_it[which(df_it[,11]==fileNb),]
          genDataNb =  unique(df_it_fileNb[,12])
          #print(sprintf("len genDataNb=%i",length(genDataNb)))
          res1 <- 
            foreach(i = genDataNb, .combine='rbind') %do%
            {
              print(sprintf("it=%i,fileNb=%i, i=%i",iter,fileNb,i))
              genDataList <- genDataFiles[[fileNb]]
              generatedData = genDataList[[i]]

              #print(sprintf("model=%s, interval=%i, fileNb=%i, datasetNb=%i, genDataSimModel=%s",modelName,iter,fileNb,i,generatedData@simModel))
              df_genData = df_it_fileNb[which(df_it_fileNb[,12]==i),]
              #print(sprintf("len df_genData=%i",length(df_genData[,1])))
              min_lik1 = 1000000
              minmodel <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = 1)

              for(idx in 1:length(df_genData[,1]))
              {
                #print(sprintf("idx=%i",idx))
                modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = 1)
                modelData@alpha = df_genData[idx,3]
                modelData@gamma1 = df_genData[idx,4]
                
                argList <- getArgList(modelData, generatedData)
                lik <- TurnsNew::getTurnsLikelihood(generatedData, modelData, argList[[6]], sim=1)
                lik1 <- sum(lik[c(1:iter)])*-1
                #lik2 <- sum(lik[-c(1:800)])*-1
                #print(sprintf("alpha=%.10f, gamma1=%.10f",modelData@alpha,modelData@gamma1))

                
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
                  minmodel@alpha = df_genData[idx,3]
                  minmodel@gamma1 = df_genData[idx,4]
                  minmodel_genDataFileNum = df_genData[idx,11]
                  minmodel_genDataNum = df_genData[idx,12]

                }    
              }

              ### Compute probrow using modelData=minmodel  ####################

              
              probMat <- TurnsNew::getProbMatrix(generatedData, minmodel, argList[[6]], sim=1)
              #print(minmodel)
              idx = length(df_genData[,1])
              
              trueModelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = 1)
              trueModelData@alpha = df_genData[idx,7]
              trueModelData@gamma1 = df_genData[idx,8]
              #trueModelData@gamma2 = df_genData[idx,9]
              #trueModelData@lambda = df_genData[idx,10]
              #trueModelData <- allData[[minmodel_genDataNum]]@simModelData
              #print(trueModelData)
              trueProbMat <- TurnsNew::getProbMatrix(generatedData, trueModelData, argList[[6]], sim=1)
                    
              probRow <- round((trueProbMat[iter,] - probMat[iter,]),2)/round(trueProbMat[iter,],2) 
              probRow[is.nan(probRow)] <- 0
              probRow[is.infinite(probRow)] <- 0
              print(probRow)
              list(iter = iter, model = modelName, creditAssignment=creditAssignment, genDataFileNum=minmodel_genDataFileNum,genDataNum=minmodel_genDataNum,res_alpha=minmodel@alpha, res_gamma1=minmodel@gamma1, res_gamma2=minmodel@gamma2,res_lambda=minmodel@lambda,probRow=probRow,trueAlpha = trueModelData@alpha,trueGamma1 = trueModelData@gamma1,trueGamma2 = trueModelData@gamma2,trueLambda = trueModelData@lambda)
            }
            res1
        }
        res2
    }
  #minDflist <- unlist(minDfModels, recursive = FALSE)
  #minDfModels <- Reduce(rbind,minDflist)
  save(minDflist, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_minDflist.Rdata"))

  print("Generating Df")
   

   
  df <- data.frame(model=character(),
                  iter=integer(),
                  genDataFileNum=integer(),
                  genDataNum = integer(),
                  alpha=double(),
                  gamma1=double(),
                  gamma2=double(),
                  lambda=double(),
                  trueAlpha=double(),
                  trueGamma1=double(),
                  trueGamma2=double(),
                  trueLambda=double(),
                  stringsAsFactors=FALSE)

  for(k in c(1:length(minDflist[,1])))
  {
    iter = minDflist[k,]$iter
    genDataFileNum = minDflist[k,]$genDataFileNum
    genDataNum = minDflist[k,]$genDataNum
    model = minDflist[k,]$model
    #modelDataRes = minDflist[[k]]$res
    #trueModelData = minDflist[[k]]$trueModelData

    res_alpha = minDflist[k,]$res_alpha
    res_gamma1 = minDflist[k,]$res_gamma1
    res_gamma2 = minDflist[k,]$res_gamma2
    res_lambda = minDflist[k,]$res_lambda

    trueAlpha = minDflist[k,]$trueAlpha
    trueGamma1 = minDflist[k,]$trueGamma1
    trueGamma2 = minDflist[k,]$trueGamma2
    trueLambda= minDflist[k,]$trueLambda
        
    df[k,1] <- model
    df[k,2] <- iter
    df[k,3] <- genDataFileNum
    df[k,4] <- genDataNum
    df[k,5] <- res_alpha
    df[k,6] <- res_gamma1
    df[k,7] <- res_gamma2
    df[k,8] <- res_lambda
    df[k,9] <- trueAlpha
    df[k,10] <- trueGamma1
    df[k,11] <- trueGamma2
    df[k,12] <- trueLambda

  }
  rat = ratdata@rat
  if(StabilityTest)
  {
    save(df, file = paste0(res.model.data.dir, "/" , rat,"_", timestamp,"_ParamEs_Stability_df.Rdata"))
  }
  else
  {
    save(df,  file = paste0(res.model.data.dir, "/" , rat,"_", timestamp,"_ParamEs_Conv_df.Rdata"))
  }
   
}
