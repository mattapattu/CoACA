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

                      
testLikelihoodModelSelection=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite)
{
      ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside combineHoldoutResLists"))
  ratName = ratdata@rat
  models = testData@Models
  #param.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
  #allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

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

  holdoutResLists1 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)

  resMatList <- listenv()
  for(i in c(1:length(holdoutResLists1)))
  {
    pattern=paste0(ratName,"_holdVal",i,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    resMatList[[i]] <- resList
  }
  resList <- Reduce(rbind,resMatList)

  dir.create(file.path(model.data.dir,ratName,"likelihoodModelSelection"), showWarnings = FALSE)
  res.data.dir=file.path(model.data.dir, ratName,"likelihoodModelSelection")

  save(resList, file = paste0(res.data.dir, "/" , ratName,"_",timestamp,"_Stability_resList.Rdata"))


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
      if(length(df_genData[,1]) == 0)
      {
        return(NULL)
      }
      genData_minlik = 1000000
      minmodel <- new("ModelData", sim = 1)
      creditAssignment = strsplit(models[1],"\\.")[[1]][2]
      trueModel = paste0(df_genData[1,2],".",creditAssignment)
      genDataList <- genDataFiles[[genDataFile]]
      generatedData = genDataList[[genDataNum]]
      print(sprintf('rat=%s, genDataFile=%i, genDataNum = %i, trueModel = %s\n', ratName,genDataFile,genDataNum, generatedData@simModel))
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
        if(!is.na(df_genData_model[5]))  modelData@gamma2 = as.numeric(df_genData_model[5])
        if(!is.na(df_genData_model[6]))  modelData@lambda = as.numeric(df_genData_model[6]) 
          
        argList <- getArgList(modelData, generatedData)

        lik <- TurnsNew::getTurnsLikelihood(generatedData, modelData, argList[[6]], sim=1)
        holdoutLik <- sum(lik[c(1:800)])*-1

        print(sprintf("model=%s,holdoutLik=%f",model,holdoutLik))
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
          minmodel@gamma2 = modelData@gamma2
          minmodel@lambda = as.modelData@lambda
        }

        #print(sprintf('modelName = %s, holdoutLik=%f, alpha=%.10f, gamma1=%.10f,gamma2=%f, lambda=%f,\n', modelName, holdoutLik, minmodel@alpha, minmodel@gamma1, minmodel@gamma2, minmodel@lambda))
        
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
  save(resList1, file = paste0(res.data.dir, "/" , ratName,"_", timestamp,"_HoldoutRes.Rdata"))


  #df <- as.data.frame(resList1)
  confusionMatrix <- matrix(0,length(testData@Models),length(testData@Models))
  colnames(confusionMatrix) <- c(testData@Models)
  rownames(confusionMatrix) <- c(testData@Models)
  
  #print(confusionMatrix)
  for(i in c(1:length(resList1)))
  {
    #print(sprintf("i=%i",i))
    if(is.null(resList1[[i]]))
    {
      next
    }
    trueModel = resList1[[i]]$trueModel
    minModel = resList1[[i]]$minModel
    #print(sprintf("trueModel=%s, minModel=%s", trueModel, minModel))
    confusionMatrix[trueModel,minModel] = confusionMatrix[trueModel,minModel]+1 
  }

  print(confusionMatrix)
  save(confusionMatrix, file = paste0(res.data.dir, "/" , ratName,"_", timestamp,"_confusionMatrix.Rdata"))

}


### For DL, add another gen.data.dir as argument                                    
multiLikModelSelectionTest=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite, gen.model.dir)
{
      ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside multiLikModelSelectionTest"))
  ratName = ratdata@rat
  models = testData@Models
  creditAssignment = strsplit(models[1],"\\.")[[1]][2]
  #param.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
  #allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

 
  #print(res.model.data.dir) 
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')
   
  gen.data.dir=file.path(gen.model.dir, "Datasets") 
  print(sprintf("testSuite=%s, gen.data.dir=%s",testSuite,gen.data.dir))
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


  res.model.data.dir=file.path(model.data.dir, ratName, creditAssignment)
  #res.model.data.dir=paste(model.data.dir,creditAssignment,sep="/")
  print(sprintf("res.model.data.dir=%s",res.model.data.dir))

  setwd(res.model.data.dir)
  holdoutResLists1 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)
  resMatList <- listenv()
  for(i in c(1:length(holdoutResLists1)))
  {
    pattern=paste0(ratName,"_multiHold",i,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    resMatList[[i]] <- resList
  }

  gen.resMat.dir = file.path(gen.model.dir, "holdoutTest")
  print(sprintf("gen.resMat.dir=%s",gen.resMat.dir))
  setwd(gen.resMat.dir)
  holdoutResLists2 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)
  for(k in c(1:length(holdoutResLists2)))
  {
    pattern=paste0(ratName,"_holdVal",k,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    i=i+k
    resMatList[[i]] <- resList
  }  

  resList <- Reduce(rbind,resMatList)

  res.data.dir=file.path(model.data.dir, ratName,"likelihoodValidation", creditAssignment)
  print(sprintf("res.data.dir=%s",res.data.dir))
  dir.create(file.path(model.data.dir,ratName,"likelihoodValidation", creditAssignment), showWarnings = TRUE)
  setwd(res.data.dir)

  save(resList, file = paste0(res.data.dir, "/" , ratName,"_",timestamp,"_Stability_resList.Rdata"))


  df <- as.data.frame(resList)
  cols.num <- c(3,4,5,6,7,8,9,10,11,12)
  df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  #anyNA <- any(!complete.cases(df))
  #print(sprintf("anyNA=%s",anyNA))

  #save(df, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_ParamEs_Stability_df.Rdata"))
  all.models <- c(unique(Reduce(rbind,resList[,1])))
  print(all.models)

  resList1<-
  foreach(genDataFile = c(1:10), .packages=c("stringr"), .export=c("model.src"),.errorhandling='pass') %:%
    foreach(genDataNum = c(1:60),.errorhandling='pass')  %do%
    {
      df_genData = df[which(df[,11]== genDataFile & df[,12]==genDataNum),]
      if(length(df_genData[,1]) == 0)
      {
        return(NULL)
      }
      genData_minlik = 1000000
      minmodel <- new("ModelData", sim = 1)
      creditAssignment = strsplit(models[1],"\\.")[[1]][2]
      
      genDataList <- genDataFiles[[genDataFile]]
      generatedData = genDataList[[genDataNum]]
      trueModel = generatedData@simModel
      trueCreditAssignment = generatedData@simMethod
      trueModel = paste0(trueModel,".",trueCreditAssignment)
      cat(sprintf('rat=%s, genDataFile=%i, genDataNum = %i, trueModel = %s\n', ratName,genDataFile,genDataNum, trueModel))
      #cat(models)
      #cat("\n")
     modelDataList <- 
      foreach(model = all.models,.errorhandling='pass') %do%
      {
        cat(sprintf("model=%s\n", model))
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        df_genData_model = df_genData[which(df_genData[,1]==model),]
        modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = 1)
        modelData@alpha = as.numeric(df_genData_model[3])
        modelData@gamma1 = as.numeric(df_genData_model[4])
        if(!is.na(df_genData_model[5]))  modelData@gamma2 = as.numeric(df_genData_model[5])
        if(!is.na(df_genData_model[6]))  modelData@lambda = as.numeric(df_genData_model[6]) 
          
        argList <- getArgList(modelData, generatedData)

        lik <- TurnsNew::getTurnsLikelihood(generatedData, modelData, argList[[6]], sim=1)
        holdoutLik <- sum(lik[c(1:800)])*-1

        cat(sprintf("holdoutLik=%f\n",holdoutLik))
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
          minmodel@gamma2 = modelData@gamma2
          minmodel@lambda = as.modelData@lambda
        }

        cat(sprintf('modelName = %s, holdoutLik=%f, alpha=%.10f, gamma1=%.10f,gamma2=%f, lambda=%f,\n', modelName, holdoutLik, minmodel@alpha, minmodel@gamma1, minmodel@gamma2, minmodel@lambda))
        
        modelData
      }
      cat(sprintf('selectedModel = %s, genData_minlik=%f\n', minmodel@Model, genData_minlik))

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
  save(resList1, file = paste0(res.data.dir, "/" , ratName,"_", timestamp,"_HoldoutRes.Rdata"))


  #df <- as.data.frame(resList1)
  
  trueModels <- all.models[!all.models %in% testData@Models]
  confusionMatrix <- matrix(0,length(trueModels),length(all.models))
  colnames(confusionMatrix) <- c(all.models)
  rownames(confusionMatrix) <- c(trueModels)
  
  #print(confusionMatrix)
  for(i in c(1:length(resList1)))
  {
    #print(sprintf("i=%i",i))
    if(is.null(resList1[[i]]))
    {
      next
    }
    trueModel = resList1[[i]]$trueModel
    minModel = resList1[[i]]$minModel
    cat(sprintf("trueModel=%s, minModel=%s\n", trueModel, minModel))
    confusionMatrix[trueModel,minModel] = confusionMatrix[trueModel,minModel]+1 
  }

  print(confusionMatrix)
  save(confusionMatrix, file = paste0(res.data.dir, "/" , ratName,"_", timestamp,"_confusionMatrix.Rdata"))

}