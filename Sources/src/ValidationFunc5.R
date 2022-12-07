library(doMPI)
library(rlist)
library(nloptr)
library(stringr)

                      
### For DRL, add another gen.model.dir as argument                                    
multiHoldoutValidation=function(ratdata,testData, src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name,initpop, testSuite, gen.model.dir, currentTest)
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
  dir.create(file.path(res.model.data.dir,currentTest), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, currentTest)
   
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  
  print(sprintf("models: %s",toString(models)))
  gen.data.dir = file.path(gen.model.dir, "Datasets")
  print(sprintf("multiHoldoutValidation: creditAssignment=%s, dataset:%s",creditAssignment,gen.data.dir))

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


### For DL, add another gen.data.dir as argument                                    
combinemultiHoldoutValidation=function(ratdata,data.dir,model.data.dir,count, gen.model.dir,currentTest)
{
      ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside combinemultiHoldoutValidation"))
  ratName = ratdata@rat
  #models = testData@Models
  #param.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
  #allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

 
  #print(res.model.data.dir) 
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  res.model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(res.model.data.dir,currentTest), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, currentTest)
  print(sprintf("res.model.data.dir=%s",res.model.data.dir))
   
  gen.data.dir=file.path(gen.model.dir, "Datasets") 
  print(sprintf("gen.data.dir=%s",gen.data.dir))
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

  if(currentTest == "holdoutValidation_on_arl")
  {
    model.data.dir1=file.path(data.dir, "ARLTestSuite",ratName,"holdoutTest")
    model.data.dir2=file.path(data.dir, "ARLDRLCoACAR5",ratName,"drl_on_arl")
    model.data.dir3=file.path(data.dir, "ARLDRLCoACAR5",ratName,"coaca_on_arl")
  }else if(currentTest == "holdoutValidation_on_coaca")
  {
    model.data.dir1=file.path(data.dir, "CoACAR5",ratName,"holdoutTest")
    model.data.dir2=file.path(data.dir, "ARLDRLCoACAR5",ratName,"arl_on_coaca")
    model.data.dir3=file.path(data.dir, "ARLDRLCoACAR5",ratName,"drl_on_coaca")
  }else if(currentTest == "holdoutValidation_on_drl")
  {
    model.data.dir1=file.path(data.dir, "DRLTestSuite",ratName,"holdoutTest")
    model.data.dir2=file.path(data.dir, "ARLDRLCoACAR5",ratName,"arl_on_drl")
    model.data.dir3=file.path(data.dir, "ARLDRLCoACAR5",ratName,"coaca_on_drl")
  }


  resMatList <- listenv()


  print(sprintf("model.data.dir1=%s",model.data.dir1))
  setwd(model.data.dir1)
  holdoutResLists1 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)

  for(i in c(1:length(holdoutResLists1)))
  {
    pattern=paste0(ratName,"_holdVal",i,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    idx = i
    resMatList[[idx]] <- resList
  }

  print(sprintf("model.data.dir2=%s",model.data.dir2))
  setwd(model.data.dir2)
  holdoutResLists2 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)
  for(k in c(1:length(holdoutResLists2)))
  {
    pattern=paste0(ratName,"_multiHold",k,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    idx = i+k
    #print(idx)
    resMatList[[idx]] <- resList
  }

  print(sprintf("model.data.dir3=%s",model.data.dir3))
  setwd(model.data.dir3)
  holdoutResLists3 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)
  for(j in c(1:length(holdoutResLists3)))
  {
    pattern=paste0(ratName,"_multiHold",j,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    idx = i+k+j
    resMatList[[idx]] <- resList
  }

  
  resList <- Reduce(rbind,resMatList)
  save(resList, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_Stability_resList.Rdata"))


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
      #creditAssignment = strsplit(models[1],"\\.")[[1]][2]
      
      genDataList <- genDataFiles[[genDataFile]]
      generatedData = genDataList[[genDataNum]]
      trueModel = generatedData@simModel
      trueCreditAssignment = generatedData@simMethod
      trueModel = paste0(trueModel,".",trueCreditAssignment)
      #cat(sprintf('rat=%s, genDataFile=%i, genDataNum = %i, trueModel = %s\n', ratName,genDataFile,genDataNum, trueModel))
      #cat(models)
      #cat("\n")
     modelDataList <- 
      foreach(model = all.models,.errorhandling='pass') %do%
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
        holdoutLik <- sum(lik[-c(1:800)])*-1

        #cat(sprintf("holdoutLik=%f\n",holdoutLik))
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

        #cat(sprintf('modelName = %s, holdoutLik=%f, alpha=%.10f, gamma1=%.10f,gamma2=%f, lambda=%f,\n', modelName, holdoutLik, minmodel@alpha, minmodel@gamma1, minmodel@gamma2, minmodel@lambda))
        
        modelData
      }
      #cat(sprintf('selectedModel = %s, genData_minlik=%f\n', minmodel@Model, genData_minlik))

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

  if(currentTest == "holdoutValidation_on_arl")
  {
    gen.models = c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd")

  }else if(currentTest == "holdoutValidation_on_coaca")
  {
    gen.models = c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2")

  }else if(currentTest == "holdoutValidation_on_drl")
  {
    gen.models = c("Paths.qlearningDisRwd","Hybrid1.qlearningDisRwd","Hybrid2.qlearningDisRwd","Hybrid3.qlearningDisRwd","Hybrid4.qlearningDisRwd","Turns.qlearningDisRwd")

  }
  
  #trueModels <- all.models[!all.models %in% testData@Models]
  confusionMatrix <- matrix(0,length(gen.models),length(all.models))
  colnames(confusionMatrix) <- c(all.models)
  rownames(confusionMatrix) <- c(gen.models)
  
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
  save(confusionMatrix, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_confusionMatrix.Rdata"))

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
  dir.create(file.path(model.data.dir,ratName,"likelihoodValidation"), showWarnings = TRUE)
  dir.create(file.path(model.data.dir,ratName,"likelihoodValidation",creditAssignment), showWarnings = TRUE)
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


###########################################
#########################################


# Validation of ikelihood model selection

######################################################################################

likModelSelectionTest2=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)
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
  dir.create(file.path(res.model.data.dir,"likModelSelectionTest2"), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, "likModelSelectionTest2")

  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  
  #print(sprintf("models: %s",toString(models)))
  print(gen.data.dir)
  setwd(gen.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  #print(dfData)
  #dfData <- dfData[which(str_detect(dfData,paste0("GenData",genDataList,"_")))]
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    print(res)
    load(res)
    
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
        allpaths.len = length(ratdata@allpaths[,1])
        out <-DEoptim(negLogLikFunc,lower=c(0,0),upper=c(1,1),ratdata=generatedData,half_index=allpaths.len,modelData=modelData,testModel = argList[[6]],sim = 1,myList)
        
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

getConfMatLikModelSelTest2=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite)
{
      ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside getConfMatLikModelSelTest2"))
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
  res.model.data.dir=file.path(res.model.data.dir, "likModelSelectionTest2")
  print(res.model.data.dir) 
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
        holdoutLik <- sum(lik)*-1

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
  save(resList1, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_HoldoutRes.Rdata"))


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
    print(sprintf("trueModel=%s, minModel=%s", trueModel, minModel))
    confusionMatrix[trueModel,minModel] = confusionMatrix[trueModel,minModel]+1 
  }

  print(confusionMatrix)
  save(confusionMatrix, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_confusionMatrix.Rdata"))

}

### For DRL, add another gen.model.dir as argument                                    
multiLikModelSelectionTest2=function(ratdata,testData, src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name,initpop, testSuite, gen.model.dir, currentTest)
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
  dir.create(file.path(res.model.data.dir,currentTest), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, currentTest)
   
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  
  print(sprintf("models: %s",toString(models)))
  gen.data.dir = file.path(gen.model.dir, "Datasets")
  print(sprintf("multiLikModelSelectionTest2: creditAssignment=%s, dataset:%s",creditAssignment,gen.data.dir))

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
        allpaths.len = length(ratdata@allpaths[,1])
        out <-DEoptim(negLogLikFunc,lower=c(0,0),upper=c(1,1),ratdata=generatedData,half_index=allpaths.len,modelData=modelData,testModel = argList[[6]],sim = 1,myList)
        
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


### For DL, add another gen.data.dir as argument                                    
getMultiConfMatLikModelSelTest2=function(ratdata,data.dir,model.data.dir,count, gen.model.dir,currentTest)
{
      ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ####################################

  print(sprintf("Inside getMultiConfMatLikModelSelTest2"))
  ratName = ratdata@rat
  #models = testData@Models
  #param.model.data.dir=paste(model.data.dir,"paramEstTest",ratName,sep="/")
  #allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)

 
  #print(res.model.data.dir) 
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  timestamp = format(Sys.time(),'_%Y%m%d_%H%M%S')

  res.model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(res.model.data.dir,currentTest), showWarnings = FALSE)
  res.model.data.dir=file.path(res.model.data.dir, currentTest)
  print(sprintf("res.model.data.dir=%s",res.model.data.dir))
   
  gen.data.dir=file.path(gen.model.dir, "Datasets") 
  print(sprintf("gen.data.dir=%s",gen.data.dir))
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

  if(currentTest == "likValidation_on_arl")
  {
    model.data.dir1=file.path(data.dir, "ARLTestSuite",ratName,"likModelSelectionTest2")
    model.data.dir2=file.path(data.dir, "ARLDRLCoACAR5V2",ratName,"drl_on_arlV2")
    model.data.dir3=file.path(data.dir, "ARLDRLCoACAR5V2",ratName,"coaca_on_arlV2")
  }else if(currentTest == "likValidation_on_coaca")
  {
    model.data.dir1=file.path(data.dir, "CoACAR5",ratName,"likModelSelectionTest2")
    model.data.dir2=file.path(data.dir, "ARLDRLCoACAR5V2",ratName,"arl_on_coacaV2")
    model.data.dir3=file.path(data.dir, "ARLDRLCoACAR5V2",ratName,"drl_on_coacaV2")
  }else if(currentTest == "likValidation_on_drl")
  {
    model.data.dir1=file.path(data.dir, "DRLTestSuite",ratName,"likModelSelectionTest2")
    model.data.dir2=file.path(data.dir, "ARLDRLCoACAR5V2",ratName,"arl_on_drlV2")
    model.data.dir3=file.path(data.dir, "ARLDRLCoACAR5V2",ratName,"coaca_on_drlV2")
  }


  resMatList <- listenv()


  print(sprintf("model.data.dir1=%s",model.data.dir1))
  setwd(model.data.dir1)
  holdoutResLists1 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)

  for(i in c(1:length(holdoutResLists1)))
  {
    pattern=paste0(ratName,"_holdVal",i,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    idx = i
    resMatList[[idx]] <- resList
  }

  print(sprintf("model.data.dir2=%s",model.data.dir2))
  setwd(model.data.dir2)
  holdoutResLists2 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)
  for(k in c(1:length(holdoutResLists2)))
  {
    pattern=paste0(ratName,"_multiLik",k,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    idx = i+k
    #print(idx)
    resMatList[[idx]] <- resList
  }

  print(sprintf("model.data.dir3=%s",model.data.dir3))
  setwd(model.data.dir3)
  holdoutResLists3 <- list.files(".", pattern=paste0(ratName,".*HoldoutResList.Rdata"), full.names=FALSE)
  for(j in c(1:length(holdoutResLists3)))
  {
    pattern=paste0(ratName,"_multiLik",j,"_.*_HoldoutResList.Rdata")
    resList=list.files(".", pattern=pattern, full.names=FALSE)
    print(resList)
    #print(any(!complete.cases(resList)))
    load(resList)
    idx = i+k+j
    resMatList[[idx]] <- resList
  }

  
  resList <- Reduce(rbind,resMatList)
  save(resList, file = paste0(res.model.data.dir, "/" , ratName,"_",timestamp,"_Stability_resList.Rdata"))


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
      #creditAssignment = strsplit(models[1],"\\.")[[1]][2]
      
      genDataList <- genDataFiles[[genDataFile]]
      generatedData = genDataList[[genDataNum]]
      trueModel = generatedData@simModel
      trueCreditAssignment = generatedData@simMethod
      trueModel = paste0(trueModel,".",trueCreditAssignment)
      #cat(sprintf('rat=%s, genDataFile=%i, genDataNum = %i, trueModel = %s\n', ratName,genDataFile,genDataNum, trueModel))
      #cat(models)
      #cat("\n")
     modelDataList <- 
      foreach(model = all.models,.errorhandling='pass') %do%
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
        holdoutLik <- sum(lik)*-1

        #cat(sprintf("holdoutLik=%f\n",holdoutLik))
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

        #cat(sprintf('modelName = %s, holdoutLik=%f, alpha=%.10f, gamma1=%.10f,gamma2=%f, lambda=%f,\n', modelName, holdoutLik, minmodel@alpha, minmodel@gamma1, minmodel@gamma2, minmodel@lambda))
        
        modelData
      }
      #cat(sprintf('selectedModel = %s, genData_minlik=%f\n', minmodel@Model, genData_minlik))

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

  if(currentTest == "likValidation_on_arl")
  {
    gen.models = c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd")

  }else if(currentTest == "likValidation_on_coaca")
  {
    gen.models = c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2")

  }else if(currentTest == "likValidation_on_drl")
  {
    gen.models = c("Paths.qlearningDisRwd","Hybrid1.qlearningDisRwd","Hybrid2.qlearningDisRwd","Hybrid3.qlearningDisRwd","Hybrid4.qlearningDisRwd","Turns.qlearningDisRwd")

  }
  
  #trueModels <- all.models[!all.models %in% testData@Models]
  confusionMatrix <- matrix(0,length(gen.models),length(all.models))
  colnames(confusionMatrix) <- c(all.models)
  rownames(confusionMatrix) <- c(gen.models)
  
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
  save(confusionMatrix, file = paste0(res.model.data.dir, "/" , ratName,"_", timestamp,"_confusionMatrix.Rdata"))

}

############################################
#
#   END likV2
###############################################################




getGenDataStats=function(ratdata,model.data.dir,testSuite)
{

  ratName = ratdata@rat
  gen.data.dir=file.path(model.data.dir, ratName)
  gen.data.dir=file.path(gen.data.dir, "Datasets")

  print(gen.data.dir)
  setwd(gen.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  #print(dfData)
  #dfData <- dfData[which(str_detect(dfData,paste0("GenData",genDataList,"_")))]
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    pattern=paste0(ratName,"_GenData",i,"_.*Rdata")
    print(pattern)
    res=list.files(".", pattern=pattern, full.names=FALSE)
    print(res)
    load(res)
    
    genDataFiles[[i]] <- allData

    #genDataFiles[[i]] <- get(load(dfData[[i]]))
  }
 
  PathCounterMatPostLearning <- matrix(0,600,7)
  PathCounterMat <- matrix(0,600,8)
  index = 0 
  for(genDataFile in c(1:10))
  {
   for(genDataNum in c(1:60)) 
   {
    genDataList <- genDataFiles[[genDataFile]]
    if(genDataNum <= length(genDataList))
    {
      generatedData = genDataList[[genDataNum]]
      endIdx = getEndIndex(ratName,generatedData@allpaths,sim=1,limit=0.8)
      
      allpathsLearning <- generatedData@allpaths[1:endIdx,]
      path1count = length(which(allpathsLearning[,1] == 0))
      path2count = length(which(allpathsLearning[,1] == 1))
      path3count = length(which(allpathsLearning[,1] == 2))
      path4count = length(which(allpathsLearning[,1] == 3))
      path5count = length(which(allpathsLearning[,1] == 4))
      path6count = length(which(allpathsLearning[,1] == 5))

      PathCounterLearning = c(path1count,path2count,path3count,path4count,path5count,path6count)    
      PathCounterLearning = PathCounterLearning/endIdx
      model = paste0(generatedData@simModel,generatedData@simMethod)
      
      PathCounterLearning = c(PathCounterLearning,model,endIdx)


      allpaths <- generatedData@allpaths[-c(1:endIdx),]
      path1count = length(which(allpaths[,1] == 0))
      path2count = length(which(allpaths[,1] == 1))
      path3count = length(which(allpaths[,1] == 2))
      path4count = length(which(allpaths[,1] == 3))
      path5count = length(which(allpaths[,1] == 4))
      path6count = length(which(allpaths[,1] == 5))

      PathCounterPostLearning = c(path1count,path2count,path3count,path4count,path5count,path6count)  
      len = length(generatedData@allpaths[,1])-endIdx
      PathCounterPostLearning = PathCounterPostLearning/len
      PathCounterPostLearning = c(PathCounterPostLearning,model)

      index = index + 1
      PathCounterMat[index,] = PathCounterLearning
      PathCounterMatPostLearning[index,] = PathCounterPostLearning
    }
   }
  }



  res.data.dir=file.path(model.data.dir, ratName,"PathStats")
  print(sprintf("res.data.dir=%s",res.data.dir))
  dir.create(file.path(model.data.dir,ratName,"PathStats"), showWarnings = TRUE)
     
  save(PathCounterMat, file = paste0(res.data.dir, "/" , ratName,"_",testSuite, "_PathCounterMatLearning.Rdata"))
  save(PathCounterMatPostLearning, file = paste0(res.data.dir, "/" , ratName,"_",testSuite, "_PathCounterMatPostLearning.Rdata"))


}

########################################################3

getRealDataStats=function(ratdata,data.dir,testSuite)
{

  ratName = ratdata@rat
  endIdx = getEndIndex(ratName,ratdata@allpaths,sim=2,limit=0.85)
  probMat<-matrix(0,0,15)


  for(crAssgn in c("aca2","qlearningAvgRwd","qlearningDisRwd")) 
  {
    if(crAssgn=="aca2")
    {
      testSuite = "CoACAR5"
      testModels = c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2")      
    }else if(crAssgn == "qlearningAvgRwd")
    {
      testSuite = "ARLTestSuite"
      testModels = c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd")
    }else if(crAssgn == "qlearningDisRwd"){
      
      testSuite = "DRLTestSuite"
      testModels = c("Paths.qlearningDisRwd","Hybrid1.qlearningDisRwd","Hybrid2.qlearningDisRwd","Hybrid3.qlearningDisRwd","Hybrid4.qlearningDisRwd","Turns.qlearningDisRwd")
    }
    
    testData = new("TestModels", Name = testSuite,Models=testModels)
    param.model.data.dir=file.path(data.dir, testSuite,ratName,"modelParams")
    allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)
   for(modelName in c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"))
   {
    trueModelData = slot(slot(allModelRes,modelName),crAssgn)
    argList <- getArgList(trueModelData, ratdata)
    probMat_model <- TurnsNew::getProbMatrix(ratdata, trueModelData, argList[[6]], sim=2)
    model = paste0(modelName,".",crAssgn)
    
    probMat_model <- cbind(probMat_model,rep(model,length(probMat_model[,1])),rep(crAssgn,length(probMat_model[,1])))
    #print(sprintf("probMat cols=%i, probMat_model cols=%i", ncol(probMat), ncol(probMat_model)))
    probMat<- rbind(probMat,probMat_model)
    
   }
   
  }

  res.data.dir=file.path(model.data.dir, ratName,"PathStats")
  dir.create(file.path(model.data.dir,ratName,"PathStats"))

  
  save(probMat, file = paste0(res.data.dir, "/" , ratName,"_","_probMat.Rdata"))

  probMat_df <- as.data.frame(probMat)
  colnames(probMat_df) <- c("Path1.LF","Path2.LF","Path3.LF","Path4.LF","Path5.LF","Path6.LF","Path1.RF","Path2.RF","Path3.RF","Path4.RF","Path5.RF","Path6.RF","Idx","Model","CrAssgn")
  
  probMat_df[,14] = gsub(".qlearningDisRwd",".DRL",probMat_df[,14])
  probMat_df[,14] = gsub(".qlearningAvgRwd",".ARL",probMat_df[,14])
  
  cols.num <- c(1:13)
  probMat_df[,cols.num] <- lapply(cols.num,function(x) as.numeric(probMat_df[[x]]))  
  
  probMat_df_aca2<-probMat_df[which(probMat_df[,15]=="aca2"),]
  probMat_df.melt.aca2 <- melt(probMat_df_aca2[which(probMat_df_aca2[,13] <= endIdx),c(1:12,14,15)],id.vars = c("Model","CrAssgn"))
  p1<-ggplot(probMat_df.melt.aca2) +  geom_boxplot(aes(x=variable, y=value, fill=Model))
  probMat_df_arl<-probMat_df[which(probMat_df[,15]=="qlearningAvgRwd"),]
  probMat_df.melt.arl <- melt(probMat_df_arl[which(probMat_df_arl[,13] <= endIdx),c(1:12,14,15)],id.vars = c("Model","CrAssgn"))
  p2<-ggplot(probMat_df.melt.arl) +  geom_boxplot(aes(x=variable, y=value, fill=Model))
  probMat_df_drl<-probMat_df[which(probMat_df[,15]=="qlearningDisRwd"),]
  probMat_df.melt.drl <- melt(probMat_df_drl[which(probMat_df_drl[,13] <= endIdx),c(1:12,14,15)],id.vars = c("Model","CrAssgn"))
  p3<-ggplot(probMat_df.melt.drl) +  geom_boxplot(aes(x=variable, y=value, fill=Model))
  p4<-grid.arrange(p1,p2,p3,nrow=3)


  #print(file)
  ggsave("boxplotLearningProbs.pdf", p4, path = res.data.dir)

  probMat_df_aca2<-probMat_df[which(probMat_df[,15]=="aca2"),]
  probMat_df.melt.aca2 <- melt(probMat_df_aca2[which(probMat_df_aca2[,13] > endIdx),c(1:12,14,15)],id.vars = c("Model","CrAssgn"))
  p1<-ggplot(probMat_df.melt.aca2) +  geom_boxplot(aes(x=variable, y=value, fill=Model))
  probMat_df_arl<-probMat_df[which(probMat_df[,15]=="qlearningAvgRwd"),]
  probMat_df.melt.arl <- melt(probMat_df_arl[which(probMat_df_arl[,13] > endIdx),c(1:12,14,15)],id.vars = c("Model","CrAssgn"))
  p2<-ggplot(probMat_df.melt.arl) +  geom_boxplot(aes(x=variable, y=value, fill=Model))
  probMat_df_drl<-probMat_df[which(probMat_df[,15]=="qlearningDisRwd"),]
  probMat_df.melt.drl <- melt(probMat_df_drl[which(probMat_df_drl[,13] > endIdx),c(1:12,14,15)],id.vars = c("Model","CrAssgn"))
  p3<-ggplot(probMat_df.melt.drl) +  geom_boxplot(aes(x=variable, y=value, fill=Model))
  p5<-grid.arrange(p1,p2,p3,nrow=3)


  #print(file)
  ggsave("boxplotPostLearningProbs.pdf", p5, path = res.data.dir)
   
  #save(PathCounterMat, file = paste0(res.data.dir, "/" , ratName,"_",testSuite, "_PathCounterMatLearning.Rdata"))
  #save(PathCounterMatPostLearning, file = paste0(res.data.dir, "/" , ratName,"_",testSuite, "_PathCounterMatPostLearning.Rdata"))


}