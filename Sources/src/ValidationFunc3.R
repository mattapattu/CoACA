library(doMPI)
library(rlist)
library(nloptr)
library(stringr)

GenerateData=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count, gridMat, name)
{
  ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ##################################
  models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  param.model.data.dir=paste(model.data.dir,"modelParams",ratName,sep="/")
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
   
   chunkSize = 2
   #chunkSize = 150
   opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 
 
   source(paste(src.dir,"../exportFunctions.R", sep="/")) 
   
   generatedDataList <-  
   foreach(i=1:length(gridMat[,1]), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %dopar%
   {
        
      model = gridMat[i,1] 
      index = as.integer(gridMat[i,2])
      cat(sprintf('model = %s, index = %i, \n', model,index))

      modelName = strsplit(model,"\\.")[[1]][1]
      creditAssignment = strsplit(model,"\\.")[[1]][2]
      trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
       
        #trueModelData = modifyModelData(trueModelData) 
      simLearns = FALSE 
      missedOptimalIter = 0
        
      while(!simLearns)
      {
        if(StabilityTest)
        {
         trueModelData_mod = modifyModelData(trueModelData)
         generatedData = simulateData(trueModelData_mod,ratdata,allModels)
        }else{
         generatedData = simulateData(trueModelData,ratdata,allModels)
        }
          
          #end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.95)
        simLearns = checkSimLearns(generatedData@allpaths,sim=1,limit=0.8) 
        missedOptimalIter=missedOptimalIter+1
          
        if(missedOptimalIter>500)
        {
          cat(sprintf('model = %s, missedOptimalIter = %i, trueAlpha = %f, trueGamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1))
          break
        }
          #cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %.10f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
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


HoldoutTestV2=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count, gridMat, name)
{
  
  ## Test settings ###############
  StabilityTest = TRUE 
  DataGenerated = TRUE
  
  ##################################

  testDataName = testData@Name
  models = testData@Models
  ratName = ratdata@rat
  param.model.data.dir=paste(model.data.dir,"modelParams",ratName,sep="/")
  allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)
  
  res.model.data.dir=paste(model.data.dir,"holdoutTest",ratName,sep="/")

  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  
  print(sprintf("models: %s",toString(models)))
  
  
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=seed) 
  exportDoMPI(cl, c("src.dir","model.data.dir"),envir=environment())
  registerDoMPI(cl)
   
  cat(sprintf('Running validation with %d worker(s)\n', getDoParWorkers()))
   
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
    
   chunkSize = length(gridMat[,1])/getDoParWorkers()
   opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 
 

    
  nefTaskIndex = as.numeric(gsub("GenData(\\d+).*",'\\1',name))
  setwd(res.model.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  dfData <- dfData[which(str_detect(dfData,paste0("GenData",nefTaskIndex,"_")))]
  load(dfData)

  modelNum =  length(allData)
  
  #chunkSize = 150
  chunkSize = length(gridMat[,1])/getDoParWorkers()
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  
  resList<-
    foreach(j = 1:modelNum, .packages="nloptr", .options.mpi=opts) %:% 
      foreach(idx = 1:length(gridMat[,1]), .inorder=TRUE) %dopar%
      { 
        alpha = gridMat[idx,1]
        gamma1 = gridMat[idx,2]
        model = gridMat[idx,3]
        generatedData = allData[[j]]

        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
        argList<-getArgList(modelData,generatedData)


        cat(sprintf("idx= %i,alpha=%.10f,gamma1=%.10f\n", idx,alpha,gamma1))
            #cat(sprintf('Rat is %s, model is %s\n', ratName,model))

        cat(sprintf('rat=%s, iter=%i,model = %s, creditAssignment=%s\n', ratName,iter,modelName,creditAssignment))
            #cat(sprintf('rat=%s, iter=%i,creditAssignment = %s\n', ratName,iter,creditAssignment))


        #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
         res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
                         fn = negLogLikFunc,ratdata=generatedData,half_index=800,modelData=modelData,testModel = argList[[6]],sim = 1)
            modelData = setModelParams(modelData, c(res$par,0.1,0))

        
        modelData = setModelResults(modelData,generatedData,allModels)
        list(data=generatedData,res=modelData)
      }
  
  
  print(time2)

  resList
  rat = ratdata@rat
  save(resList,  file = paste0(res.model.data.dir,"/",rat,"_",name, format(Sys.time(),'_%Y%m%d_%H%M%S_'),testDataName,"_resList.Rdata"))
# setwd(res.model.data.dir)
# print(res.model.data.dir)
# rat=ratdata@rat
#  resData=list.files(".", pattern = paste0(rat, ".*Mix_resList.Rdata"))
#  print(resData)
#  load(resData)
#  modelNum=length(resList)
  
  for(i in 1:modelNum)
  {
    min_method = ""
    min_score = 100000
    gen_model = resList[[i]][[1]]$data@simModel
    gen_method = resList[[i]][[1]]$data@simMethod
    gen_modelname = paste(gen_model, gen_method, sep=".")
    for(m in 1:length(models))
    {
      res_model = resList[[i]][[m]]$res@Model
      res_method = resList[[i]][[m]]$res@creditAssignment
      likelihood = resList[[i]][[m]]$res@likelihood
      res_modelname = paste(res_model, res_method, sep=".")
      model_score = sum(likelihood[-(1:800)]) * -1
      print(sprintf("i=%i,m=%s,res_modelname=%s,model_score=%f,gen_modelname=%s",i,m,res_modelname,model_score,gen_modelname))
  
      if(model_score < min_score)
      {
        min_method = res_modelname
        min_score = model_score 
      }
      
    }
    print(sprintf("gen_modelname=%s, min_method=%s, min_score=%f",gen_modelname,min_method, min_score))
    mat_res[gen_modelname,min_method] = mat_res[gen_modelname,min_method]+1
  }
  
  rat = ratdata@rat
  save(mat_res, generatedDataList,resList,  file = paste0(res.model.data.dir, "/" , rat,"_",name, format(Sys.time(),'_%Y%m%d_%H%M%S_'),testDataName,"_mat_res.Rdata"))
  
  
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



testParamEstimationV2=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count,gridMat,name)
{
  ## Test settings ###############
  
  StabilityTest = TRUE 
  
  ##################################
  models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  param.model.data.dir=paste(model.data.dir,"modelParams",ratName,sep="/")
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
  

  setwd(res.model.data.dir)
  dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
  dfData <- dfData[which(str_detect(dfData,paste0("GenData",genDataList,"_")))]
  genDataFiles <- list()
  for(i in 1:length(dfData))
  {
    genDataFiles[[i]] <- get(load(dfData[[i]]))
  }

  #chunkSize = length(gridMat[,1])/getDoParWorkers()
  chunkSize = 1000
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  print(sprintf("gridMat len=%i, getDoParWorkers=%i",length(gridMat[,1]),getDoParWorkers()))
   
  resList1 <- 
      foreach(idx = 1:length(gridMat[,1]), .packages=c("nloptr","stringr"), .options.mpi=opts) %dopar%
      {
        #start_idx=sequences[i]
        #idx = start_idx+j
        alpha = gridMat[idx,1]
        gamma1 = gridMat[idx,2]
        iter = gridMat[idx,3]
        genDataFileNum = as.numeric(gridMat[idx,4])
        genDataNum = as.numeric(gridMat[idx,5])

        genDataList <- genDataFiles[[genDataFileNum]]
        if(length(genDataList) < genDataNum)
        {
          return(NULL)
        }else
        {
          generatedData = genDataList[[genDataNum]]
        }
        
        cat(sprintf("idx= %i,alpha=%.10f,gamma1=%.10f\n", idx,alpha,gamma1))
            #cat(sprintf('Rat is %s, model is %s\n', ratName,model))

        modelName = generatedData@simModel
        creditAssignment = generatedData@simMethod              
        cat(sprintf('rat=%s, iter=%i,model = %s, creditAssignment=%s\n', ratName,iter,modelName,creditAssignment))
            #cat(sprintf('rat=%s, iter=%i,creditAssignment = %s\n', ratName,iter,creditAssignment))


            #cat(sprintf('rat=%s, iter=%i,model = %s\n', ratName,iter,modelName))
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
        argList<-getArgList(modelData,generatedData)

            #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
        res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
                  fn = negLogLikFunc,ratdata=generatedData,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 1)
        modelData = setModelParams(modelData, c(res$par,0.1,0))
        model = paste0(modelName,".",creditAssignment)
        c(iter = iter,model=model,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda, genDataList=genDataList,genDataNum=genDataNum)
      }

  resList1 <- unlist(resList1, recursive = FALSE)
  resList1 <- Reduce(rbind,resList1)
  rat = ratdata@rat
  save(resList1,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_ParamEstResList1.Rdata"))

   
  chunkSize = ceiling(length(models)*iters/getDoParWorkers())
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  df <- as.data.frame(resList1)
  cols.num <- c(1,3,4,5,6,7,8)
  df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  iters = unique(as.numeric(resList1[,1]))
 
  minDflist <- foreach(model = models, .inorder=TRUE, .options.mpi=opts, .packages=c("stringr"),) %:% 
    foreach(iter = iters, .inorder=TRUE) %dopar%
    {
      print(sprintf("it=%i,model=%s",it,model))
      df_it <- df[which(df[,1]==it & df[,2]==model),]
      min_lik1 = 1000000
      minmodel = modelData <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
      minmodel_genDataList = 0
      minmodel_genDataNum = 0
      
      for(idx in 1:length(df_it[,1]))
      {
        modelData@alpha = df_it[idx,3]
        modelData@gamma1 = df_it[idx,4]
        modelData@gamma2 = 0.1
        modelData@lambda = 0
        argList <- getArgList(modelData, ratdata)
        lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=1)
        lik1 <- sum(lik[c(1:it)])*-1
        lik2 <- sum(lik[-c(1:800)])*-1
        
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
          minmodel_genDataList = df_it[idx,7]
          minmodel_genDataNum = df_it[idx,8]

        }    
      }
      ### Compute probrow using modelData=minmodel  ####################
      probMat <- TurnsNew::getProbMatrix(argList[[3]], minmodel, argList[[6]], sim=1)

      setwd(res.model.data.dir)
      dfData <- list.files(".", pattern=paste0(ratName,".*genDataset.Rdata"), full.names=FALSE)
      dfData <- dfData[which(str_detect(dfData,paste0("GenData",minmodel_genDataList,"_")))]
      load(dfData)
      trueModelData <- allData[[minmodel_genDataNum]]@simModelData
      trueProbMat <- TurnsNew::getProbMatrix(argList[[3]], trueModelData, argList[[6]], sim=1)
            
      row1 <- round((trueProbMat[iter,] - probMat[iter,]),2)/round(trueProbMat[iter,],2) 
      if(trueProbMat[iter,1] == -1)
      {
        index <- max(which(probMat[1:iter,1] != -1))
      }else{
        index <- max(which(probMat[1:iter,7] != -1))
      }
            #print(sprintf("index=%i",index))
            
      row2 <- round((trueProbMat[index,] - probMat[index,]),2)/round(trueProbMat[index,],2)
      row1[is.nan(row1)] <- 0
      row2[is.nan(row2)] <- 0
      probRow <- row1 + row2  
      probRow[is.infinite(probRow)] <- 0
      list(iter = iter, genDataList=minmodel_genDataList,genDataNum=minmodel_genDataNum,res=minmodel,probRow=probRow)



      #cat(sprintf("it=%i,model=%s, min_lik1=%i",it,model,min_lik1))
    #c(model,it,minmodel@alpha,minmodel@gamma1,minmodel@gamma2,minmodel@lambda,min_lik1,min_lik2)
      
    }
  #minDflist <- unlist(minDfModels, recursive = FALSE)
  #minDfModels <- Reduce(rbind,minDflist)

   

    if(any(grepl("qlearningAvgRwd",models)))
    {
      df <- data.frame(model=character(),
                    iter=integer(),
                    genDataList=integer(),
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

      for(k in c(1:length(minDflist)))
      {
        iter = minDflist[[k]]$iter
        genDataList = minDflist[[k]]$genDataList
        genDataNum = minDflist[[k]]$genDataNum
        generatedData = allData[[genIndex]]
        modelDataRes = minDflist[[k]]$res
        trueModelData = minDflist[[k]]$trueModelData
        
        df[k,1] <- modelDataRes@Model
        df[k,2] <- iter
        df[k,3] <- genDataList
        df[k,4] <- genDataNum
        df[k,5] <- modelDataRes@alpha
        df[k,6] <- modelDataRes@gamma1
        df[k,7] <- modelDataRes@gamma2
        df[k,8] <- modelDataRes@lambda
        df[k,9] <- trueModelData@alpha
        df[k,10] <- trueModelData@gamma1
        df[k,11] <- trueModelData@gamma2
        df[k,12] <- trueModelData@lambda

      }
      rat = ratdata@rat
      if(StabilityTest)
      {
        save(df, file = paste0(res.model.data.dir, "/" , rat,"_",name, timestamp,"_ParamEs_Stability_df.Rdata"))
      }
      else
      {
        save(df,  file = paste0(res.model.data.dir, "/" , rat,"_",name, timestamp,"_ParamEs_Conv_df.Rdata"))
      }

   }
   else
   {
      df <- data.frame(model=character(),
                    iter=integer(),
                    genIndex=integer(),
                    alpha=double(),
                    gamma=double(),
                    trueAlpha=double(),
                    trueGamma=double(),
                    stringsAsFactors=FALSE)
      
      for(k in c(1:length(minDflist)))
      {
        iter = minDflist[[k]]$iter
        genIndex = minDflist[[k]]$genDataIndex
        generatedData = allData[[genIndex]]
        modelDataRes = minDflist[[k]]$res
        trueModelData = minDflist[[k]]$trueModelData
        
        df[k,1] <- modelDataRes@Model
        df[k,2] <- iter
        df[k,3] <- genIndex
        df[k,4] <- modelDataRes@alpha
        df[k,5] <- modelDataRes@gamma1
        df[k,6] <- trueModelData@alpha
        df[k,7] <- trueModelData@gamma1 

      }
      rat = ratdata@rat

      if(StabilityTest)
      {
        save(df, file = paste0(res.model.data.dir, "/" , rat,"_",name, timestamp,"_ParamEs_Stability_df.Rdata"))
      }
      else
      {
        save(df, file = paste0(res.model.data.dir, "/" , rat,"_",name, timestamp,"_ParamEs_Conv_df.Rdata"))
      }
   
   }
      
   
  if(setup.hpc)
  {
    closeCluster(cl)
  }
  
}

