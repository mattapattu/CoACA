library(doMPI)
library(rlist)



HoldoutTestNew=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count, gridMat, name)
{
  
  ## Test settings ###############
  
  DataGenerated = FALSE
  
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
    
  opts <- list(initEnvir=initWorkers)
    
  if(!DataGenerated)
  {
    generatedDataList <-  
     foreach(i=1:length(models), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %:%
     foreach(generation=1:10) %dopar%
     {
       model = models[i] 
       modelName = strsplit(models[i],"\\.")[[1]][1]
       creditAssignment = strsplit(models[i],"\\.")[[1]][2]
       trueModelData = slot(slot(allModelRes,modelName),creditAssignment)

       #trueModelData = modifyModelData(trueModelData) 
       simLearns = FALSE 
       missedOptimalIter = 0
       
       while(!simLearns){
         generated_data = simulateData(trueModelData,ratdata,allModels)
         #end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.95)
         simLearns = checkSimLearns(generated_data@allpaths,sim=1,limit=0.8) 
         missedOptimalIter=missedOptimalIter+1
         
         if(missedOptimalIter>500)
         {
            cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1))
	    break
         }
         cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
       }
       
       if(simLearns)
       {
         generated_data = populateSimRatModel(ratdata,generated_data,modelName)
         generated_data@simModel = trueModelData@Model
         generated_data@simMethod = trueModelData@creditAssignment
         generated_data@simModelData = trueModelData
         generated_data
       }
       
     }   

    rat=ratdata@rat
    print(sprintf("Generated DataList"))   
    save(generatedDataList,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_generatedDataList.Rdata"))
   

  }else{
    ## load generatedDataList
  }
 
  allData<-unlist(generatedDataList)
  modelNum =  length(allData)
  
  chunkSize = 75
  #chunkSize = length(gridMat[,1])/getDoParWorkers()
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  
  time2<- system.time(  
    resList<-
      foreach(i = 1:length(gridMat[,1]), .combine='rbind', .options.mpi=opts) %dopar%
      { 
        generated_data = allData[[i]]

        alpha = gridMat[idx,1]
        gamma1 = gridMat[idx,2]
        model = gridMat[idx,3]
        modelNb = gridMat[idx,4]
        generatedData = allData[[modelNb]]

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
        opt <- optim(par = c(alpha,gamma1),
                         fn = negLogLikFunc,ratdata=generatedData,half_index=800,modelData=modelData,testModel = argList[[6]],sim = 1)
            modelData = setModelParams(modelData, c(opt$par,0.1,0))

        
        modelData = setModelResults(modelData,generatedData,allModels)
        list(data=generatedData,res=modelData)
      }
  )
  
  print(time2)
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



testParamEstimationNew=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count,gridMat,name)
{
  ## Test settings ###############
  
  StabilityTest = TRUE 
  DataGenerated = FALSE
  
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
 
   opts <- list(initEnvir=initWorkers) 
 
   source(paste(src.dir,"../exportFunctions.R", sep="/")) 
   #for(i in c(1:length(modelNames)))
   #{
   # model = modelNames[i]
   # modelName = strsplit(model,"\\.")[[1]][1]
   # creditAssignment = strsplit(model,"\\.")[[1]][2]
   # trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
   # trueModelData = modifyModelData(trueModelData) 
   #} 
   if(!DataGenerated)
   {
      generatedDataList <-  
      foreach(i=1:length(models), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %:%
      foreach(generation=1:10) %dopar%
      {
        
        model = models[i] 
        modelName = strsplit(models[i],"\\.")[[1]][1]
        creditAssignment = strsplit(models[i],"\\.")[[1]][2]
        trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
        if(StabilityTest)
        {
          trueModelData = modifyModelData(trueModelData)
        }
        #trueModelData = modifyModelData(trueModelData) 
        simLearns = FALSE 
        missedOptimalIter = 0
        
        while(!simLearns){
          generated_data = simulateData(trueModelData,ratdata,allModels)
          #end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.95)
          simLearns = checkSimLearns(generated_data@allpaths,sim=1,limit=0.8) 
          missedOptimalIter=missedOptimalIter+1
          
          if(missedOptimalIter>500)
          {
              cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1))
        break
          }
          cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
        }
        
        if(simLearns)
        {
          generated_data = populateSimRatModel(ratdata,generated_data,modelName)
          generated_data@simModel = trueModelData@Model
          generated_data@simMethod = trueModelData@creditAssignment
          generated_data@simModelData = trueModelData
          generated_data
        }
        
      } 
    
    rat=ratdata@rat
    print(sprintf("Generated DataList"))   
    save(generatedDataList,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_generatedDataList.Rdata"))

   }else{
    ### Load generatedDataList.Rdata
   }

   allData<-unlist(generatedDataList)
   modelNum =  length(allData)

  #chunkSize = length(gridMat[,1])/getDoParWorkers()
  chunkSize = 75
  opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 

  print(sprintf("gridMat len=%i, getDoParWorkers=%i",length(gridMat[,1]),getDoParWorkers()))
   
  resList <- 
      foreach(idx = 1:length(gridMat[,1]), .combine='rbind', .options.mpi=opts) %dopar% {
        #start_idx=sequences[i]
        #idx = start_idx+j
        alpha = gridMat[idx,1]
        gamma1 = gridMat[idx,2]
        iter = gridMat[idx,3]
        modelNum = gridMat[idx,4]
        generatedData = allData[[modelNum]]

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
        opt <- optim(par = c(alpha,gamma1),
                  fn = negLogLikFunc,ratdata=generatedData,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 1)
        modelData = setModelParams(modelData, c(opt$par,0.1,0))

        probMat <- TurnsNew::getProbMatrix(argList[[3]], modelData, argList[[6]], sim=1)
        trueModelData <- generated_data@simModelData
        trueProbMat <- TurnsNew::getProbMatrix(argList[[3]], trueModelData, argList[[6]], sim=1)
            
        row1 <- round((trueProbMat[rowEnd,] - probMat[rowEnd,]),2)/round(trueProbMat[rowEnd,],2) 
        if(trueProbMat[rowEnd,1] == -1)
          {
            index <- max(which(probMat[1:rowEnd,1] != -1))
          }else{
            index <- max(which(probMat[1:rowEnd,7] != -1))
          }
            #print(sprintf("index=%i",index))
            
        row2 <- round((trueProbMat[index,] - probMat[index,]),2)/round(trueProbMat[index,],2)
        row1[is.nan(row1)] <- 0
        row2[is.nan(row2)] <- 0
        probRow <- row1 + row2  
        probRow[is.infinite(probRow)] <- 0
        list(iter = iter, genDataIndex = j,data=generated_data,res=modelData,probRow=probRow,trueModelData=trueModelData)
      }

   
    rat = ratdata@rat
    save(resList,  file = paste0(res.model.data.dir,"/",rat,"_",name, timestamp,"_ParamEstResList.Rdata"))
   

    if(any(grepl("qlearningAvgRwd",models)))
    {
      df <- data.frame(model=character(),
                    iter=integer(),
                    genIndex=integer(),
                    alpha=double(),
                    gamma1=double(),
                    gamma2=double(),
                    lambda=double(),
                    trueAlpha=double(),
                    trueGamma1=double(),
                    trueGamma2=double(),
                    trueLambda=double(),
                    stringsAsFactors=FALSE)

      for(k in c(1:length(resList)))
      {
        iter = resList[[k]]$iter
        genIndex = resList[[k]]$genDataIndex
        generated_data = resList[[k]]$data
        modelDataRes = resList[[k]]$res
        trueModelData = resList[[k]]$trueModelData
        
        df[k,1] <- modelDataRes@Model
        df[k,2] <- iter
        df[k,3] <- genIndex
        df[k,4] <- modelDataRes@alpha
        df[k,5] <- modelDataRes@gamma1
        df[k,6] <- modelDataRes@gamma2
        df[k,7] <- modelDataRes@lambda
        df[k,8] <- trueModelData@alpha
        df[k,9] <- trueModelData@gamma1
        df[k,10] <- trueModelData@gamma2
        df[k,11] <- trueModelData@lambda

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
      
      for(k in c(1:length(resList)))
      {
        iter = resList[[k]]$iter
        genIndex = resList[[k]]$genDataIndex
        generated_data = resList[[k]]$data
        modelDataRes = resList[[k]]$res
        trueModelData = resList[[k]]$trueModelData
        
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

