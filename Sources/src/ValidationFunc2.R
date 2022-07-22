library(doMPI)
library(rlist)



HoldoutTestNew=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count)
{
  testDataName = testData@Name
  models = testData@Models
  ratName = ratdata@rat
  param.model.data.dir=paste(model.data.dir,"modelParams",ratName,sep="/")
  allModelRes = readModelParamsNew(ratdata,param.model.data.dir,testData, sim=2)
  
  res.model.data.dir=paste(model.data.dir,"holdoutTest",ratName,sep="/")

  # mat_res = matrix(0, length(models), length(models))
  # colnames(mat_res) <- models
  # rownames(mat_res) <- models
  
  
  # print(sprintf("models: %s",toString(models)))
  
  
  #   #worker.nodes = mpi.universe.size()-1
  #   #print(sprintf("worker.nodes=%i",worker.nodes))
  #   dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/"))
  #   cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  #   setRngDoMPI(cl, seed=seed) 
  #   exportDoMPI(cl, c("src.dir","model.data.dir"),envir=environment())
  #   registerDoMPI(cl)
    
  #   cat(sprintf('Running validation with %d worker(s)\n', getDoParWorkers()))
    
  #   initWorkers <-  function() {
  #     source(paste(src.dir,"../ModelClasses.R", sep="/"))
  #     source(paste(src.dir,"PathModel.R", sep="/"))
  #     source(paste(src.dir,"TurnModel.R", sep="/"))
  #     source(paste(src.dir,"HybridModel1.R", sep="/"))
  #     source(paste(src.dir,"HybridModel2.R", sep="/"))
  #     source(paste(src.dir,"HybridModel3.R", sep="/"))
  #     source(paste(src.dir,"HybridModel4.R", sep="/"))
  #     source(paste(src.dir,"../BaseClasses.R", sep="/"))
  #     source(paste(src.dir,"../exportFunctions.R", sep="/"))
  #     source(paste(src.dir,"../ModelUpdateFunc.R", sep="/"))
  #     #attach(myEnv, name="sourced_scripts")
  #   }
    
  #   opts <- list(initEnvir=initWorkers)
    
  
  #  generatedDataList <-  
  #    foreach(i=1:length(models), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %:%
  #    foreach(generation=1:20) %dopar%
  #    {
  #      model = models[i] 
  #      modelName = strsplit(models[i],"\\.")[[1]][1]
  #      creditAssignment = strsplit(models[i],"\\.")[[1]][2]
  #      trueModelData = slot(slot(allModelRes,modelName),creditAssignment)

  #      #trueModelData = modifyModelData(trueModelData) 
  #      simLearns = FALSE 
  #      missedOptimalIter = 0
       
  #      while(!simLearns){
  #        generated_data = simulateData(trueModelData,ratdata,allModels)
  #        #end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.95)
  #        simLearns = checkSimLearns(generated_data@allpaths,sim=1,limit=0.8) 
  #        missedOptimalIter=missedOptimalIter+1
         
  #        if(missedOptimalIter>500)
  #        {
  #           cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1))
	#     break
  #        }
  #        cat(sprintf('model = %s, missedOptimalIter = %i, alpha = %f, gamma = %f\n', model,missedOptimalIter,trueModelData@alpha, trueModelData@gamma1)) 
  #      }
       
  #      if(simLearns)
  #      {
  #        generated_data = populateSimRatModel(ratdata,generated_data,modelName)
  #        generated_data@simModel = trueModelData@Model
  #        generated_data@simMethod = trueModelData@creditAssignment
  #        generated_data@simModelData = trueModelData
  #        generated_data
  #      }
       
  #    }    
 
  # allData<-unlist(generatedDataList)
  # modelNum =  length(allData)
  
  
  # time2<- system.time(  
  #   resList<-
  #     foreach(i = 1:modelNum, .options.mpi=opts) %:%
  #     foreach(model = models) %dopar% {
  #       generated_data = allData[[i]]
  #       modelName = strsplit(model,"\\.")[[1]][1]
  #       creditAssignment = strsplit(model,"\\.")[[1]][2]
  #       modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
  #       argList<-getArgList(modelData,generated_data)
  #       np.val = length(argList$lower) * 10
  #       myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
  #       out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=800,modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
  #       modelData = setModelParams(modelData, unname(out$optim$bestmem))
  #       modelData = setModelResults(modelData,generated_data,allModels)
  #       list(data=generated_data,res=modelData)
  #     }
  # )
  
  # print(time2)
  # rat = ratdata@rat
  # save(resList,  file = paste0(res.model.data.dir,"/",rat, format(Sys.time(),'_%Y%m%d_%H%M%S_'),testDataName,"_resList.Rdata"))
setwd(res.model.data.dir)
print(resData)
rat=ratdata@rat
 resData=list.files(".", pattern = paste0(rat, ".*Mix_resList.Rdata"))
 print(resData)
 load(resData)
 modelNum=length(resList)
  
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
  save(mat_res, generatedDataList,resList,  file = paste0(res.model.data.dir, "/" , rat, format(Sys.time(),'_%Y%m%d_%H%M%S_'),testDataName,"_mat_res.Rdata"))
  
  
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



testParamEstimationNew=function(ratdata,testData,src.dir,setup.hpc,model.data.dir,seed,count)
{
  StabilityTest = FALSE 
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


   generatedDataList <-  
     foreach(i=1:length(models), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %:%
     foreach(generation=1:20) %dopar%
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
   
   print(sprintf("Generated DataList"))   
   allData<-unlist(generatedDataList)
   modelNum =  length(allData)
   #iter=as.integer(floor(length(ratdata@allpaths[,1])/100))
   
   #n = 10
   #x<-c(1:length(ratdata@allpaths[,1]))
   #splits<-split(x, sort(x%%n))
   #maxVecs<-sapply(splits, function(x) max(x))
   n = 8
   sessions<-unique(ratdata@allpaths[,5])
   session_grps<-split(sessions, sort(sessions%%8))
   maxVecs <- c()
   for(grp in c(1:n))
   {
    print(grp)
    begin_ses <- min(session_grps[[grp]])
    end_ses <- max(session_grps[[grp]])
    indices_of_ses <- which(ratdata@allpaths[,5]>=begin_ses & ratdata@allpaths[,5] <=end_ses)
    maxVecs <- c(maxVecs,max(indices_of_ses))
   }
   
   resList<-
     foreach(i=maxVecs, .combine='rbind', .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"), .inorder=TRUE) %:%
      foreach(j = c(1:modelNum)) %dopar% {
       generated_data = allData[[j]]
       #if(j==iter)
       #{
       #  rowEnd = length(generated_data@allpaths[,1])
       #}else{
       #  rowEnd = i*100
       #}
       rowEnd = i
       model = generated_data@simModel
       creditAssignment = generated_data@simMethod

       cat(sprintf('model = %s, creditAssignment=%s, rowEnd = %i\n', model,creditAssignment,rowEnd))
       modelData =  new("ModelData", Model=model, creditAssignment = creditAssignment, sim=1)
       argList<-getArgList(modelData,generated_data)
       np.val = length(argList$lower) * 10
       myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
       out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=rowEnd,modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
       modelData <- setModelParams(modelData, unname(out$optim$bestmem))
       #modelData <- setModelResults(modelData, ratdata, allModels)
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
       
       #diff1 <-  (trueProbMat[rowEnd,] - probMat[rowEnd,])
       #diff2 <-  (trueProbMat[index,] - probMat[index,])
       #diff <- diff1 + diff2
        
      alphahat = modelData@alpha
      gammahat = modelData@gamma1
      alpha = trueModelData@alpha
      gamma = trueModelData@gamma1

       #cat(sprintf("model=%s, iter=%i, alphahat=%f, alpha=%f, gammahat=%f, gamma=%f\n",model,rowEnd,alphahat,alpha,gammahat,gamma)) 
       #cat(sprintf("trueProbMat: %s\n",toString(round(trueProbMat[rowEnd,],2))))
       #cat(sprintf("probMat: %s\n",toString(round(probMat[rowEnd,],2))))
       #cat(sprintf("trueProbMat: %s\n",toString(round(trueProbMat[index,],2)))) 
       #cat(sprintf("probMat: %s\n",toString(round(probMat[index,],2))))
       #cat(sprintf("%s, %s\n",toString(round(probRow[1:13]),2),model))
       #cat(sprintf("%s", paste(round(probRow[1:13]),2, collapse=" ")))
       #cat(sprintf("%s\n", paste(round(diff[1:12],2), collapse=" ")))
       #cat(sprintf("%s\n", paste(round(probRow[1:12],2), collapse=" ")))

      # cat(sprintf("Params= (%s)\n", toString(unname(out$optim$bestmem))))
      # cat(sprintf("BestLik= %s, testModel=%s,\n", out$optim$bestval,argList[[6]]@Name))
      # lik <- TurnsNew::getTurnsLikelihood(argList[[3]], modelData, argList[[6]], sim=1)
      #likSum <- (-1) *sum(lik[1:rowEnd])
      #cat(sprintf("likSum=%f\n", likSum)) 
      #cat(sprintf('Success: rowEnd = %i, alpha = %f, gamma = %f\n',rowEnd, modelData@alpha, modelData@gamma1))
       list(iter = rowEnd, genDataIndex = j,data=generated_data,res=modelData,probRow=probRow,trueModelData=trueModelData)
     }
   
   

   rat = ratdata@rat
   save(resList,  file = paste0(res.model.data.dir,"/",rat, timestamp,"_ParamEstResList.Rdata"))
   
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
    save(df, generatedDataList,resList,  file = paste0(res.model.data.dir, "/" , rat, timestamp,"_ParamEs_Stability_df.Rdata"))
   }
   else
   {
    save(df, generatedDataList,resList,  file = paste0(res.model.data.dir, "/" , rat, timestamp,"_ParamEs_Conv_df.Rdata"))
   }
   
   
  if(setup.hpc)
  {
    closeCluster(cl)
  }
  
}

