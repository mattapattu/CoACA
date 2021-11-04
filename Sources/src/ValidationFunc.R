library(doMPI)
>>>>>>> Stashed changes
library(rlist)


modifyParam=function(param)
{
  lower = param - (param/20)
  upper = param + (param/20)
  
  if(lower <=0 )
    lower = param
  
  if(upper > 1)
    upper = 1
  
  newparam = runif(1, lower, upper)
  return(newparam)
}

modifyModelData=function(modelData)
{
  
  modelData@alpha = modifyParam(modelData@alpha)
  modelData@gamma1 = modifyParam(modelData@gamma1)
  #modelData@gamma2 = modifyParam(modelData@gamma2)
  
  return(modelData)
}

unitTestHoldOut=function(ratdata,allModelRes,testData,src.dir)
{
  rat = ratdata@rat
  models = testData@Models
  creditAssignment = testData@creditAssignment
  
  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  
  mat_res = matrix(0, length(modelNames), length(modelNames))
  colnames(mat_res) <- modelNames
  rownames(mat_res) <- modelNames
  
  print(sprintf("models: %s",toString(modelNames)))
  
  for(i in c(1:length(modelNames)))
  {
      model = modelNames[i] 
      modelName = strsplit(model,"\\.")[[1]][1]
      creditAssignment = strsplit(model,"\\.")[[1]][2]
      trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
      trueModelData = modifyModelData(trueModelData) 
      end_index = -1
      missedOptimalIter = 0
      
      while(end_index == -1){
        generated_data = simulateData(trueModelData,ratdata,allModels)
        end_index = getEndIndex(rat,generated_data@allpaths, sim=1, limit=0.95)
        missedOptimalIter=missedOptimalIter+1
        
        if(missedOptimalIter>2000)
        {
          break
        }
        set.seed(missedOptimalIter)
      }
      
      print(sprintf("Model=%s converges, missedOptimalIter =  %i", model,missedOptimalIter))
      
      
      if(end_index > -1)
      {
        #debug(populateSimRatModel)
        generated_data = populateSimRatModel(ratdata,generated_data,modelName)
        bool1 <- any(is.na(generated_data@allpaths) == TRUE)
        bool2 <- any(is.na(generated_data@turnTimes) == TRUE)
        bool3 <- any(is.na(generated_data@hybridModel1) == TRUE)
        bool4 <- any(is.na(generated_data@hybridModel2) == TRUE)
        bool5 <- any(is.na(generated_data@hybridModel3) == TRUE)
        bool6 <- any(is.na(generated_data@hybridModel4) == TRUE)
        if(!bool1 && !bool2 && !bool3 && !bool4 && !bool5 && !bool6)
        {
          print(sprintf("Successfully converted data"))
        }
        else
        {
          print(sprintf("Error: NA in generated data"))
        }
        
      }
      
    }
}

HoldoutTest=function(ratdata,allModelRes,testData,src.dir,setup.hpc,model.data.dir)
{
  models = testData@Models
  creditAssignment = testData@creditAssignment
  
  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  
  ratName = ratdata@rat
  
  mat_res = matrix(0, length(modelNames), length(modelNames))
  colnames(mat_res) <- modelNames
  rownames(mat_res) <- modelNames
  
  
  print(sprintf("models: %s",toString(modelNames)))
  
  
  if(setup.hpc)
  {
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
    cl <- startMPIcluster()
    setRngDoMPI(cl, seed=1234)
    exportDoMPI(cl, c("src.dir"),envir=environment())
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
    
  }
  else
  {
    cl <- makeCluster(3, outfile = "")
    #registerDoParallel(cl)
    clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes","simulateData","src.dir","populateSimRatModel","getMinimumLikelihood","getModelResults","negLogLikFunc","getAllModelResults","getTurnTimesMat","getModelResultsSeq"))
    clusterEvalQ(cl, source(paste(src.dir,"../ModelClasses.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"PathModel.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"TurnModel.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel1.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel2.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel3.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel4.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"../BaseClasses.R", sep="/")))
    clusterExport(cl, varlist = c("ratdata","allModelRes","testData","creditAssignment","modifyModelData","modifyParam"),envir=environment())
    clusterEvalQ(cl, library("TTR"))
    clusterEvalQ(cl, library("dplyr"))
    clusterEvalQ(cl, library("DEoptim"))
    
    clusterCall(cl, function() {
      library(doParallel)
      NULL 
    })
    
    registerDoParallel(cl)
  }
  
  
  time1 <-system.time( 
    generatedDataList <-  
      foreach(i=1:length(modelNames), .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"),.export=c("testData")) %:%
      foreach(generation=1:50) %dopar%
      {
        model = modelNames[i] 
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
        trueModelData = modifyModelData(trueModelData) 
        end_index = -1
        missedOptimalIter = 0
        
        while(end_index == -1){
          generated_data = simulateData(trueModelData,ratdata,allModels)
          end_index = getEndIndex(ratName,generated_data@allpaths, sim=1, limit=0.85)
          missedOptimalIter=missedOptimalIter+1
          
          if(missedOptimalIter>2000)
          {
            break
          }
          set.seed(missedOptimalIter)
        }
        
        if(end_index > -1)
        {
          generated_data = populateSimRatModel(ratdata,generated_data,modelName)
          generated_data@simModel = trueModelData@Model
          generated_data@simMethod = trueModelData@creditAssignment
          generated_data
        }
        
      }
  )
  
  print(time1)
  
  allData<-unlist(generatedDataList)
  modelNum =  length(allData)
  
  
  time2<- system.time(  
    resList<-
      foreach(i = 1:modelNum, .options.mpi=opts) %:%
      foreach(model = modelNames) %dopar% {
        generated_data = allData[[i]]
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
        argList<-getArgList(modelData,generated_data)
        np.val = length(argList$lower) * 10
        myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
        out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=argList[[4]],modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
        modelData = setModelParams(modelData, unname(out$optim$bestmem))
        modelData = setModelResults(modelData,generated_data,allModels)
        list(data=generated_data,res=modelData)
      }
  )
  
  print(time2)
  rat = ratdata@rat
  save(resList,  file = paste0(model.data.dir,"/",rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resList.Rdata"))
  for(i in 1:modelNum)
  {
    min_method = ""
    min_lik = 100000
    gen_model = resList[[i]][[1]]$data@simModel
    gen_method = resList[[i]][[1]]$data@simMethod
    gen_modelname = paste(gen_model, gen_method, sep=".")
    
    for(m in 1:length(modelNames))
    {
      res_model = resList[[i]][[m]]$res@Model
      res_method = resList[[i]][[m]]$res@creditAssignment
      likelihood = resList[[i]][[m]]$res@likelihood
      res_modelname = paste(gen_model, gen_method, sep=".")
      
      if(likelihood < min_lik)
      {
        min_method = res_modelname
      }
      
    }
    mat_res[gen_modelname,res_modelname] = mat_res[gen_modelname,res_modelname]+1
  }
  
  rat = ratdata@rat
  save(mat_res, generatedDataList,resList,  file = paste0(model.data.dir, "/" , rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_mat_res.Rdata"))
  
  
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



testParamEstimation=function(ratdata,allModelRes,testData,src.dir,setup.hpc,model.data.dir)
{
  models = testData@Models
  creditAssignment = testData@creditAssignment
  
  paramTest = list()
  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  
  ratName = ratdata@rat 
  
  if(setup.hpc)
  {
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
    cl <- startMPIcluster()
    setRngDoMPI(cl, seed=1234)
    
    exportDoMPI(cl, c("src.dir"),envir=environment())
    registerDoMPI(cl)
    
    initWorkers <-  function() {
      source(paste(src.dir,"ModelClasses.R", sep="/"))
      source(paste(src.dir,"TurnModel.R", sep="/"))
      source(paste(src.dir,"HybridModel1.R", sep="/"))
      source(paste(src.dir,"HybridModel2.R", sep="/"))
      source(paste(src.dir,"HybridModel3.R", sep="/"))
      source(paste(src.dir,"HybridModel4.R", sep="/"))
      source(paste(src.dir,"BaseClasses.R", sep="/"))
      source(paste(src.dir,"exportFunctions.R", sep="/"))
      source(paste(src.dir,"ModelUpdateFunc.R", sep="/"))
      #attach(myEnv, name="sourced_scripts")
    }
    
    opts <- list(initEnvir=initWorkers) 
  }
  else
  {
    cl <- makeCluster(3, outfile = "")
    #registerDoParallel(cl)
    clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes","simulateData","src.dir","populateSimRatModel","getMinimumLikelihood","getModelResults","negLogLikFunc","getAllModelResults","getTurnTimesMat","getModelResultsSeq"))
    clusterEvalQ(cl, source(paste(src.dir,"ModelClasses.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"TurnModel.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel1.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel2.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel3.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"HybridModel4.R", sep="/")))
    clusterEvalQ(cl, source(paste(src.dir,"BaseClasses.R", sep="/")))
    clusterExport(cl, varlist = c("ratdata","allModelRes","testData","creditAssignment"),envir=environment())
    clusterEvalQ(cl, library("TTR"))
    clusterEvalQ(cl, library("dplyr"))
    clusterEvalQ(cl, library("DEoptim"))
    
    clusterCall(cl, function() {
      library(doParallel)
      NULL 
    })
    
    registerDoParallel(cl)
    
  }
  
  for(i in 1:length(modelNames))
  {
    model = modelNames[i] 
    #cat(sprintf('Model is %s\n', model))
    modelName = strsplit(model,"\\.")[[1]][1]
    creditAssignment = strsplit(model,"\\.")[[1]][2]
    trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
    end_index = -1
    missedOptimalIter = 0
    
    while(end_index == -1){
      generated_data = simulateData(trueModelData,ratdata,allModels)
      #generated_data@simModel = trueModelData@Model
      #generated_data@simMethod = trueModelData@creditAssignment
      end_index = getEndIndex(ratName, generated_data@allpaths, sim=1, limit=0.95)
      #cat('i=',i, ', j=',j,' end_index=', end_index, '.\n', sep = '') 
      missedOptimalIter=missedOptimalIter+1
      
      if(missedOptimalIter>500)
      {
        break
      }
      cat(sprintf('model = %s, missedOptimalIter=%i\n', model, missedOptimalIter))
      set.seed(missedOptimalIter)
    }
    rat = ratdata@rat
    save(generated_data, file = paste0(model.data.dir, "/", rat, "_", modelName,"_genData.Rdata")) 
    if(end_index > -1)
    {
      iter=as.integer(floor(length(generated_data@allpaths[,1])/100))-1
      resMat <- 
        foreach(j=c(1:iter), .combine='rbind', .options.mpi=opts,.packages = c("rlist","DEoptim","dplyr","TTR"), .inorder=TRUE) %dopar%{
          rowEnd = j*100
          #cat(sprintf('rowEnd = %i\n', rowEnd))
          modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
          argList<-getArgList(modelData,generated_data)
          np.val = length(argList$lower) * 10
          myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
          out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=rowEnd,modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
          modelData = setModelParams(modelData, unname(out$optim$bestmem))
          #cat(sprintf('Success\n'))
          c(rowEnd,modelData@alpha, modelData@gamma1, modelData@gamma2)
          
        }   
    }
    paramTest = list.append(paramTest,list(model=trueModelData,resMat=resMat))
  }
  
  
  rat = ratdata@rat
  save(paramTest, file = paste0(model.data.dir,"/",rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_paramTest.Rdata")) 
  
  if(setup.hpc)
  {
    closeCluster(cl)
  }
  
}



getMinimumLikelihood=function(ratdata, allmodelRes,testingdata,sim)
{
  min_index = 0
  min = 100000
  min_method = "null"
  ratName = ratdata@rat
  endLearningStage = getEndIndex(ratName,ratdata@allpaths,sim=sim, limit=0.95)
  #endLearningStage = endLearningStage/2
  half_stage = endLearningStage/2
  for(m in testingdata@Models)
  {
    for(crAssgn in testingdata@creditAssignment)
    {
      modelData = getModelData(allmodelRes,m,crAssgn)
      lik = modelData@likelihood
      lik = (-1)*sum(lik[-(1:half_stage)])
      #lik = (-1)*sum(lik[(half_stage:endLearningStage)])
      modelName = paste(modelData@Model,modelData@creditAssignment,sep=".")
      
      print(sprintf("model=%s,likelihood=%f",modelName,lik))
      
      if(lik < min)
      {
        min = lik
        min_method = modelName
      }
    }
  }
  return(min_method)
}

