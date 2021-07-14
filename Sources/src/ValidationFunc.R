library(foreach)
library(doParallel)

modifyParam=function(param)
{
  lower = param - (param/50)
  upper = param + (param/50)
  
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
  modelData@gamma2 = modifyParam(modelData@gamma2)
  
  return(modelData)
}


HoldoutTest=function(ratdata,allModelRes,testData,src.dir,setup.hpc)
{
  models = testData@Models
  creditAssignment = testData@creditAssignment

  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))

  mat_res = matrix(0, length(modelNames), length(modelNames))
  colnames(mat_res) <- modelNames
  rownames(mat_res) <- modelNames
  

  print(sprintf("models: %s",toString(modelNames)))
  
  
  if(setup.hpc)
  {
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
    cl <- makeCluster(30, type='PSOCK',outfile = "")
    
    #cl <- startMPIcluster(worker.nodes)
    #registerDoMPI(cl)
  }
  else
  {
    cl <- makeCluster(3, outfile = "")
    #registerDoParallel(cl)
  }
  
  clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes","simulateData","src.dir","populateSimRatModel","getMinimumLikelihood","getModelResults","negLogLikFunc","getAllModelResults","getTurnTimesMat","getModelResultsSeq"))
  clusterEvalQ(cl, source(paste(src.dir,"ModelClasses.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"TurnModel.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"HybridModel1.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"HybridModel2.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"HybridModel3.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"HybridModel4.R", sep="/")))
  clusterEvalQ(cl, source(paste(src.dir,"BaseClasses.R", sep="/")))
  clusterExport(cl, varlist = c("ratdata","allModelRes","testData","creditAssignment","modifyModelData","modifyParam"),envir=environment())
  clusterEvalQ(cl, library("TTR"))
  clusterEvalQ(cl, library("dplyr"))
  clusterEvalQ(cl, library("DEoptim"))

  clusterCall(cl, function() {
    library(doParallel)
    NULL 
  })
  
  registerDoParallel(cl)
  resVecMat = matrix("",0,2) 
  for(chunk in 1:4)
  {
   resVec <-  
   foreach(i=1:length(modelNames), .combine='rbind') %:%
    #print(sprintf("model= %s",model))
    #(clusterExport(cl, varlist = c("trueModelData"),envir=environment()),file='/dev/null')
    #for(i in 1:length(modelNames)) {
      
      foreach(j=c(1:5), .combine='rbind', .inorder=FALSE) %do%{
        cat('Starting i=',i, ', j=',j,' model=', modelNames[i], ' on ',Sys.info()[['nodename']]
, '.\n', sep = '')
        model = modelNames[i] 
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
        trueModelData = modifyModelData(trueModelData)
        end_index = -1
        missedOptimalIter = 0
        while(end_index == -1){
          generated_data = simulateData(trueModelData,ratdata,allModels)
          end_index = getEndIndex(generated_data@allpaths, sim=1, limit=0.95)
          cat('i=',i, ', j=',j,' end_index=', end_index, '.\n', sep = '') 
          missedOptimalIter=missedOptimalIter+1
          
          if(missedOptimalIter>500)
          {
            break
          }
          set.seed(NULL)
        }
        
        if(end_index > -1)
        {
          generated_data = populateSimRatModel(generated_data,modelName)
          allmodelRes = getModelResultsSeq(generated_data,testData,sim=1, src.dir, setup.hpc)
          #allmodelRes = getModelResults(generated_data,testData,sim=1, src.dir, setup.hpc)
          min_method = getMinimumLikelihood(generated_data,allmodelRes,testData,sim=1)
          
        }
        else
        {
          min_method = "Not converging"
        }
        cat('i=',i, ', j=',j,' min_method for ',  modelNames[i], ' is ',min_method,'.\n', sep = '') 
        res_vec = c(toString(model),toString(min_method))

      }
     print(sprintf("chunk=%i",chunk))
     resVecMat = rbind(resVecMat,resVec)
     print(resVecMat)
     #}
     #print(resVecMat)
   }
    #save(mat_res, file = paste0(rat,"_mat_res.Rdata"))
    #print(sprintf("Nb of iterations where optimal behaviour was not learned=%i", missedOptimalIter))
    #print(mat_res)
  rat = ratdata@rat
  save(resVecMat, file = paste0(rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resVec.Rdata"))
  if(setup.hpc)
  {
    stopCluster(cl)
    stopImplicitCluster()
    #closeCluster(cl)
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



testParamEstimation=function(ratdata,allModelRes,testData,src.dir,setup.hpc)
{
  models = testData@Models
  creditAssignment = testData@creditAssignment
  
  paramTest = list()
  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  
  
  if(setup.hpc)
  {
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
    cl <- makeCluster(30, type='PSOCK',outfile = "")
    
    #cl <- startMPIcluster(worker.nodes)
    #registerDoMPI(cl)
  }
  else
  {
    cl <- makeCluster(3, outfile = "")
    #registerDoParallel(cl)
  }
  
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
  
  
  for(i in 1:length(modelNames))
  {
      model = modelNames[i] 
      modelName = strsplit(model,"\\.")[[1]][1]
      creditAssignment = strsplit(model,"\\.")[[1]][2]
      trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
      end_index = -1
      missedOptimalIter = 0
      
      # while(end_index == -1){
      #   generated_data = simulateData(trueModelData,ratdata,allModels)
      #   #generated_data@simModel = trueModelData@Model
      #   #generated_data@simMethod = trueModelData@creditAssignment
      #   end_index = getEndIndex(generated_data@allpaths, sim=1, limit=0.95)
      #   #cat('i=',i, ', j=',j,' end_index=', end_index, '.\n', sep = '') 
      #   missedOptimalIter=missedOptimalIter+1
      #   
      #   if(missedOptimalIter>500)
      #   {
      #     break
      #   }
      #   set.seed(NULL)
      # }
      
      if(end_index > -1)
      {
        iter=as.integer(floor(length(generated_data@allpaths[,1])/200))
        resMat <- 
          foreach(j=c(1:iter), .combine='rbind', .inorder=TRUE) %dopar%{
            rowEnd = j*100
            modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=1)
            argList<-getArgList(modelData,generated_data)
            np.val = length(argList$lower) * 10
            myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9,trace = FALSE, itermax = 200)
            out <-DEoptim(negLogLikFunc,argList$lower,argList$upper,ratdata=argList[[3]],half_index=rowEnd,modelData=argList[[5]],testModel = argList[[6]],sim = argList[[7]],myList)
            modelData = setModelParams(modelData, unname(out$optim$bestmem))
            c(rowEnd,modelData@alpha, modelData@gamma1, modelData@gamma2)
            
          }   
      }
      paramTest = list.append(paramTest,list(model=trueModelData,resMat=resMat))
  }
  rat = ratdata@rat
  save(paramTest, file = paste0(rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_paramTest.Rdata")) 
}

  

getMinimumLikelihood=function(ratdata, allmodelRes,testingdata,sim)
{
  min_index = 0
  min = 100000
  min_method = "null"
  endLearningStage = getEndIndex(ratdata@allpaths,sim=sim, limit=0.95)
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

