# library(GA)
#library(DEoptim)
# 
# library(rgenoud)
library(rlist)
#library(foreach)
#library(doParallel)
#library(Rmpi)
library(doMPI);
# library(snow);
# library(doSNOW);

#library(GA)
#library(DEoptim)
#library(Rmpi)
#library(rgenoud)
#library(rlist)
#library(parallel)
#library(foreach)
#library(doParallel)
#library(doMPI);
#library(snow)
#library(doSNOW)
#library(future)
library(bigsnpr)
#library(Rmpi)
#library(parallelly)
#library(doFuture)
library(listenv)
library(nloptr)


analyzeParamSpaceV2=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)
{
  models = testData@Models
  #########################

  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  model.data.dir=file.path(model.data.dir, ratName)
  dir.create(file.path(model.data.dir,"modelParams"), showWarnings = FALSE)
  model.data.dir=file.path(model.data.dir, "modelParams")
  print(model.data.dir)

  dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  
  cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  setRngDoMPI(cl, seed=count)
  exportDoMPI(cl, c("src.dir","model.data.dir","model.src", "gamma2_Global", "lambda_Global"), envir=environment())
  registerDoMPI(cl)
  
   initWorkers <-  function() {
       source(paste(src.dir, "ModelClasses.R", sep = "/"))
       source(paste(model.src, "PathModel.R", sep = "/"))
       source(paste(model.src, "TurnModel.R", sep = "/"))
       source(paste(model.src, "HybridModel1.R", sep = "/"))
       source(paste(model.src, "HybridModel2.R", sep = "/"))
       source(paste(model.src, "HybridModel3.R", sep = "/"))
       source(paste(model.src, "HybridModel4.R", sep = "/"))
       #source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
       source(paste(src.dir,"exportFunctions.R", sep="/"))
   
       #attach(myEnv, name="sourced_scripts")
     }
  
  #chunkSize = length(gridMat[,1])/getDoParWorkers()
  #chunkSize = 150
  opts <- list(initEnvir=initWorkers) 
  source(paste(src.dir,"exportFunctions.R", sep="/")) 
  print(sprintf("gridMat len=%i, getDoParWorkers=%i",length(gridMat[,1]),getDoParWorkers()))
   
  resMat <- 
      foreach(idx = 1:length(gridMat[,1]), .packages="DEoptim",.options.mpi=opts) %dopar% {
            
            cat(names(environment()))
            cat("\n")
            cat(sprintf('gamma2_Global=%f, lambda_Global=%f\n', gamma2_Global,lambda_Global))
            source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
            #start_idx=sequences[i]
            #idx = start_idx+j
            #alpha = gridMat[idx,1]
            #gamma1 = gridMat[idx,2]
            iter = gridMat[idx,1]
            model = gridMat[idx,2]
            #cat(sprintf("idx= %i,alpha=%.10f,gamma1=%.10f\n", idx,alpha,gamma1))
            #cat(sprintf('Rat is %s, model is %s\n', ratName,model))


            modelName = strsplit(model,"\\.")[[1]][1]
            #cat(sprintf('rat=%s, iter=%i,modelName = %s\n', ratName,iter,modelName))
            creditAssignment = strsplit(model,"\\.")[[1]][2]
            cat(sprintf('rat=%s, iter=%i,modelName=%s,creditAssignment = %s\n', ratName,iter,modelName,creditAssignment))


            #cat(sprintf('rat=%s, iter=%i,model = %s\n', ratName,iter,modelName))
            modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=2)
            argList<-getArgList(modelData,ratdata)
            #cat(sprintf('gamma2=%f, lambda=%f\n', modelData@gamma2,modelData@lambda))
            #cat(sprintf("res$alpha=%.10f, res$gamma1=%.10f",res$minlevels[1],res$minlevels[2]))
            #cat("Here1")
            myList <- DEoptim.control(initialpop=initpop, F=0.8, CR = 0.9,trace = FALSE, itermax = 30)
            out <-DEoptim(negLogLikFunc,lower=c(0,0),upper=c(1,1),ratdata=ratdata,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 2,myList)

            # res <- bobyqa(x0 = c(alpha,gamma1),lower = c(0,0),upper=c(1,1),
            #              fn = negLogLikFunc,ratdata=ratdata,half_index=iter,modelData=modelData,testModel = argList[[6]],sim = 2)
            #cat("Here2")
            if(out$optim$bestval >= 1000000)
            {
              modelData = setModelParams(modelData, c(NA,NA,modelData@gamma2,modelData@lambda))
              c(iter,modelName,NA, NA,modelData@gamma2,modelData@lambda,NA,idx)

            }else
            {
              modelData = setModelParams(modelData, c(out$optim$bestmem,modelData@gamma2,modelData@lambda))
                lik1 <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2)
                lik1 <- sum(lik1[(1:iter)])*-1
                #reprint(lik1)
                if (is.infinite(lik1)) {
                  lik1= 1000000
                }else if (is.nan(lik1)) {
                  #print(sprintf("Alpha = %f", alpha))
                  lik1 = 1000000
                }else if (is.na(lik1)) {
                  #print(sprintf("Alpha = %f, Gamma1=%f", alpha,gamma1))
                  lik1 = 1000000
                }
                #cat(sprintf('Iter=%i, alpha = %.10f, gamma1 = %.15f, gamma2 = %f, lik1=%f\n', iter,modelData@alpha, modelData@gamma1,0.1,lik1))
                c(iter,modelName,modelData@alpha, modelData@gamma1,modelData@gamma2,modelData@lambda,lik1,idx)
            }
            

      }
   resMat <- Reduce(rbind,resMat)
   print(resMat)
   rat = ratdata@rat
   save(resMat, file = paste0(model.data.dir,"/",rat,"_",name, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMat.Rdata"))   
}


learningStageModelSelection=function(ratdata,data.dir)
{
  min_index = 0
  min = 100000
  min_method = "null"
  ratName = ratdata@rat
  #endLearningStage = getEndIndex(ratName,ratdata@allpaths,sim=sim, limit=0.95)
  #endLearningStage = endLearningStage/2
  #half_stage = endLearningStage/2

  
  model.data.dir1=file.path(data.dir, "ARLTestSuite",ratName,"modelParams")
  model.data.dir2=file.path(data.dir, "CoACAR5",ratName,"modelParams")
  model.data.dir3=file.path(data.dir, "DRLTestSuite",ratName,"modelParams")
  
  model.data.dirs = c(model.data.dir1,model.data.dir2,model.data.dir3)
  crAssgns = c("qlearningAvgRwd","aca2","qlearningDisRwd")
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  half_stage = 800
  for(i in c(1:3))
  {
    model.data.dir = model.data.dirs[i]
    creditAssignment = creditAssignment[i]
    print(model.data.dir)
    setwd(model.data.dir)
    ratName = ratdata@rat
    details = file.info(list.files(".", pattern=paste0(ratName,".*resMatList.Rdata"), full.names=FALSE))
    details = details[with(details, order(as.POSIXct(mtime))), ]
    files = rownames(details)
    load(files[length(files)])
    for(m in testData@Models)
    {
      modelName = models[i]
      modelData =  new("ModelData", Model=modelName, creditAssignment = creditAssignment, sim=2)
      modelData@alpha <- as.numeric(resMat[which(as.numeric(resMat[,1])==half_stage & resMat[,2] == modelName),3])
      modelData@gamma1 <- as.numeric(resMat[which(as.numeric(resMat[,1])==half_stage & resMat[,2] == modelName),4])
      modelData@gamma2 <- as.numeric(resMat[which(as.numeric(resMat[,1])==half_stage & resMat[,2] == modelName),5])
      modelData@lambda <- as.numeric(resMat[which(as.numeric(resMat[,1])==half_stage & resMat[,2] == modelName),6])
      
      argList<-getArgList(modelData,ratdata)
      lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2) 
      lik = (-1)*sum(lik[(1:half_stage)])
        #lik = (-1)*sum(lik[(half_stage:endLearningStage)])
      #modelName = paste(modelData@Model,modelData@creditAssignment,sep=".")

      print(sprintf("model=%s,likelihood=%f",m,lik))

      if(is.nan(lik))
      {
        print(sprintf("model=%s,likelihood is NAN, skipping to next method",m))
        next
      }
      
      if(lik < min)
      {
        min = lik
        min_method = m
      } 
      
    }
  }
  
  print(sprintf("Model selected for rat %s is %s",min_method))

}

generateParamResMatV2=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,testSuite)
{
  
  #################################

  #models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #paramTest = list()
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  ratName = ratdata@rat
  #model.data.dir = paste(model.data.dir,"modelParams",ratName,sep="/")
  model.data.dir=file.path(model.data.dir, ratName)
  model.data.dir=file.path(model.data.dir, "modelParams")
  print(model.data.dir)
  setwd(model.data.dir)
  resMatList <- listenv()

  for(i in c(1:6))
  {
    
    pattern=paste0(ratName,"_mParams",i,"_.*Rdata")
    print(pattern)
    resMat=list.files(".", pattern=pattern, full.names=FALSE)
    print(resMat)
    load(resMat)
    resMatList[[i]] <- resMat
  }
  resMat <- Reduce(rbind,resMatList)
  save(resMat, file = paste0(model.data.dir,"/",ratName,"_",testSuite, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMatList.Rdata")) 

}


readModelParamsNew <- function(ratdata,param.model.data.dir,testingdata, sim){
  
  print(sprintf("Inside readModelParams"))
  models <- testingdata@Models
  
  setwd(param.model.data.dir)
  print(sprintf("param.model.data.dir=%s",param.model.data.dir))
  ratName = ratdata@rat
  load(list.files(".", pattern=paste0(ratName,".*resMatList.Rdata"), full.names=FALSE))
  modelParamsList <- resMat[which(as.numeric(resMat[,1])==  length(ratdata@allpaths[,1])),]

  allmodelRes <- new("AllModelRes")
  #setwd(res.dir)
  #print(res.dir)
  rat=ratdata@rat
  #paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
  #print(paramTestData)
  #load(paramTestData)
  
  
  for (i in 1:length(models))
  {
    modelName = strsplit(models[i],"\\.")[[1]][1]
    creditAssignment = strsplit(models[i],"\\.")[[1]][2]
    modelData <- new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim = sim)
    print(sprintf("modelName=%s,creditAssignment=%s",modelName,creditAssignment))

    modelData@alpha <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),3])
    modelData@gamma1 <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),4])
    modelData@gamma2 <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),5])
    modelData@lambda <- as.numeric(modelParamsList[which(modelParamsList[,2]==modelName),6])

    modelData <- setModelResults(modelData, ratdata, allModels)
    allmodelRes <- addModelData(allmodelRes, modelData)

  }
  
  return(allmodelRes)
  
}