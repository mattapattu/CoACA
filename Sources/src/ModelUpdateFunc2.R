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
       source(paste(src.dir, "ModelClasses.R", sep = "/"), local=environment())
       source(paste(model.src, "PathModel.R", sep = "/"), local=environment())
       source(paste(model.src, "TurnModel.R", sep = "/"), local=environment())
       source(paste(model.src, "HybridModel1.R", sep = "/"), local=environment())
       source(paste(model.src, "HybridModel2.R", sep = "/"), local=environment())
       source(paste(model.src, "HybridModel3.R", sep = "/"), local=environment())
       source(paste(model.src, "HybridModel4.R", sep = "/"), local=environment())
       #source(paste(src.dir, "BaseClasses.R", sep = "/"), local=environment())
       source(paste(src.dir,"exportFunctions.R", sep="/"), local=environment())
   
       #attach(myEnv, name="sourced_scripts")
     }
  
  #chunkSize = length(gridMat[,1])/getDoParWorkers()
  #chunkSize = 150
  opts <- list(initEnvir=initWorkers) 
  source(paste(src.dir,"../exportFunctions.R", sep="/")) 
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


generateParamResMatV2=function(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count)
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
    #print(pattern)
    resMat=list.files(".", pattern=pattern, full.names=FALSE)
    load(resMat)
    resMatList[[i]] <- resMat
  }
  resMat <- Reduce(rbind,resMatList)
  save(resMat, file = paste0(model.data.dir,"/",ratName,"_", format(Sys.time(),'_%Y%m%d_%H%M%S'),"_resMatList.Rdata")) 

  # df <- as.data.frame(resMat)
  # cols.num <- c(1,3,4,5,6,7)
  # df[,cols.num] <- lapply(cols.num,function(x) as.numeric(df[[x]]))
  # models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")

  # dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  
  # iter = unique(as.numeric(resMat[,1]))


  # dir.path = file.path(paste("/home/amoongat/Projects/Rats-Credit/Sources/logs",ratName, sep = "/")) 
  
  # cl <- startMPIcluster(count=count,verbose=TRUE, logdir = dir.path)
  # setRngDoMPI(cl, seed=count)
  # exportDoMPI(cl, c("src.dir","model.data.dir","model.src"),envir=environment())
  # registerDoMPI(cl)
  
  #  initWorkers <-  function() {
  #      source(paste(src.dir, "ModelClasses.R", sep = "/"))
  #      source(paste(model.src, "PathModel.R", sep = "/"))
  #      source(paste(model.src, "TurnModel.R", sep = "/"))
  #      source(paste(model.src, "HybridModel1.R", sep = "/"))
  #      source(paste(model.src, "HybridModel2.R", sep = "/"))
  #      source(paste(model.src, "HybridModel3.R", sep = "/"))
  #      source(paste(model.src, "HybridModel4.R", sep = "/"))
  #      source(paste(src.dir, "BaseClasses.R", sep = "/"))
  #      source(paste(src.dir,"exportFunctions.R", sep="/"))
   
  #      #attach(myEnv, name="sourced_scripts")
  #    }


  # chunkSize = ceiling(length(models)*length(iter)/getDoParWorkers())
  # opts <- list(initEnvir=initWorkers,chunkSize=chunkSize) 
   
  # minDfModels <- foreach(model = models, .inorder=TRUE, .options.mpi=opts) %:% 
  #   foreach(it = iter, .inorder=TRUE) %dopar%
  #   {
  #     print(sprintf("it=%i,model=%s",it,model))
  #     df_it <- df[which(df[,1]==it & df[,2]==model),]
  #     min_lik1 = 1000000
  #     minmodel <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
  #     modelData <- new("ModelData", Model = model, creditAssignment = "qlearningAvgRwd", sim = 2)
  #     for(idx in 1:length(df_it[,1]))
  #     {
  #       modelData@alpha = df_it[idx,3]
  #       modelData@gamma1 = df_it[idx,4]
        
  #       argList <- getArgList(modelData, ratdata)
  #       lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, argList[[6]], sim=2)
  #       lik1 <- sum(lik[c(1:it)])*-1
  #       lik2 <- sum(lik[-c(1:800)])*-1
  #       df_it[idx,7]= lik1
        
  #       if (is.infinite(lik1)) {
  #         #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
  #         lik1= 1000000
  #         next
  #       }else if (is.nan(lik1)) {
  #         #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
  #         lik1 = 1000000
  #         next
  #       }else if (is.na(lik1)) {
  #         #cat(sprintf("Alpha = %f, Gamma1=%f, lik=%f", modelData@alpha,modelData@gamma1, lik1))
  #         lik1 = 1000000
  #         next
  #       }
  #       if(lik1 < min_lik1)
  #       {
  #         min_lik1=lik1
  #         min_lik2=lik2
  #         minmodel@alpha = df_it[idx,3]
  #         minmodel@gamma1 = df_it[idx,4]
  #       }    
  #     }
  #     #cat(sprintf("it=%i,model=%s, min_lik1=%i",it,model,min_lik1))
  #     c(model,it,minmodel@alpha,minmodel@gamma1,minmodel@gamma2,minmodel@lambda,min_lik1,min_lik2)
      
  #   }
  # minDflist <- unlist(minDfModels, recursive = FALSE)
  # minDfModels <- Reduce(rbind,minDflist)
  # print(minDfModels)
  # save(minDfModels, file = paste0(model.data.dir,"/",ratdata@rat, format(Sys.time(),'_%Y%m%d_%H%M%S'),"_minDfModels.Rdata")) 


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