
library(doParallel)

options(error=recover)

rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114","robert")
names=c('e','f','g','c','d','h','i','j','a','b','k')

### Options Linux/Windows ####

#src.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/src")
#src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")
src.dir = file.path("C:/Users/matta/OneDrive/Documents/Projects/Rats-Credit/Sources/src")

setup.hpc = FALSE
#setup.hpc = TRUE

############### Select tests to run

unitTest1 = FALSE
unitTest2 = FALSE
unitTest3 = F

computeModelParams = F

computeModelLik = F
loadAllModelRes = F
modelSelection = F

plotProb = F
plotLik = F

validateHoldout = FALSE

printHoldoutRes = F
paramEstTest = T
analyzeParams = F
thetaHatTest = F
pcaPlot = F
dumpModelParams = F

successPlot = F
ratSpeedPlot = FALSE

############### TEST EXECUTIONS ######################################



if(unitTest1)
{
  source(paste(src.dir,"unitTestaca3.R", sep="/"))
  data.path = file.path("C:/Rats-Credits/Data/testDonnes1.RData") 
}else
{
  data.path = file.path("C:/Users/matta/OneDrive/Documents/Projects/Rats-Credit/Data/new_data_journeys.Rdata")
  #data.path = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Data/data_journeys.Rdata")
  #data.path = file.path("/home/amoongat/Projects/Rats-Credit/data_journeys.Rdata")
  
}

load(data.path)
#load(data.path2)

plot.dir = file.path("C:/Users/matta/OneDrive/Documents/Projects/Rats-Credit/Plots")
#model.data.dir = file.path("C:/Projects/Rats-Credits/Data")
model.data.dir = file.path("C:/Projects/Rats-Credits/Data/qlearning")
#plot.dir = file.path("/home/ajames/Rats-Credit")

model = "Model2"  ## {Model1,Model2,Model3}
source(paste(src.dir,"ModelClasses.R", sep="/"))

## Model files
model.src = paste(src.dir,model, sep="/")
source(paste(model.src,"PathModel.R", sep="/"))
source(paste(model.src,"TurnModel.R", sep="/"))
source(paste(model.src,"HybridModel1.R", sep="/"))
source(paste(model.src,"HybridModel2.R", sep="/"))
source(paste(model.src,"HybridModel2.R", sep="/"))
source(paste(model.src,"HybridModel3.R", sep="/"))
source(paste(model.src,"HybridModel4.R", sep="/"))


source(paste(src.dir,"BaseClasses.R", sep="/"))
source(paste(src.dir,"ModelUpdateFunc.R", sep="/"))
source(paste(src.dir,"ValidationFunc.R", sep="/"))
source(paste(src.dir,"../PathModels/utils.R", sep="/"))

### Loop through the enreg of all 6 rats
ratDataList = list()
allmodelResList <- list()
dfThetahat<- data.frame(rowEnd=integer(),
                        alpha=double(),
                        gamma1=double(),
                        model=character(),
                        rat=character(),
                        stringsAsFactors=FALSE)

dfParamEstTest<- data.frame(rowEnd=integer(),
                            alphaDiff=double(),
                            gammaDiff=double(),
                            model=character(),
                            rat=character(),
                            stringsAsFactors=FALSE)

dfPCA<- data.frame(PC1=double(),
                   PC2=double(),
                   Model=character(),
                   PathNb =character(),
                   TextVec=integer(),
                   Rat=character(),
                   stringsAsFactors=FALSE)

dfProbBoxPlot <- data.frame(Path1.S1=double(),
                            Path1.S1=double(),
                            Model=character(),
                            PathNb =character(),
                            TextVec=integer(),
                            Rat=character(),
                            stringsAsFactors=FALSE)
avgRwd_gamma2= 0.3

for (i in c(4)) {
  
  #testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca2"))
  testData = new("TestModels", Models=c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd"))
  #testData = new("TestModels", Models=c("Paths.aca4","Hybrid1.aca4","Hybrid2.aca4","Hybrid3.aca4","Hybrid4.aca4","Turns.aca4"))
  
  #model = "Model2"
  
  
  if(unitTest1)
  {
    rawData <- testDonnes1
  }
  else
  {
    rawData <- donnees_ash[[i]]
  }
  
  enregres = enregCombine(rawData,rats[i])
  allpaths = enregres$allpaths
  boxTimes = enregres$boxTimes
  
  
  if(unitTest1)
  {
    ratdata = populateRatModel(allpaths=allpaths,rat="testRat",rawData,TurnModel)
    debug(testCode)
    testCode(ratdata)
  }
  else
  {
    ratdata = populateRatModel(allpaths=allpaths,rat=rats[i],donnees_ash[[i]],TurnModel)
    #load(paste0("C:/Users/matta/Downloads/rat_112_allmodelRes.Rdata"))
    #load(paste0("C:/Rats-Credits/allmodelRes_",rats[i],".RData"))
    #load(paste0("C:/Rats-Credits/aca2_allmodelRes_",rats[i],".RData"))
  }
  
  if(unitTest2)
  {
    #debug(unitTestHoldOut)
    unitTestHoldOut(ratdata,allmodelRes,testData,model.src) 
  }
  
  if(unitTest3)
  {
    source(paste(src.dir,"unitTestaca3.R", sep="/"))
    #debug(unitTestRes)
    unitTestRes(ratdata)
  }
  
  
  
  # 
  ratDataList[[i]] = ratdata
  
  
  
  ############### Likelihood Computation and Model Selection ###########################################
  
  if(printHoldoutRes)
  {
    printMatRes(ratdata,testData,"C:/Users/matta/Downloads")
  }
 
  if(computeModelParams)
  {
    getModelParams(ratdata,testData)
  }
  
  if(analyzeParams)
  {
    debug(analyzeParamSpace)
    #resMat = analyzeParamSpaceWrapper(ratdata,testData,src.dir,model.src)
    resMat = analyzeParamSpace(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir)
    #print(resMat)
    model.data.dir="C:/Users/matta/Downloads"
    debug(generateParamResMat)
    generateParamResMat(ratdata,model.data.dir)
  }
  
  if(computeModelLik)
  {
    debug(getModelResults)
    allmodelRes = getModelResults(ratdata,testData,sim=2,src.dir, model.src, setup.hpc)
    save(allmodelRes,file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
  }
  
  if(loadAllModelRes)
  {
    load(file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
    
  }
  
  if(modelSelection)
  {
    
    min_method = getMinimumLikelihood(ratdata,allmodelRes,testData,sim=2)
    print(sprintf("%s is best model for %s",min_method,rats[i]))
    
    
  }
  
  if(plotProb)
  {
    #setwd(file.path(plot.dir,"New"))
    #debug(generatePlots)
    #dir <- file.path(plot.dir,"New")
    #generatePlots(ratdata,allmodelRes,window=30,dir)
    debug(plotPathProbs2)
    load(file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
    plotPathProbs2(ratdata,allmodelRes,plot.dir)
    
    
    #debug(generateEmpiricalPlots)
    #generateEmpiricalPlots(ratdata,window=20)
    #plotTurnProb(ratdata,allmodelRes,Hybrid3)
    
  }
  
  if(plotLik)
  {
    setwd(plot.dir)
    debug(generateLikelihoodPlots)
    generateLikelihoodPlots(ratdata,allmodelRes,testData,plot.dir)
    
    #debug(generateEmpiricalPlots)
    #generateEmpiricalPlots(ratdata,window=20)
    #plotTurnProb(ratdata,allmodelRes,Hybrid3)
    
  }
  
  
  # #### Holdout Validation ########################################
  
 
  
  
  if(validateHoldout)
  {
    #debug(HoldoutTest)
    HoldoutTest(ratdata,allmodelRes,testData,src.dir,setup.hpc,model.data.dir)
  }
  
  
  
  #### Parameter estimation test ##############
  
  if(paramEstTest)
  {
    model.data.dir="C:/Users/matta/Downloads"
    
    debug(plotSimProbBoxPlots)
    plotSimProbBoxPlots(ratdata,model.data.dir,plot.dir)
    
    #debug(plotSimParamEstimation2)
    #plotSimParamEstimation2(ratdata,testData,model.data.dir,plot.dir)
    
    #debug(readModelParams)
    #allmodelRes = readModelParams(ratdata,model.data.dir,testData, sim=2)
    #debug(readModelParamsNew)
    #allModelRes = readModelParamsNew(ratdata,"C:/Users/matta/Downloads",testData, sim=2)
    
    #debug(testParamEstimation)
    #testParamEstimation(ratdata,testData,src.dir,setup.hpc,model.data.dir,allModelRes)
    
    
    
    rat=ratdata@rat
    setwd(model.data.dir)
    dfData=list.files(".", pattern=paste0(rat,".*ParamEs_Stability_df.Rdata"), full.names=FALSE)
    listDfData <- list()
    for(i in c(1:length(dfData)))
    {
      print(dfData[i])
      load(dfData[i])
      listDfData[[i]] <- df
    }
    dfcombined <- bind_rows(listDfData)
    
    iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
    maxVecs=iters
    
    models <- c("Paths", "Hybrid1", "Hybrid2", "Hybrid3", "Hybrid4", "Turns")
    for(model in models)
    {
      dfModel <- dfcombined[which(dfcombined[,1]==model),]
      nbSims <- length(which(dfModel[,2]==maxVecs[1]))
      print(sprintf("model=%s, nbSims=%i",model,nbSims))
      if(nbSims > 0)
      {
        for(iter in maxVecs)
        {
          rowEnd = iter
          simulation_alphas <- dfModel[which(dfModel[,2]==rowEnd),5]
          simulation_gammas <- dfModel[which(dfModel[,2]==rowEnd),6]
          
          true_random_alphas <- dfModel[which(dfModel[,2]==rowEnd),9]
          true_random_gammas <- dfModel[which(dfModel[,2]==rowEnd),10]
          
          dfParamEstTest <- rbind(dfParamEstTest,cbind(simulation_alphas-true_random_alphas, simulation_gammas-true_random_gammas,rep(iter,length(simulation_alphas)),rep(model,length(simulation_alphas)),rep(rat,length(simulation_alphas)) ))
          
        }
      }
    }
    
  }
  

  if(thetaHatTest)
  {
    #res.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Data/Rat_Model_Data")
    #debug(plotThetaHat)
    #plotThetaHat(ratdata,model.data.dir,plot.dir)
    #debug(plotThetaHat2)
    #plotThetaHat2(ratdata,testData,model.data.dir,plot.dir)
    
    
    # rat=ratdata@rat
    # setwd(model.data.dir)
    # paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
    # load(paramTestData)
    # models <- c("Paths", "Hybrid1", "Hybrid2", "Hybrid3", "Hybrid4", "Turns")
    # 
    # for(i in c(1:6))
    # {
    #   rowEnd = paramTest[[i]][[1]][,1]
    #   alpha = paramTest[[i]][[1]][,2]
    #   gamma1 = paramTest[[i]][[1]][,3]
    #   #gamma2 = paramTest[[i]]$model@gamma2
    #   model = models[i]
    #   dfThetahat<-rbind(dfThetahat,cbind(rowEnd,alpha,gamma1,model,rep(rat,length(model))))
    # }
    
    rat=ratdata@rat
    #setwd(model.data.dir)
    setwd("C:/Users/matta/Downloads")
    #paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
    #load(paramTestData)
    iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
    
    minDf <- list.files(".", pattern=paste0(ratdata@rat,".*minDfModels.Rdata"), full.names=FALSE)[1]
    minDfrat<-get(load(minDf))
    minDfrat.df <- as.data.frame(minDfrat)
    cols.num <- c(2,3,4,5,6,7,8)
    minDfrat.df[,cols.num] <- lapply(cols.num,function(x) as.numeric(minDfrat.df[[x]]))
    
    dfThetahat<-rbind(dfThetahat,cbind(minDfrat.df[,c(2,3,4,1)],rep(rat,length(iters))))
    
  }
  
  if(pcaPlot)
  {
    #load(file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
    #debug(plotPCA7)
    #plotPCA7(ratdata,allmodelRes,model.data.dir)
    
    #debug(plotPCA3a)
    #plotPCA3a(ratdata,"C:/Users/matta/Downloads",allmodelRes,model.data.dir)
    #debug(testPCA)
    #testPCA(ratdata,"C:/Users/matta/Downloads",plot.dir)
    
    rat=ratdata@rat
    setwd(model.data.dir)
    dfData=list.files(".", pattern=paste0(rat,".*resList.Rdata"), full.names=FALSE)
    combinedResList <- list()
    for(i in c(1))
    {
      print(dfData[i])
      load(dfData[i])
      combinedResList <- append(combinedResList,resList)
    }
    allmodelRes <- new("AllModelRes")
    models.populated = c()
    models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
    modelProbMats <- data.frame(matrix(0,0,16))
    for(k in c(1:length(combinedResList)))
    {
      ##Extracting generator model from resList[[k]][[1]]
      genData = combinedResList[[k]][[1]]$data
      genModel = combinedResList[[k]][[1]]$data@simModel
      ## Find the index of modelData corresponding to generator model, retrieve the result of holdout cut at trial 800
      index = which(models==genModel)
      testModelData = combinedResList[[k]][[index]]$res
      testModelName = testModelData@Model
      #print(sprintf("genModel=%s, k=%i, testModel=%s",genModel,k,testModel))
      if(!testModelName %in% models.populated)
      {
        models.populated = c(models.populated,genModel)
        print(sprintf("testModel=%s",testModelName))
        #modelData <- slot(slot(allmodelRes,"Paths"),"aca2")
        
        
        #probMat_model=testModelData@probMatrix[,c(1:12)]
        testModel=slot(allModels,testModelName)
        probMat_model=TurnsNew::getProbMatrix2(genData, testModelData, testModel, sim=1)
        n=length(probMat_model[,1])
        probMat_model = cbind(probMat_model,c(1:n),rep(testModelName,n),rep(0,n),rep("SimProb",n))
        
        curr_idx = length(modelProbMats[,1])
        modelProbMats <- rbind(modelProbMats, probMat_model)
        idx1 = curr_idx+1
        modelProbMats[idx1,16] = 1
        idx2 = curr_idx+801
        modelProbMats[idx2,16] = 1
        idx3 = length(modelProbMats[,1])
        modelProbMats[idx3,16] = 1
        
      }
      
      if(length(models.populated)==6)
      {
        break
      }
    }
    
    empProbMat <- getEmpProbMat3(ratdata@allpaths,40,2)
    empProbMat <- rbind(empProbMat,c(0,0,0,1,0,0,0,0,0,1,0,0))
    
    cols.num <- c(1:13)
    modelProbMats[,cols.num] <- lapply(cols.num,function(x) as.numeric(modelProbMats[[x]]))
    
    pca = prcomp(modelProbMats[,1:12],scale=T,center = T)
    empPCA = scale(empProbMat[,c(1:12)], pca$center, pca$scale) %*% pca$rotation
    
    textVec <- rep(0,length(empProbMat[,1]))
    textVec[1]=1
    textVec[801]=1
    textVec[length(empProbMat[,1])-1]=1
    df_s1 <- as.data.frame(cbind(pca$x[,1:2], modelProbMats[,15],modelProbMats[,13],modelProbMats[,16],rep(rat,length(pca$x[,1]))))
    df_s1 <- rbind(df_s1,cbind(empPCA[,1:2], rep("Empirical",length(empPCA[,1])),empProbMat[,13],V5=textVec,rep(rat,length(empPCA[,1]))))
    
    cols.num <- c(1,2,4,5)
    df_s1[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s1[[x]]))
    dfPCA=rbind(dfPCA,df_s1)
  }
  
  if(dumpModelParams)
  {
    allmodelRes = readModelParams(ratdata,model.data.dir,testData, sim=2)
    #load(file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
    allmodelResList[[i]] <- allmodelRes
  }
  
}

if(dumpModelParams)
{
  debug(printModelParams)
  printModelParams(testData,allmodelResList)
}


if(successPlot)
{
  setwd(plot.dir)
  debug(plotSuccessRates)
  plotSuccessRates(ratDataList)
  
}

if(ratSpeedPlot)
{
  debug(plotRatSpeed)
  plotRatSpeed(ratDataList,donnees_ash,plot.dir)
  
}

if(thetaHatTest)
{
  debug(plotThetaHat)
  plotThetaHat(ratdata,dfThetahat,plot.dir)
  
}

if(paramEstTest)
{
  debug(plotSimParamEstimation)
  plotSimParamEstimation(dfParamEstTest,plot.dir)
  
}

if(pcaPlot)
{
  debug(plotPCAComb)
  plotPCAComb(dfPCA,plot.dir)
}



print(sprintf("End of script"))
