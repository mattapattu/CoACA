
library(doParallel)


#options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
print(args)

select_rat <- as.integer(args[1])
seed <- as.numeric(args[2])
count <- as.integer(args[3])
#options(error=recover)

rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114","robert")
names=c('e','f','g','c','d','h','i','j','a','b','k')

### Options Linux/Windows ####

#src.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/src")
src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")
#src.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/src")

setup.hpc = TRUE

############### Select tests to run

unitTest1 = FALSE

computeModelParams = F

computeModelLik = F
loadAllModelRes = F
modelSelection = F

plotProb = F
plotLik = F

unitTest2 = FALSE

validateHoldout = F

paramEstTest = T 
thetaHatTest = F 
pcaPlot = FALSE

successPlot = FALSE
ratSpeedPlot = FALSE

############### TEST EXECUTIONS ######################################



if(unitTest1)
{
  source(paste(src.dir,"unitTestaca3.R", sep="/"))
  data.path = file.path("C:/Rats-Credits/Data/testDonnes1.RData") 
}else
{
  #data.path = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Data/new_data_journeys.Rdata")
  #data.path = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Data/data_journeys.Rdata")
  data.path = file.path("/home/amoongat/Projects/Rats-Credit/Data/new_data_journeys.Rdata")
  
}

load(data.path)
#load(data.path2)

plot.dir = file.path("/home/amoongat/Projects/Rats-Credit/Plots")
model.data.dir = file.path("/home/amoongat/Projects/Rats-Credit/Data/Rat_Model_Data")

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
for (i in c(select_rat)) {
  
  testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca2"))
   #testData = new("TestModels", Models=c("Paths"), creditAssignment=c("aca2"))
  
  
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
  # 
  #ratDataList[[i]] = ratdata
 
######### Estimate model params at interval of 200 trials ################# 

  if(computeModelParams)
  {
   getModelParams(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count)
  }

 
  ############ New validation tests ##########################

   if(thetaHatTest)
   {
     #res.dir = file.path("C:/Users/matta/Downloads/thetahat_res")
     #debug(plotThetaHat)
     plotThetaHat(ratdata,model.data.dir,plot.dir)
   }


  if(paramEstTest)
  {
    allmodelRes = readModelParams(ratdata,model.data.dir,testData, sim=2)
    testParamEstimation(ratdata,allmodelRes,testData,model.src,setup.hpc,model.data.dir,seed,count)
    plotSimParamEstimation(ratdata,model.data.dir,plot.dir)

  }  
  ############### Likelihood Computation and Model Selection ###########################################
   
  
  if(computeModelLik)
  {
    #debug(getModelResults)
    allmodelRes = getModelResults(ratdata,testData,sim=2,src.dir, model.src, setup.hpc,count)
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
    setwd(plot.dir)
    #debug(generatePlots)
    generatePlots(ratdata,allmodelRes,window=20,plot.dir)
    
    #debug(generateEmpiricalPlots)
    #generateEmpiricalPlots(ratdata,window=20)
    #plotTurnProb(ratdata,allmodelRes,Hybrid3)
    
  }
  
  if(plotLik)
  {
    setwd(plot.dir)
    #debug(generateLikelihoodPlots)
    generateLikelihoodPlots(ratdata,allmodelRes,testData,plot.dir)
    
    #debug(generateEmpiricalPlots)
    #generateEmpiricalPlots(ratdata,window=20)
    #plotTurnProb(ratdata,allmodelRes,Hybrid3)
    
  }
  
  
  # #### Holdout Validation ########################################
  
 
  
  if(unitTest2)
  {
    #debug(unitTestHoldOut)
    unitTestHoldOut(ratdata,allmodelRes,testData,model.src) 
  }
  
  if(validateHoldout)
  {
    #debug(HoldoutTest)
    HoldoutTest(ratdata,allmodelRes,testData,model.src,setup.hpc,model.data.dir,count)
  }
  
  
  
  #### Parameter estimation test ##############
  
  # if(paramEstTest)
  # {
  #   #debug(testParamEstimation)
  #   testParamEstimation(ratdata,allmodelRes,testData,model.src,setup.hpc,model.data.dir)
  #   
  # }
  # 
  # if(thetaHatTest)
  # {
  #   #res.dir = file.path("C:/Users/matta/Downloads/thetahat_res")
  #   #debug(plotThetaHat)
  #   plotThetaHat(ratdata,model.data.dir,plot.dir)
  # }
  # 
  if(pcaPlot)
  {
    #debug(plotPCA)
    plotPCA(ratdata, allmodelRes)
    
  }
  
}

if(successPlot)
{
  #debug(plotSuccessRates)
  plotSuccessRates(ratDataList)
  
}

if(ratSpeedPlot)
{
  #debug(plotRatSpeed)
  plotRatSpeed(ratDataList,donnees_ash,plot.dir)
  
}


print(sprintf("End of script"))
