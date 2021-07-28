#library(R.matlab)
#library(SDMTools)
#library(stringr)
#library(eegkit)#Librairie EEG pour l'analyse du LFP
#library("plot3D")
#library(data.tree)
#library(pracma)
#library(sp) #for spatial polygons
#library(doMPI)
#library(Rmpi)
library(doParallel)

rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114")
names=c('e','f','g','c','d','h','i','j','a','b','k')

### Options Linux/Windows ####

src.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/src")
#src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")

setup.hpc = FALSE
#setup.hpc = TRUE

data.path = file.path("C:/Rats-Credits/Data/data_journeys.RData")
#data.path = file.path("/home/amoongat/Projects/Rats-Credit/data_journeys.Rdata")

plot.dir = file.path("C:/Rats-Credits")

options(error=recover)

load(data.path)

source(paste(src.dir,"ModelClasses.R", sep="/"))
source(paste(src.dir,"PathModel.R", sep="/"))
source(paste(src.dir,"TurnModel.R", sep="/"))
source(paste(src.dir,"HybridModel1.R", sep="/"))
source(paste(src.dir,"HybridModel2.R", sep="/"))
source(paste(src.dir,"HybridModel2.R", sep="/"))
source(paste(src.dir,"HybridModel3.R", sep="/"))
source(paste(src.dir,"HybridModel4.R", sep="/"))
source(paste(src.dir,"BaseClasses.R", sep="/"))
source(paste(src.dir,"ModelUpdateFunc.R", sep="/"))
source(paste(src.dir,"ValidationFunc.R", sep="/"))
source(paste(src.dir,"../PathModels/utils.R", sep="/"))

### Loop through the enreg of all 6 rats
ratDataList = list()
for (i in c(3:6)) {

  enregres = enregCombine(donnees_ash[[i]],rats[i])
  allpaths = enregres$allpaths
  boxTimes = enregres$boxTimes
  
  ratdata = populateRatModel(allpaths=allpaths,rat=rats[i],donnees_ash[[i]],TurnModel)
  ratDataList[[i]] = ratdata
  
  testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca3"))
  #testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca3","sarsa"))
  #testData = new("TestModels", Models=c("Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("sarsa"))
  
  #load(paste0("C:/Users/matta/Downloads/rat_112_allmodelRes.Rdata"))
  load(paste0("C:/Rats-Credits/allmodelRes_",rats[i],".RData"))
  #load(paste0("C:/Users/matta/Downloads/allmodelRes_",rats[i],".RData"))
  #debug(getModelResults)
  #allmodelRes = getModelResults(ratdata,testData,sim=2, src.dir, setup.hpc)
  min_method = getMinimumLikelihood(ratdata,allmodelRes,testData,sim=2)
  print(sprintf("%s is best model for %s",min_method,rats[i]))
  
  #save(allmodelRes,file=paste0(plot.dir,"/allmodelRes_",rats[i],".Rdata"))
  #setwd(plot.dir)
  #debug(generatePlots)
  #generatePlots(ratdata,allmodelRes,window=20,plot.dir)
  
  #debug(generateEmpiricalPlots)
  #generateEmpiricalPlots(ratdata,window=20)
  
  
  #plotTurnProb(ratdata,allmodelRes,Hybrid3)
  
  # #### Holdout Validation ########################################
  
  #debug(HoldoutTest)
  #HoldoutTest(ratdata,allmodelRes,testData,src.dir,setup.hpc)
  
  #### Parameter estimation test ##############
  #debug(testParamEstimation)
  #testParamEstimation(ratdata,allmodelRes,testData,src.dir,setup.hpc)
  
  #res.dir = file.path("C:/Users/matta/Downloads/thetahat_res")
  #debug(plotThetaHat)
  #plotThetaHat(ratdata,res.dir,plot.dir)
  #debug(plotPCA)
  #plotPCA(ratdata, allmodelRes)
  
}

#debug(plotSuccessRates)
#plotSuccessRates(ratDataList)

#debug(plotRatSpeed)
#plotRatSpeed(ratDataList,donnees_ash,plot.dir)


print(sprintf("End of script"))
