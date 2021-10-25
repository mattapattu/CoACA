
library(doParallel)

options(error=recover)

rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114","robert")
names=c('e','f','g','c','d','h','i','j','a','b','k')

### Options Linux/Windows ####

#src.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/src")
#src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")
src.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/src")

setup.hpc = FALSE
#setup.hpc = TRUE

unitTest = F

if(unitTest)
{
  source(paste(src.dir,"unitTestaca3.R", sep="/"))
  data.path = file.path("C:/Rats-Credits/Data/testDonnes1.RData") 
}else
{
  data.path = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Data/data_journeys.Rdata")
  #data.path = file.path("/home/amoongat/Projects/Rats-Credit/data_journeys.Rdata")
  
}

load(data.path)
#load(data.path2)

plot.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Plots")
model.data.dir = file.path("C:/Users/matta/OneDrive/Documents/Rats-Credit/Data/Rat_Model_Data")
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
for (i in c(1:2)) {
  
  testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca2"))
  
  
  if(unitTest)
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
  
  
  if(unitTest)
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
  
  ############### Likelihood Computation and Model Selection ###########################################
  #debug(getModelResults)
  #allmodelRes = getModelResults(ratdata,testData,sim=2,src.dir, model.src, setup.hpc)
  #min_method = getMinimumLikelihood(ratdata,allmodelRes,testData,sim=2)
  #print(sprintf("%s is best model for %s",min_method,rats[i]))
  #save(allmodelRes,file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
  #setwd(plot.dir)
  #debug(generatePlots)
  #generatePlots(ratdata,allmodelRes,window=20,plot.dir)
  
  #debug(generateEmpiricalPlots)
  #generateEmpiricalPlots(ratdata,window=20)
  
  
  #plotTurnProb(ratdata,allmodelRes,Hybrid3)
  
  # #### Holdout Validation ########################################
  load(file=paste0(model.data.dir,paste0("/aca2_",model,"_allmodelRes_",rats[i],".Rdata")))
  debug(HoldoutTest)
  src.dir = file.path(src.dir,model)
  HoldoutTest(ratdata,allmodelRes,testData,src.dir,setup.hpc)
  
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
