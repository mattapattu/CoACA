rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114","robert")
names=c('e','f','g','c','d','h','i','j','a','b','k')

src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")

data.path = file.path("/home/amoongat/Projects/Rats-Credit/Data/new_data_journeys.Rdata")
load(data.path)

plot.dir = file.path("/home/amoongat/Projects/Rats-Credit/Plots")
model.data.dir = file.path("/home/amoongat/Projects/Rats-Credit/Data/Rat_Model_Data")
#model.data.dir = file.path("/home/amoongat/Projects/Rats-Credit/Data/Rat_Model_Data/qlearning")

model = "Model2"  ## {Model1,Model2,Model3}
source(paste(src.dir,"ModelClasses.R", sep="/"))

setup.hpc = TRUE


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
source(paste(src.dir,"ValidationFunc2.R", sep="/"))
source(paste(src.dir,"ValidationFunc3.R", sep="/"))
source(paste(src.dir,"../PathModels/utils.R", sep="/"))

testData = new("TestModels", Name = "AvgRwd",Models=c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd"))


rawData <- donnees_ash[[rat]] 
enregres = enregCombine(rawData,rats[rat])
allpaths = enregres$allpaths
boxTimes = enregres$boxTimes
    
ratdata = populateRatModel(allpaths=allpaths,rat=rats[rat],donnees_ash[[rat]],TurnModel)

alpha_seq = seq_log(1e-3, 0.1,6)
gamma1_seq = seq_log(1e-8, 1e-4, 6)
initpop <- as.matrix(expand.grid(alpha_seq,gamma1_seq,stringsAsFactors = FALSE))

############### Tests #############################################



if(currentTest == "computeModelParams")
{
  #alpha_seq = seq_log(1e-3, 0.9,10)
  #gamma1_seq = seq_log(1e-8, 1e-4, 5)
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
  models = testData@Models
  gridMat<- expand.grid(iters,models,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=2)
  print(sequences)  
  print(sprintf("gridMat len=%i",length(gridMat[,1])))
  
}

############## Generate Datasets ##########################

if(currentTest == "generateDataset")
{
  models = testData@Models
  gridMat<- expand.grid(models,c(1:100),stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=11)
}




################# Test : Param estimation test ######################


if(currentTest == "paramEstTest")
{
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
  genDataList = c(1:4)
  genData = c(1:60)
  gridMat<- expand.grid(iters,genDataList, genData,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=21)
}


################# Test : validate holdout ######################

if(currentTest == "validateHoldout")
{
  models = testData@Models
  genDataList = c(1:10)
  genData = c(1:60)
  gridMat<- expand.grid(models,genDataList, genData,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=11)

}