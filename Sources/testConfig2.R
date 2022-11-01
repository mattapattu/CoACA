rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114","robert")
names=c('e','f','g','c','d','h','i','j','a','b','k')

src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")

data.path = file.path("/home/amoongat/Projects/Rats-Credit/Data/new_data_journeys.Rdata")
load(data.path)

plot.dir = file.path("/home/amoongat/Projects/Rats-Credit/Plots")

data.dir = file.path("/home/amoongat/Projects/Rats-Credit/Data/Rat_Model_Data")
dir.create(file.path(data.dir, testSuite), showWarnings = FALSE)
model.data.dir=file.path(data.dir, testSuite)
dir.create(file.path(model.data.dir, "rat_106"), showWarnings = FALSE)
dir.create(file.path(model.data.dir, "rat_112"), showWarnings = FALSE)
dir.create(file.path(model.data.dir, "rat_113"), showWarnings = FALSE)
dir.create(file.path(model.data.dir, "rat_114"), showWarnings = FALSE)
#model.data.dir = file.path("/home/amoongat/Projects/Rats-Credit/Data/Rat_Model_Data/qlearning")

model = "Model2"  ## {Model1,Model2,Model3}
source(paste(src.dir,"ModelClasses.R", sep="/"))

setup.hpc = TRUE

if(testSuite=="ARLTestSuite")
{
  gamma2_Global <<- 0.5
  lambda_Global <<- 0

}else if(testSuite=="CoACAR1"){
  gamma2_Global <<- 0
  lambda_Global <<- 0
}else if(testSuite=="CoACAR5"){
  gamma2_Global <<- 0.5
  lambda_Global <<- 0
}else if(testSuite=="ARLCoACA"){
  if(currentTest=="coaca_on_arl")
  {
      gamma2_Global <<- 0
      lambda_Global <<- 0

  }else if(currentTest=="arl_on_coaca"){
      gamma2_Global <<- 0.5
      lambda_Global <<- 0

  }
}

print(sprintf("setting gamma2=%f, lambda=%f", gamma2_Global, lambda_Global))

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
source(paste(src.dir,"ModelUpdateFunc2.R", sep="/"))
#source(paste(src.dir,"ValidationFunc.R", sep="/"))
#source(paste(src.dir,"ValidationFunc2.R", sep="/"))
source(paste(src.dir,"ValidationFunc4.R", sep="/"))
source(paste(src.dir,"../PathModels/utils.R", sep="/"))


if(testSuite=="ARLTestSuite")
{
  testModels = c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd")
}else if(testSuite=="CoACAR1"){
  testModels = c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2")
}else if(testSuite=="CoACAR5"){
  testModels = c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2")
}else if(testSuite=="ARLCoACA"){
  if(currentTest=="coaca_on_arl")
  {
    testModels = c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2")

  }else if(currentTest=="arl_on_coaca"){
    testModels = c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd")
  }
}
testData = new("TestModels", Name = testSuite,Models=testModels)


rawData <- donnees_ash[[rat]] 
enregres = enregCombine(rawData,rats[rat])
allpaths = enregres$allpaths
boxTimes = enregres$boxTimes
    
ratdata = populateRatModel(allpaths=allpaths,rat=rats[rat],donnees_ash[[rat]],TurnModel)


### Define test variables
if(testSuite=="ARLTestSuite")
{
  alpha_seq = seq_log(1e-3, 0.1,20)
  gamma1_seq = seq_log(1e-8, 1e-4,20)
  initpop <- as.matrix(expand.grid(alpha_seq,gamma1_seq,stringsAsFactors = FALSE))

}else if(testSuite=="CoACAR1"){

  alpha_seq = seq_log(0.01, 0.9,5)
  gamma1_seq = seq_log(0.01, 0.9,5)
  initpop <- as.matrix(expand.grid(alpha_seq,gamma1_seq,stringsAsFactors = FALSE))

}else if(testSuite=="CoACAR5"){

  alpha_seq = seq_log(0.01, 0.9,5)
  gamma1_seq = seq_log(0.01, 0.9,5)
  initpop <- as.matrix(expand.grid(alpha_seq,gamma1_seq,stringsAsFactors = FALSE))

}else if(testSuite=="ARLCoACA"){

  ratName = ratdata@rat
  if(currentTest=="coaca_on_arl")
  {
    alpha_seq = seq_log(0.01, 0.9,5)
    gamma1_seq = seq_log(0.01, 0.9,5)
    initpop <- as.matrix(expand.grid(alpha_seq,gamma1_seq,stringsAsFactors = FALSE))
    gen.data.dir = file.path(data.dir, "ARL",ratName, "Datasets")

  }else if(currentTest=="arl_on_coaca"){
    alpha_seq = seq_log(1e-3, 0.1,20)
    gamma1_seq = seq_log(1e-8, 1e-4,20)
    initpop <- as.matrix(expand.grid(alpha_seq,gamma1_seq,stringsAsFactors = FALSE))
    gen.data.dir = file.path(data.dir, "CoACA",ratName, "Datasets")
  }
  print(sprintf("gen.data.dir=%s",gen.data.dir))
}

############### Tests #############################################

print(sprintf("currentTest=%s",currentTest))

if(currentTest == "computeModelParams")
{
  #alpha_seq = seq_log(1e-3, 0.9,10)
  #gamma1_seq = seq_log(1e-8, 1e-4, 5)
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 200)[-1],length(allpaths[,1]))
  models = testData@Models
  gridMat<- expand.grid(iters,models,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=7)
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
  gridMat<- expand.grid(genDataList, genData,iters,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=11)
}


################# Test : validate holdout ######################

if(currentTest == "validateHoldout")
{
  models = testData@Models
  genDataList = c(1:10)
  genData = c(1:60)
  gridMat<- expand.grid(genDataList, genData,models,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=21)

}

###################### Multitest holdout

if(currentTest == "coaca_on_arl"||currentTest == "arl_on_coaca")
{
  models = testData@Models
  genDataList = c(1:10)
  genData = c(1:60)
  gridMat<- expand.grid(genDataList, genData,models,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=21)
  
}


