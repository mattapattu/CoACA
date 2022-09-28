args <- commandArgs(trailingOnly = TRUE)
print(args)

select_rat <- as.integer(args[1])
seed <- as.numeric(args[2])
count <- as.integer(args[3])
currentTest <- as.character(args[4])
start_idx <- as.integer(args[5])
end_idx <- as.integer(args[6])
#options(error=recover)
options(error=function()traceback(2))

################ INIT ####################################################################

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
source(paste(src.dir,"../PathModels/utils.R", sep="/"))

rawData <- donnees_ash[[select_rat]] 
enregres = enregCombine(rawData,rats[select_rat])
allpaths = enregres$allpaths
boxTimes = enregres$boxTimes
    
ratdata = populateRatModel(allpaths=allpaths,rat=rats[select_rat],donnees_ash[[select_rat]],TurnModel)

############## END INIT ##############################################  

#testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca2"))
#testData = new("TestModels", Name = "Aca2",Models=c("Paths.aca2","Hybrid1.aca2","Hybrid2.aca2","Hybrid3.aca2","Hybrid4.aca2","Turns.aca2"))
# testData = new("TestModels", Name = "Aca4",Models=c("Paths.aca4","Hybrid1.aca4","Hybrid2.aca4","Hybrid3.aca4","Hybrid4.aca4","Turns.aca4"))
testData = new("TestModels", Name = "AvgRwd",Models=c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd"))
#testData = new("TestModels", Name = "acaAndQLearning",Models=c("Hybrid3.aca2","Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd"))
#testData = new("TestModels", Models=c("Paths"), creditAssignment=c("aca2"))


############### Tests #############################################



if(currentTest == "computeModelParams")
{
  alpha_seq = seq_log(1e-3, 0.9,60)
  gamma1_seq = seq_log(1e-8, 1e-4, 10)
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
  models = testData@Models
  gridMat<- expand.grid(alpha_seq,gamma1_seq,iters,models,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=10)
  gridMat <- gridMat[start_idx:end_idx,]
  print(sprintf("start_idx=%i,end_idx=%i",start_idx,end_idx))
    
  seq_id <- which((sequences+1) %in% start_idx)
  name = paste0("mParams",seq_id,"_",paste0("rat",select_rat))
  #analyzeParamSpaceWrapper(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, gridMat)
  analyzeParamSpace(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name)
  #getModelParams(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count)
}

################### Test 2: generateModelParamMat #################333


if(currentTest == "generateModelParamMat"){
  generateParamResMat(ratdata,model.data.dir,count)
} 
  
  




print(sprintf("End of script"))
