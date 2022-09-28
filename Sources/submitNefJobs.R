library(bigsnpr)
args <- commandArgs(trailingOnly = TRUE)
print(args)


rat <- as.integer(args[1])
#options(error=recover)
options(error=function()traceback(2))



################ INIT ####################################################################
rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114","robert")
names=c('e','f','g','c','d','h','i','j','a','b','k')

### Options Linux/Windows ####

src.dir = file.path("/home/amoongat/Projects/Rats-Credit/Sources/src")

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
source(paste(src.dir,"ValidationFunc2.R", sep="/"))
source(paste(src.dir,"../PathModels/utils.R", sep="/"))



data.path = file.path("/home/amoongat/Projects/Rats-Credit/Data/new_data_journeys.Rdata")
load(data.path)
plot.dir = file.path("/home/amoongat/Projects/Rats-Credit/Plots")
model.data.dir = file.path("/home/amoongat/Projects/Rats-Credit/Data/Rat_Model_Data")

rawData <- donnees_ash[[rat]]
enregres = enregCombine(rawData,rats[i])
allpaths = enregres$allpaths
boxTimes = enregres$boxTimes
ratdata = populateRatModel(allpaths=allpaths,rat=rats[rat],donnees_ash[[rat]],TurnModel)
  
############## END INIT ##############################################  

testData = new("TestModels", Name = "AvgRwd",Models=c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd"))

#### Tests ##############
computeModelParams = F
generateModelParamMat = T

########################## Test 1: computeModelParams 

if(computeModelParams){
  alpha_seq = seq_log(1e-3, 0.9,60)
  gamma1_seq = seq_log(1e-8, 1e-4, 10)
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
  models = testData@Models
  gridMat<- expand.grid(alpha_seq,gamma1_seq,iters,models,stringsAsFactors = FALSE)

    # each slave node ~ 400 iterations

  sequences = seq(0,length(gridMat[,1]), length.out=10)
  cores = 10
  walltime = "7:00"

  for(i in c(1:9))
  {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
      #name = paste0("modelParams_",i,"_",rats[[rat]])
      name = paste0("mParams",i,"_",paste0("rat",rat))
      stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
      stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")
      currentTest = "computeModelParams"

      command <- sprintf("oarsub -t besteffort -t idempotent -l core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
      cat(command)
      cat("\n")
      system(command)
  }
}

################### Test 2: generateModelParamMat #################333


if(generateModelParamMat){
  cores = 5
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("genPMat_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")
  currentTest = "generateModelParamMat"

  command <- sprintf("oarsub -t besteffort -t idempotent -l core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)

}

################## Test 3: ################
