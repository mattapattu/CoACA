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

testData = new("TestModels", Name = "AvgRwd",Models=c("Paths.qlearningAvgRwd","Hybrid1.qlearningAvgRwd","Hybrid2.qlearningAvgRwd","Hybrid3.qlearningAvgRwd","Hybrid4.qlearningAvgRwd","Turns.qlearningAvgRwd"))


rawData <- donnees_ash[[rat]] 
enregres = enregCombine(rawData,rats[rat])
allpaths = enregres$allpaths
boxTimes = enregres$boxTimes
    
ratdata = populateRatModel(allpaths=allpaths,rat=rats[rat],donnees_ash[[rat]],TurnModel)

############### Tests #############################################



if(currentTest == "computeModelParams")
{
  alpha_seq = seq_log(1e-3, 0.9,60)
  gamma1_seq = seq_log(1e-8, 1e-4, 10)
  iters=c(seq(from = 0, to = length(allpaths[,1]), by = 400)[-1],length(allpaths[,1]))
  models = testData@Models
  gridMat<- expand.grid(alpha_seq,gamma1_seq,iters,models,stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=11)    
}

################# Test : Param estimation test ######################


if(currentTest == "paramEstTest")
{
  n = 8
  sessions<-unique(ratdata@allpaths[,5])
  session_grps<-split(sessions, sort(sessions%%8))
  maxVecs <- c()
  for(grp in c(1:n))
  {
   print(grp)
   begin_ses <- min(session_grps[[grp]])
   end_ses <- max(session_grps[[grp]])
   indices_of_ses <- which(ratdata@allpaths[,5]>=begin_ses & ratdata@allpaths[,5] <=end_ses)
   maxVecs <- c(maxVecs,max(indices_of_ses))
  }

  alpha_seq = seq_log(1e-3, 0.9,60)
  gamma1_seq = seq_log(1e-8, 1e-4, 10)
  gridMat<- expand.grid(alpha_seq,gamma1_seq,maxVecs,c(1:40),stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=21)
}


################# Test : validate holdout ######################

if(currentTest == "paramEstTest")
{
  alpha_seq = seq_log(1e-3, 0.9,60)
  gamma1_seq = seq_log(1e-8, 1e-4, 10)
  models = testData@Models
  gridMat<- expand.grid(alpha_seq,gamma1_seq,models,c(1:100),stringsAsFactors = FALSE)
  sequences = seq(0,length(gridMat[,1]), length.out=21)

}