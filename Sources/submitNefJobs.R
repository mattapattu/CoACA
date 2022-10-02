library(bigsnpr)
args <- commandArgs(trailingOnly = TRUE)
print(args)


rat <- as.integer(args[1])
#options(error=recover)
options(error=function()traceback(2))


#### Tests ##############
computeModelParams = F
generateModelParamMat = F
paramEstTest = T
validateHoldout = F

########################## Test 1: computeModelParams 

if(computeModelParams){

  currentTest = "computeModelParams"
  source("testConfig.R")

  cores = 10
  walltime = "10:00"

  for(i in c(1:10))
  {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
      #name = paste0("modelParams_",i,"_",rats[[rat]])
      name = paste0("mParams",i,"_",paste0("rat",rat))
      stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
      stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")
      

      command <- sprintf("oarsub -t besteffort -t idempotent -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
      cat(command)
      cat("\n")
      system(command)
  }
}

################### Test 2: generateModelParamMat #################333


if(generateModelParamMat){

  currentTest = "generateModelParamMat"
  source("testConfig.R")

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
  cat(command)
  cat("\n")
  system(command)
  #generateParamResMat(ratdata,model.data.dir,count)
}

############## Generate Datasets ##########################

if(generateDataset)
{
  currentTest = "generateDataset"
  source("testConfig.R")
  cores = 10
  walltime = "12:00"

 

   for(i in c(1:10))
   {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
      #name = paste0("modelParams_",i,"_",rats[[rat]])
      name = paste0("GenData",i,"_",paste0("rat",rat))
      stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
      stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

      command <- sprintf("oarsub -t besteffort -t idempotent -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
      cat(command)
      cat("\n")
      system(command)
   }

}




################## Test 3: Estimate params on artificial data ################

if(paramEstTest)
{
    #allmodelRes = readModelParams(ratdata,model.data.dir,testData, sim=2)
    #testParamEstimation(ratdata,allmodelRes,testData,model.src,setup.hpc,model.data.dir,seed,count)
    
  currentTest = "paramEstTest"
  source("testConfig.R")
  cores = 10
  walltime = "12:00"


   for(i in c(1:10))
   {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
      #name = paste0("modelParams_",i,"_",rats[[rat]])
      name = paste0("paramEs",i,"_",paste0("rat",rat))
      stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
      stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

      command <- sprintf("oarsub -t besteffort -t idempotent -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
      cat(command)
      cat("\n")
      system(command)
   }

}  


################## Test 4: Holdout test on artificial data ################

if(validateHoldout)
  {
   
    currentTest = "validateHoldout"
    source("testConfig.R")  
    cores = 10
    walltime = "10:00"

   for(i in c(1:10))
   {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
      #name = paste0("modelParams_",i,"_",rats[[rat]])
      name = paste0("holdVal",i,"_",paste0("rat",rat))
      stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
      stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

      command <- sprintf("oarsub -t besteffort -t idempotent -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
      cat(command)
      cat("\n")
      system(command)
   }


  }

