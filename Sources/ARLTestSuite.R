library(bigsnpr)
args <- commandArgs(trailingOnly = TRUE)
print(args)


rat <- as.integer(args[1])
#options(error=recover)
options(error=function()traceback(2))


#### Tests ##############
unitTestProbDiff = F


computeModelParams = T
generateModelParamMat = F
generateDataset = F
paramEstTest = F
combineParamEstResLists = F
validateHoldout = F

########################## Test 1: computeModelParams  ########################

if(isTRUE(computeModelParams)){

  currentTest = "computeModelParams"
  source("testConfig2.R")

  cores = 10
  walltime = "10:00"
  name = paste0("mParams","_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1:10), .combine='rbind')
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
  write.table(paramMat, file="ARL_paramMat_T1.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T1.txt",cores, walltime,name,stdout,stderr)
  cat(command)
  cat("\n")
  system(command)
}

################### Test 2: generateModelParamMat #################333

name = paste0("genPMat_",paste0("rat",rat))
stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

if(isTRUE(generateModelParamMat)){

  currentTest = "generateModelParamMat"
  source("testConfig2.R")

  paramMat <-
    foreach(i = c(1), .combine='rbind')
    {
      cores = 10
      walltime = "1:00"
      spawnslaves = cores-1
      start_idx = 0
      end_idx = 0
      seed = 0
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
  write.table(paramMat, file="ARL_paramMat_T2.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T2.txt",cores, walltime,name,stdout,stderr)

  #command <- sprintf("oarsub -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
  cat(command)
  cat("\n")
  system(command)
  #generateParamResMat(ratdata,model.data.dir,count)
}

############## Unit Test ###############################
if(isTRUE(unitTestProbDiff))
{
  currentTest = "unitTestProbDiff"
  source("testConfig2.R")

  cores = 7
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("unitTest_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  command <- sprintf("oarsub -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
  cat(command)
  cat("\n")
  system(command)
}
  



############## Generate Datasets ##########################

if(isTRUE(generateDataset))
{
  currentTest = "generateDataset"
  source("testConfig2.R")
  cores = 15
  walltime = "12:00"

  name = paste0("GenData","_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")  

   paramMat <-
    foreach(i = c(1:10), .combine='rbind')
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
  write.table(paramMat, file="ARL_paramMat_T3.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T3.txt",cores, walltime,name,stdout,stderr)
  cat(command)
  cat("\n")
  system(command)

}




################## Test 3: Estimate params on artificial data ################

if(isTRUE(paramEstTest))
{
    #allmodelRes = readModelParams(ratdata,model.data.dir,testData, sim=2)
    #testParamEstimation(ratdata,allmodelRes,testData,model.src,setup.hpc,model.data.dir,seed,count)
    
  currentTest = "paramEstTest"
  source("testConfig2.R")
  cores = 10
  walltime = "14:00"
  name = paste0("paramEs","_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

   paramMat <-
    foreach(i = c(1:10), .combine='rbind')
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
  write.table(paramMat, file="ARL_paramMat_T4.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T4.txt",cores, walltime,name,stdout,stderr)
  cat(command)
  cat("\n")
  system(command)
}  

################# paramEstTest: combineParamEstResLists #########################
#print(sprintf("combineParamEstResLists=%s, is",toString(combineParamEstResLists)))
print(is.logical(combineParamEstResLists))
if(isTRUE(combineParamEstResLists))
{
  currentTest = "combineParamEstResLists"
  source("testConfig.R")

  cores = 10
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("combineParamEstResLists_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1), .combine='rbind')
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
  write.table(paramMat, file="ARL_paramMat_T5.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T5.txt",cores, walltime,name,stdout,stderr)
  cat(command)
  cat("\n")
  system(command)
}  

################## Test 4: Holdout test on artificial data ################

if(isTRUE(validateHoldout))
{
   
    currentTest = "validateHoldout"
    source("testConfig2.R")
    cores = 10
    walltime = "14:00"
    name = paste0("holdVal",i,"_",paste0("rat",rat))
    stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
    stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

   paramMat <-
    foreach(i = c(1:20), .combine='rbind')
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
    write.table(paramMat, file="ARL_paramMat_T6.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

    command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T6.txt",cores, walltime,name,stdout,stderr)
    cat(command)
    cat("\n")
    system(command)

}

############# Test ####################################################

if(isTRUE(combineHoldoutResLists))
{
  currentTest = "combineHoldoutResLists"
  source("testConfig2.R")
  cores = 10
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("combineHoldoutResLists_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1), .combine='rbind')
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx)  
    }
  write.table(paramMat, file="ARL_paramMat_T6.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarctl sub --array-param-file param_file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T6.txt",cores, walltime,name,stdout,stderr)
  cat(command)
  cat("\n")
  system(command)
}

