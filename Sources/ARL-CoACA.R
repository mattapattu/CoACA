library(bigsnpr)
args <- commandArgs(trailingOnly = TRUE)
print(args)


rat <- as.integer(args[1])
#options(error=recover)
options(error=function()traceback(2))

testSuite = "ARLTestSuite"



#### Tests ##############
unitTestProbDiff = F


coaca_on_arl = T          
arl_on_coaca = T
combineHoldoutResLists = T


################## Test 1: Holdout test using CoACA on ARL dataset ################

if(isTRUE(coaca_on_arl))
{
   
    currentTest = "coaca_on_arl"
    source("testConfig2.R")
    cores = 10
    walltime = "16:00"
    name = paste0("multiHold","_",paste0("rat",rat))
    stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
    stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

   paramMat <-
    foreach(i = c(1:20), .combine='rbind')%do%
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }
    write.table(paramMat, file="ARLCoACA_paramMat_T1.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

    command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARLCoACA_paramMat_T1.txt",cores, walltime,name,stdout,stderr)
    cat(command)
    cat("\n")
    system(command)

}

################## Test 1: Holdout test using ARL on CoACA dataset ################
3

if(isTRUE(arl_on_coaca))
{
   
    currentTest = "arl_on_coaca"
    source("testConfig2.R")
    cores = 10
    walltime = "16:00"
    name = paste0("holdMul","_",paste0("rat",rat))
    stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
    stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

   paramMat <-
    foreach(i = c(1:20), .combine='rbind')%do%
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }
    write.table(paramMat, file="ARLCoACA_paramMat_T2.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

    command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARLCoACA_paramMat_T2.txt",cores, walltime,name,stdout,stderr)
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
    foreach(i = c(1), .combine='rbind')%do%
    {
      start_idx = 0
      end_idx = 0
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }
  write.table(t(paramMat), file="ARL_paramMat_T6.txt", row.names=FALSE, col.names=FALSE,quote=FALSE)

  command <- sprintf("oarsub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", "ARL_paramMat_T6.txt",cores, walltime,name,stdout,stderr)
  cat(command)
  cat("\n")
  system(command)
}

