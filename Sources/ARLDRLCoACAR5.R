suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("bigsnpr"))


options(error=function()traceback(2))

testSuite = "ARLDRLCoACAR5"



#### Tests ##############
# unitTestProbDiff = F


# arl_on_coaca = F
# arl_on_drl = T

# coaca_on_arl = F          
# coaca_on_drl = T

# drl_on_arl = T
# drl_on_coaca = T

# combineRes_on_arl = F
# combineRes_on_drl = F
# combineRes_on_coaca = T


# coaca_on_arl_likVal = T          
# arl_on_coaca_likVal = T


option_list = list(
  make_option(c("-r","--rat"), action="store", default=NA, type="integer"
              ),
  make_option(c("--arl_on_coaca"), action="store_true", default=FALSE,
              ),
  make_option(c("--arl_on_drl"), action="store_true", default=FALSE,
              ),
  make_option(c("--coaca_on_arl"), action="store_true", default=FALSE,
              ),
  make_option(c("--coaca_on_drl"), action="store_true", default=FALSE,
              ),
  make_option(c("--drl_on_arl"), action="store_true", default=FALSE,
              ),
  make_option(c("--drl_on_coaca"), action="store_true", default=FALSE,
              ),
  make_option(c("--combineRes_on_arl"), action="store_true", default=FALSE,
              ),
  make_option(c("--combineRes_on_drl"), action="store_true", default=FALSE,
              ),
  make_option(c("--combineRes_on_coaca"), action="store_true", default=FALSE,
              ),
  make_option(c("--coaca_on_arl_likVal"), action="store_true", default=FALSE,
              ),
  make_option(c("--arl_on_coaca_likVal"), action="store_true", default=FALSE,
              )                                                                                                 
)
opt = parse_args(OptionParser(option_list=option_list))
rat=opt$rat



################## Test 1: Holdout test using CoACA on ARL dataset ################

if(opt$coaca_on_arl)
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
    filename = paste0("ARLCoACA_paramMat_T1_rat",rat)  
    write.table(paramMat, file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
    command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

    cat(command)
    cat("\n")
    system(command)

}


################## Test 1: Holdout test using ARL on CoACA dataset ################


if(opt$arl_on_coaca)
{
   
    currentTest = "arl_on_coaca"
    source("testConfig2.R")
    cores = 10
    walltime = "16:00"
    name = paste0("holdMul","_",paste0("rat",rat))
    stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
    stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

   paramMat <-
    foreach(i = c(1:40), .combine='rbind')%do%
    {
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      seed = start_idx
      spawnslaves = cores-1
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }
    filename = paste0("ARLCoACA_paramMat_T2_rat",rat)  
    write.table(paramMat, file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
    command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)
    cat(command)
    cat("\n")
    system(command)

}

############# Test ####################################################

if(opt$combineRes_on_arl)
{
  currentTest = "combineRes_on_arl"
  source("testConfig2.R")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("combineRes_on_arl_",paste0("rat",rat))
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
  filename = paste0("ARLDRLCoACA_paramMat_T3_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  cat(command)
  cat("\n")
  system(command)
}

###############

if(opt$combineRes_on_drl)
{
  currentTest = "combineRes_on_drl"
  source("testConfig2.R")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("combineRes_on_drl_",paste0("rat",rat))
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
  filename = paste0("ARLDRLCoACA_paramMat_T3_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  cat(command)
  cat("\n")
  system(command)
}


###############
if(opt$combineRes_on_coaca)
{
  currentTest = "combineRes_on_coaca"
  source("testConfig2.R")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("combineRes_on_coaca_",paste0("rat",rat))
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
  filename = paste0("ARLDRLCoACA_paramMat_T4_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  cat(command)
  cat("\n")
  system(command)
}


##########################

if(opt$coaca_on_arl_likVal)
{
  currentTest = "coaca_on_arl_likVal"
  source("testConfig2.R")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("coaca_on_arl_likVal_",paste0("rat",rat))
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
  filename = paste0("ARLCoACA_paramMat_T5_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  cat(command)
  cat("\n")
  system(command)
}


############################


if(opt$arl_on_coaca_likVal)
{
  currentTest = "arl_on_coaca_likVal"
  source("testConfig2.R")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  start_idx = 0
  end_idx = 0
  seed = 0
  name = paste0("arl_on_coaca_likVal_",paste0("rat",rat))
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
  filename = paste0("ARLCoACA_paramMat_T6_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  cat(command)
  cat("\n")
  system(command)
}
