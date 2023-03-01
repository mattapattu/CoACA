suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("bigsnpr"))


#options(error=recover)
options(error=function()traceback(2))

testSuite = "CogTestARLDRLCoACA"



#### Tests ##############
# unitTestProbDiff = F


# computeModelParams = F
# generateModelParamMat = F
# getMinModel = F
# generateDataset = F
# paramEstTest = F
# combineParamEstResLists = F
# validateHoldout = F
# combineHoldoutResLists = F
# testLikelihoodModelSelection = T

option_list = list(
  make_option(c("-r","--rat"), action="store", default=NA, type="integer"
              ),
  make_option(c("--computeARLCogModelParams"), action="store_true", default=FALSE,
              ),
  make_option(c("--computeDRLCogModelParams"), action="store_true", default=FALSE,
              ),
  make_option(c("--computeCoACACogModelParams"), action="store_true", default=FALSE,
              ),                        
  make_option(c("--generateCogModelParamMat"), action="store_true", default=FALSE,
              ),
  make_option(c("--getCogMinModel"), action="store_true", default=FALSE,
              ),
)
opt = parse_args(OptionParser(option_list=option_list))
rat=opt$rat


########################## Test 1: computeModelParams  ########################

if((opt$computeARLCogModelParams)){
  
  currentTest = "computeARLCogModelParams"
  source("testConfig2.R")
  #print(sprintf("currentTest=%s", currentTest)) 
  cores = 10
  walltime = "10:00"
  name = paste0("mParams","_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1:6), .combine='rbind') %do%
    {
      
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      print(sprintf("start_idx=%d, end_idx=%d", start_idx, end_idx))
      seed = start_idx
      spawnslaves = cores-1
      
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }

  filename = paste0("CogTestARLDRLCoACA_paramMat_T1_rat",rat)  
  write.table(paramMat, file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)


  cat(command)
  cat("\n")
  system(command)
}


##########################################################

if((opt$computeDRLCogModelParams)){
  
  currentTest = "computeDRLCogModelParams"
  source("testConfig2.R")
  #print(sprintf("currentTest=%s", currentTest)) 
  cores = 10
  walltime = "10:00"
  name = paste0("mParams","_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1:6), .combine='rbind') %do%
    {
      
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      print(sprintf("start_idx=%d, end_idx=%d", start_idx, end_idx))
      seed = start_idx
      spawnslaves = cores-1
      
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }

  filename = paste0("CogTestARLDRLCoACA_paramMat_T2_rat",rat)  
  write.table(paramMat, file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)


  cat(command)
  cat("\n")
  system(command)
}

########################################################################

if((opt$computeCoACACogModelParams)){
  
  currentTest = "computeCoACACogModelParams"
  source("testConfig2.R")
  #print(sprintf("currentTest=%s", currentTest)) 
  cores = 10
  walltime = "10:00"
  name = paste0("mParams","_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1:6), .combine='rbind') %do%
    {
      
      start_idx = sequences[i]+1
      end_idx = sequences[i+1]
      print(sprintf("start_idx=%d, end_idx=%d", start_idx, end_idx))
      seed = start_idx
      spawnslaves = cores-1
      
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }

  filename = paste0("CogTestARLDRLCoACA_paramMat_T3_rat",rat)  
  write.table(paramMat, file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)


  cat(command)
  cat("\n")
  system(command)
}

################### Test 2: generateModelParamMat #################333


if((opt$generateCogModelParamMat)){

  currentTest = "generateCogModelParamMat"
  source("testConfig2.R")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  name = paste0("genPMat_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")

  paramMat <-
    foreach(i = c(1), .combine='rbind')%do%
    {
      
      start_idx = 0
      end_idx = 0
      seed = 0
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }
  filename = paste0("CogTestARLDRLCoACA_paramMat_T4_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  #command <- sprintf("oarsub -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
  cat(command)
  cat("\n")
  system(command)
  #generateParamResMat(ratdata,model.data.dir,count)
}

###################### getMin model ##############################

if((opt$getCogMinModel)){
  currentTest = "getCogMinModel"
  source("testConfig2.R")
  name = paste0("minLik_",paste0("rat",rat))
  stdout = paste0("\'logs/",name,"_%jobid%.stdout\'")
  stderr = paste0("\'logs/",name,"_%jobid%.stderr\'")
  cores = 2
  walltime = "1:00"
  spawnslaves = cores-1
  paramMat <-
    foreach(i = c(1), .combine='rbind')%do%
    {
      
      start_idx = 0
      end_idx = 0
      seed = 0
        #name = paste0("modelParams_",i,"_",rats[[rat]])
      c(rat,seed,spawnslaves,currentTest, start_idx, end_idx, testSuite)  
    }
  filename = paste0("CogTestARLDRLCoACA_paramMat_T5_rat",rat)  
  write.table(t(paramMat), file=filename, row.names=FALSE, col.names=FALSE,quote=FALSE)
  command <- sprintf("oarctl sub --array-param-file %s -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l /nodes=1/core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh \" ", filename,cores, walltime,name,stdout,stderr)

  #command <- sprintf("oarsub -t besteffort -t idempotent -p \"cputype=\'xeon\'\" -l core=%i,walltime=%s -n %s --stdout=%s --stderr=%s -S \"./ratscript2.sh %i %i %i %s %i %i\" ", cores, walltime,name,stdout,stderr,rat,seed,spawnslaves,currentTest, start_idx, end_idx)
  cat(command)
  cat("\n")
  system(command)
  #generateParamResMat(ratdata,model.data.dir,count)
}



