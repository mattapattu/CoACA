args <- commandArgs(trailingOnly = TRUE)
print(args)

rat <- as.integer(args[1])
seed <- as.numeric(args[2])
count <- as.integer(args[3])
currentTest <- as.character(args[4])
start_idx <- as.integer(args[5])
end_idx <- as.integer(args[6])
testSuite <- as.character(args[7])
#options(error=recover)
options(error=function()traceback(2))

print(sprintf("Executing testSuite %s", testSuite))

################ INIT ####################################################################

source("testConfig2.R")

############### Tests #############################################


if(currentTest == "computeModelParams")
{
  gridMat <- gridMat[start_idx:end_idx,]
  print(sprintf("start_idx=%i,end_idx=%i",start_idx,end_idx))
  
  seq_id <- which((sequences+1) %in% start_idx)
  name = paste0("mParams",seq_id,"_",paste0("rat",rat))
  analyzeParamSpaceV2(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)
}

################# getMinModel #########################################

if(currentTest == "getMinModel")
{            
  getMinModel(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir)
}

################### Test 2: generateModelParamMat #################333


if(currentTest == "generateModelParamMat"){

  generateParamResMatV2(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,testSuite)


}


################# Unit Test ###################

if(currentTest == "unitTestProbDiff"){

  unitTestProbDiff(ratdata,testData,model.src,setup.hpc,model.data.dir,seed,count)
}


################# Generate Dataset ######################

  if(currentTest == "generateDataset"){

    
    gridMat <- gridMat[start_idx:end_idx,]
    seq_id <- which((sequences+1) %in% start_idx)
    name = paste0("GenData",seq_id,"_",paste0("rat",rat))

    generateDataV4(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,seed,count, gridMat, name)


  } 


################# Test 3: Param estimation test ######################

  if(currentTest == "paramEstTest"){

    gridMat <- gridMat[start_idx:end_idx,]
    seq_id <- which((sequences+1) %in% start_idx)
    name = paste0("paramEs",seq_id,"_",paste0("rat",rat))
    print(sprintf("Test = paramEstTest, start_idx=%i, end_idx=%i",start_idx,end_idx))
    testParamEstimationV4(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)


  } 

  ################# Test 4: combineParamEstResLists ######################

  if(currentTest == "combineParamEstResLists"){

    combineParamEstResListsV4(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite)

  } 
  
  
  ################## Test 5: Holdout test on artificial data ################

  if(currentTest == "validateHoldout")
  {

    gridMat <- gridMat[start_idx:end_idx,]
    seq_id <- which((sequences+1) %in% start_idx)
    name = paste0("holdVal",seq_id,"_",paste0("rat",rat))
    print(sprintf("Test = validateHoldout, start_idx=%i, end_idx=%i",start_idx,end_idx))
 
    HoldoutTestV4(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name, initpop, testSuite)
    #model.data.dir = paste(model.data.dir,"holdoutTest",ratdata@rat,sep="/")
    #printMatRes(ratdata,testData,model.data.dir)
  }

################# Test 6: combineHoldoutResLists ######################

  if(currentTest == "combineHoldoutResLists"){

    combineHoldoutResListsV4(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite)

  } 

  ########### Test 7: Holdout on diif dtasets ####################

  if(currentTest == "coaca_on_arl"||currentTest == "arl_on_coaca")
  {

    #print(sprintf("gen.data.dir=%s",gen.data.dir))
    gridMat <- gridMat[start_idx:end_idx,]
    seq_id <- which((sequences+1) %in% start_idx)
    name = paste0("multiHold",seq_id,"_",paste0("rat",rat))
    print(sprintf("Test = holdoutMultiTest, start_idx=%i, end_idx=%i",start_idx,end_idx))
                                
    multiHoldoutValidation(ratdata,testData, src.dir,model.src,setup.hpc,model.data.dir,count,gridMat,name,initpop, testSuite, gen.model.dir)
    #model.data.dir = paste(model.data.dir,"holdoutTest",ratdata@rat,sep="/")
    #printMatRes(ratdata,testData,model.data.dir)
  }

  ##############

if(currentTest == "coaca_on_arl_combineRes" || currentTest == "arl_on_coaca_combineRes")
{
  combinemultiHoldoutResListsV4(ratdata,testData,src.dir,model.src,setup.hpc,model.data.dir,count, testSuite, gen.model.dir)

}


print(sprintf("End of script"))
