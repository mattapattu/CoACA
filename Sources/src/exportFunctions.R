library(TTR)

getEndIndex = function(generated_data, sim, limit){
  if(sim==1){
    generated_data[,1:2] = generated_data[,1:2] + 1
  }
  end_index1=0
  s1 <- which(generated_data[,2]==1)
  l<-which(SMA(generated_data[s1,3],30)>=limit)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>20){
      end_index1=k[[set]][1]
      break
    }
  }
  
  
  end_index2=0
  s2 <- which(generated_data[,2]==2)
  l<-which(SMA(generated_data[s2,3],30)>=limit)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>20){
      end_index2=k[[set]][1]
      break
    }
  }
  
  if(end_index1==0 || end_index2 ==0){
    end_index = -1
  }else{
    end_index = max(s1[end_index1],s2[end_index2])
  }
  
  #print(sprintf("end_index=%i", end_index))
  
  return(end_index)
}


convertTurnTimes=function(ratdata, turnsModel, hybridModel, sim)
{
  allpaths = ratdata@allpaths
  turnTimes = ratdata@turnTimes
  
  if(sim == 1)
  {
    turntimevec = turnTimes[,4]
    turnsactNbvec = turnTimes[,6]
  }
  else
  {
    turntimevec = turnTimes[,6]
    turnsactNbvec = turnTimes[,1]
  }
  
  totActions = length(turnTimes[,1])*2
  hybridModelMat = matrix(0,totActions,6)
  colnames(hybridModelMat) <- c("ActionNb", "Path", "State","ActionId","Session", "Duration" )
  actIdx = 1;
  
  if(sim != 1)
  {
    allpaths[,1:2] = allpaths[,1:2]-1
    
  }
  
  currPath=""
  currState=""
  nodeList=""
  for(rowNb in 1:nrow(allpaths)) 
  {
    row = allpaths[rowNb,]
    if(row[1]==0)
    {
      currPath = "Path0";
    }
    else if(row[1]==1)
    {
      currPath = "Path1";
    }
    else if(row[1]==2)
    {
      currPath = "Path2";
    }
    else if(row[1]==3)
    {
      currPath = "Path3";
    }
    else if(row[1]==4)
    {
      currPath = "Path4";
    }
    else if(row[1]==5)
    {
      currPath = "Path5";
    }
    else if(row[1]==6)
    {
      next;
    }
    
    if(row[2]==0)
    {
      currState = "S0"
      nodeList = "nodes.S0"
    }
    else if(row[2]==1)
    {
      currState = "S1"
      nodeList = "nodes.S1"
    }
    
    actions = slot(slot(hybridModel, currState),currPath)
    for(j in 1:length(actions))
    {
      hybridModelMat[actIdx,1] = row[6]
      hybridModelMat[actIdx,2] = row[1]
      hybridModelMat[actIdx,3] = row[2]
      actId = which(slot(hybridModel, nodeList) == actions[j]) - 1
      hybridModelMat[actIdx,4] = actId
      hybridModelMat[actIdx,5] = row[5]
      
      turnVector = slot(slot(turnsModel,currState),currPath)
      hybridVector = slot(slot(hybridModel,currState),currPath)
      
      if(!actions[j] %in% turnVector)
      {
        common = intersect(hybridVector,turnVector)
        res = turnVector[!turnVector %in% common]
        res_idx = which(turnVector %in% res)
        turn_idx = which(turnsactNbvec == row[6])
        diff_times = turntimevec[turn_idx[res_idx]]
      }
      else
      {
        res_idx = which(turnVector %in% actions[j])
        turn_idx = which(turnsactNbvec == row[6])
        diff_times = turntimevec[turn_idx[res_idx]]
      }
      
      hybridModelMat[actIdx,6] = sum(diff_times)
      actIdx = actIdx+1
    }
  }
  hybridModelMat = hybridModelMat[-(actIdx:totActions),]
  if(sim == 1)
  {
    hybridModelMat = cbind(hybridModelMat[,4],hybridModelMat[,3],rep(0,length(hybridModelMat[,3])),hybridModelMat[,6],hybridModelMat[,5],hybridModelMat[,1])
  }
  return(hybridModelMat)
}


negLogLikFunc=function(par,ratdata,half_index,modelData,testModel,sim) {
  
  alpha = par[1]
  Model = modelData@Model
  creditAssignment = modelData@creditAssignment
  
  if(Model == "Paths")
  {
    if(creditAssignment == "aca3"){
      Hinit = matrix(0,2,6)
      gamma1 = par[2]
      gamma2 = par[3]
      
      probMatrix = Aca3::getProbMatrix(ratdata@allpaths, alpha, gamma1, gamma2, sim)
      path4Probs = probMatrix[which(probMatrix[,4]>0),4]
      path4AboveLim = which(path4Probs >= 0.95)
      result <- rle(diff(path4AboveLim))
      path4Converged = any(result$lengths>=30 & result$values==1)
      
      path10Probs = probMatrix[which(probMatrix[,10]>0),10]
      path10AboveLim = which(path10Probs >= 0.95)
      result <- rle(diff(path10AboveLim))
      path10Converged = any(result$lengths>=30 & result$values==1)
      
      if(path4Converged &&  path10Converged)
      {
        lik = Aca3::getPathLikelihood(ratdata@allpaths[1:half_index,], alpha,gamma1,gamma2, sim)
      }
      else
      {
        lik = -1000000
      }
      
    }
  }
  else
  {
    if(creditAssignment == "aca3"){
      gamma1 = par[2]
      gamma2 = par[3]
      # reward = par[4]
      # reward = 1+reward*9
      reward = 1
      # 
      modelData@alpha = alpha
      modelData@gamma1 = gamma1
      modelData@gamma2 = gamma2
      probMatrix = TurnsNew::getProbMatrix(ratdata,modelData,testModel,sim)
      path4Probs = probMatrix[which(probMatrix[,4]>0),4]
      path4AboveLim = which(path4Probs >= 0.95)
      result <- rle(diff(path4AboveLim))
      path4Converged = any(result$lengths>=30 & result$values==1)
      
      path10Probs = probMatrix[which(probMatrix[,10]>0),10]
      path10AboveLim = which(path10Probs >= 0.95)
      result <- rle(diff(path10AboveLim))
      path10Converged = any(result$lengths>=30 & result$values==1)
      
      if(path4Converged &&  path10Converged)
      {
        #ratdata@allpaths = ratdata@allpaths[1:half_index,]
        lik=TurnsNew::getTurnsLikelihood(ratdata,modelData,testModel,sim)
        lik = lik[1:half_index]
      }
      else
      {
        lik = -1000000
      }
      
      
    }   
  }  
  
  negLogLik = (-1) *sum(lik)
  # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else if(is.nan(negLogLik)){
    print(sprintf("Alpha = %f",alpha ))
    return(1000000)
  }
  else{
    return(negLogLik)
  }
  
}

getTurnTimesMat=function(trueRatdata, generatedData, modelName)
{
  
  
  allpaths = trueRatdata@allpaths
  turnTimes = trueRatdata@turnTimes
  
  genPathData = generatedData@allpaths
  if(modelName == "Paths")
  {
    genHybridTurnTimes = slot(generatedData, "allpaths")
  }
  else if(modelName == "Turns")
  {
    genHybridTurnTimes = slot(generatedData, "turnTimes")
  }
  else if(modelName == "Hybrid1")
  {
    genHybridTurnTimes = slot(generatedData, "hybridModel1")
  }
  else if(modelName == "Hybrid2")
  {
    genHybridTurnTimes = slot(generatedData, "hybridModel2")
  }
  else if(modelName == "Hybrid3")
  {
    genHybridTurnTimes = slot(generatedData, "hybridModel3")
  }
  else if(modelName == "Hybrid4")
  {
    genHybridTurnTimes = slot(generatedData, "hybridModel4")
  }
  
  hybridModel = slot(allModels, modelName)
  
  
  totActions = length(genHybridTurnTimes[,1])*2
  TurnTimes = matrix(0,totActions,6)
  colnames(TurnTimes) <- c("Turn", "State", "Reward","Duration","Session", "ActionNb" )
  actIdx = 1;
  
  currPath=""
  currState=""
  nodeList=""
  for(rowNb in 1:nrow(allpaths)) 
  {
    row = genPathData[rowNb,]
    if(row[1]==0)
    {
      currPath = "Path0";
    }
    else if(row[1]==1)
    {
      currPath = "Path1";
    }
    else if(row[1]==2)
    {
      currPath = "Path2";
    }
    else if(row[1]==3)
    {
      currPath = "Path3";
    }
    else if(row[1]==4)
    {
      currPath = "Path4";
    }
    else if(row[1]==5)
    {
      currPath = "Path5";
    }
    else if(row[1]==6)
    {
      next;
    }
    
    if(row[2]==0)
    {
      currState = "S0"
      nodeList = "nodes.S0"
    }
    else if(row[2]==1)
    {
      currState = "S1"
      nodeList = "nodes.S1"
    }
    
    actions = slot(slot(TurnModel, currState),currPath)
    for(j in 1:length(actions))
    {
      actId = which(slot(TurnModel, nodeList) == actions[j]) - 1
      TurnTimes[actIdx,1] = actId
      TurnTimes[actIdx,2] = row[1]
      if(j==length(actions))
      {
        TurnTimes[actIdx,3] = row[2]
      }
      else
      {
        TurnTimes[actIdx,3] = 0
      }
      
      TurnTimes[actIdx,5] = row[5]
      actionNbIdx = which(genHybridTurnTimes[,6]==row[6])
      TurnTimes[actIdx,6] = unique(genHybridTurnTimes[actionNbIdx,6])
      
      
      turnVector = slot(slot(TurnModel,currState),currPath)
      hybridVector = slot(slot(hybridModel,currState),currPath)
      
      ## Here - 
      ### if "fga1" not in "fgabc1", then get row[7]
      
      if(!actions[j] %in% hybridVector)
      {
        turnsNotInIdx = which(!hybridVector %in% turnVector)
        ## Split the element duration into 2 turns (no element is greater than 2 turns??)
        actDuration = genHybridTurnTimes[actionNbIdx[turnsNotInIdx],4]/2
        
      }
      else
      {
        turnsInIdx = which(hybridVector %in% actions[j])
        actDuration = genHybridTurnTimes[actionNbIdx[turnsInIdx],4]
        
      }
      
      TurnTimes[actIdx,4] = actDuration
      actIdx = actIdx+1
    }
  }
  TurnTimes = TurnTimes[-(actIdx:totActions),]
  return(TurnTimes)
}



populateSimRatModel=function(ratdata,generated_data,testModelName)
{
  if(testModelName=="Paths")
  {
    generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
    generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
    generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
    generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
  }
  else if(testModelName=="Turns")
  {
    generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
    generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
    generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
    generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
    
    
  }
  else if(testModelName=="Hybrid1")
  {
    generated_data@turnTimes = getTurnTimesMat(ratdata,generated_data,testModelName)
    generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
    generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
    generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
    
    
  }
  else if(testModelName=="Hybrid2")
  {
    generated_data@turnTimes = getTurnTimesMat(ratdata,generated_data,testModelName)
    generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
    generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
    generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
    
    
  }
  else if(testModelName=="Hybrid3")
  {
    generated_data@turnTimes = getTurnTimesMat(ratdata,generated_data,testModelName)
    generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
    generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
    generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
    
    
  }
  else if(testModelName=="Hybrid4")
  {
    generated_data@turnTimes = getTurnTimesMat(ratdata,generated_data,testModelName)
    generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
    generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
    generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
    
    
  }
  
  return(generated_data)
  
}


getMinimumLikelihood=function(allmodelRes,testingdata)
{
  min_index = 0
  min = 100000
  min_method = "null"
  
  for(m in testingdata@Models)
  {
    for(crAssgn in testingdata@creditAssignment)
    {
      modelData = getModelData(allmodelRes,m,crAssgn)
      lik = modelData@likelihood
      modelName = paste(modelData@Model,modelData@creditAssignment,sep=".")
      
      print(sprintf("model=%s,likelihood=%f",modelName,lik))
      
      if(lik < min)
      {
        min = lik
        min_method = modelName
      }
    }
  }
  return(min_method)
}
