library(TTR)

getEndIndex = function(ratName, generated_data, sim, limit){
  
  if(sim==1){
    generated_data[,1:2] = generated_data[,1:2] + 1
  }
  if(ratName == "rat_101" || ratName == "robert")
  {
    if(limit > 0.85)
    {
      limit = 0.85
    }
    trialPeriod = 20
    consecutiveTrials = 15
  }
  else
  {
    #limit = 0.95
    trialPeriod = 30
    consecutiveTrials = 20
  }
  end_index1=0
  s1 <- which(generated_data[,2]==1)
  l<-which(SMA(generated_data[s1,3],trialPeriod)>=limit)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>consecutiveTrials){
      end_index1=k[[set]][1]
      break
    }
  }
  
  
  end_index2=0
  s2 <- which(generated_data[,2]==2)
  l<-which(SMA(generated_data[s2,3],trialPeriod)>=limit)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>consecutiveTrials){
      end_index2=k[[set]][1]
      break
    }
  }
  
  if(end_index1==0 || end_index2 ==0){
    end_index = -1
  }else{
    end_index = max(s1[end_index1],s2[end_index2])
    #end_index = round(length(generated_data[,1]))/2
  }
  
  #print(sprintf("end_index=%i", end_index))
  
  return(end_index)
}


checkSimLearns=function(allpaths,sim,limit)
{
  #end_index = -1
  simLearns = FALSE
  sessions <- allpaths[,5]
  uniqueSessIds <- unique(sessions)
  
  for(sess in uniqueSessIds)
  {
    sessIdx <- which(allpaths[,5]==sess)
    rewards_sess <- allpaths[sessIdx,3]
    successRate <- sum(rewards_sess)/length(sessIdx)
    if(successRate >= limit)
    {
      end_index = sessIdx[length(sessIdx)]
      simLearns = TRUE
      break
    }
  }
  
  return(simLearns)
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
      actId = which(slot(hybridModel, nodeList)==actions[j]) - 1
      hybridModelMat[actIdx,4] = actId
      hybridModelMat[actIdx,5] = row[5]
      
      turnVector = slot(slot(turnsModel,currState),currPath)
      hybridVector = slot(slot(hybridModel,currState),currPath)
      
      turnVector = gsub("\\w+\\.","",turnVector)
      hybridVector = gsub("\\w+\\.","",hybridVector)
      
      curr_action = gsub("\\w+\\.","",actions[j])
      if(!curr_action %in% turnVector)
      {
        common = intersect(hybridVector,turnVector)
        res = turnVector[!turnVector %in% common]
        res_idx = which(turnVector %in% res)
        turn_idx = which(turnsactNbvec == row[6])
        diff_times = turntimevec[turn_idx[res_idx]]
      }
      else
      {
        res_idx = which(turnVector %in% curr_action)
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


negLogLikFunc <- function(par, ratdata, half_index, modelData, testModel, sim) {
  alpha <- par[1]
  Model <- modelData@Model
  creditAssignment <- modelData@creditAssignment

  gamma1 <- par[2]
  #gamma2 <- par[3]
  # reward = par[4]
  # reward = 1+reward*9
  reward <- 1
  #
  modelData@alpha <- alpha
  modelData@gamma1 <- gamma1
  #modelData@gamma2 <- gamma2
  
  simLearns = checkSimLearns(ratdata@allpaths,sim=sim,limit=0.8)
  if(simLearns)
  {
   lik <- TurnsNew::getTurnsLikelihood(ratdata, modelData, testModel, sim)
   lik <- lik[1:half_index]
   negLogLik <- (-1) * sum(lik)
  }
  else
  {
   negLogLik = 1000000
  }

  # print(sprintf("negLogLik = %f",negLogLik))
  if (is.infinite(negLogLik)) {
    return(1000000)
  } else if (is.nan(negLogLik)) {
    print(sprintf("Alpha = %f", alpha))
    return(1000000)
  }
  else {
    return(negLogLik)
  }
} 


populateSimRatModel=function(ratdata,generated_data,testModelName)
{

  generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
  generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
  generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
  generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
  
  return(generated_data)
  
}


modifyParam=function(param)
{
  lower = param - (param/5)
  upper = param + (param/5)

  if(lower <=0 )
  {
    lower = param
  }

  if(upper > 1)
  {
    upper = 1
  }

  newparam = runif(1, lower, upper)
  return(newparam)
}

modifyModelData=function(modelData)
{

  modelData@alpha = modifyParam(modelData@alpha)
  modelData@gamma1 = modifyParam(modelData@gamma1)
  #modelData@gamma2 = modifyParam(modelData@gamma2)

  return(modelData)
}

getMinimumLikelihood=function(ratdata, allmodelRes,testingdata,sim)
{
  min_index = 0
  min = 100000
  min_method = "null"
  ratName = ratdata@rat
  #endLearningStage = getEndIndex(ratName,ratdata@allpaths,sim=sim, limit=0.95)
  #endLearningStage = endLearningStage/2
  #half_stage = endLearningStage/2
  half_stage = 800
  for(m in testingdata@Models)
  {
    for(crAssgn in testingdata@creditAssignment)
    {
      modelData = getModelData(allmodelRes,m,crAssgn)
      lik = modelData@likelihood
      lik = (-1)*sum(lik[-(1:half_stage)])
      #lik = (-1)*sum(lik[(half_stage:endLearningStage)])
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

