library(dplyr)
library(RColorBrewer)
library(TTR)


getPathNumber=function(path){
  #path  = gsub("^, ","",path)
  
  if(grepl("^d.*c.*h.*i$",path)){
    pathnb = 1
  }else if(grepl("^f.*g.*a.*k.*j.*i$",path)){ 
    pathnb = 2  
  }else if(grepl("^d.*c.*b.*a.*k.*j.*i$",path)){
    pathnb = 3
  }else if(grepl("^f.*g.*a.*b.*c.*d.*e$",path)){
    pathnb = 5
  }else if(grepl("^f.*g.*a.*b.*c.*h.*i$",path)){
    pathnb = 4
  }else if(grepl("^d.*c.*b.*a.*g.*f.*e$",path)){
    pathnb = 6
  }
  else if(grepl("^h.*c.*d.*e$",path)){
    pathnb = 1
  }else if(grepl("^j.*k.*a.*g.*f.*e$",path)){ 
    pathnb = 2
  }else if(grepl("^h.*c.*b.*a.*g.*f.*e$",path)){
    pathnb = 3
  }else if(grepl("^j.*k.*a.*b.*c.*h.*i$",path)){
    pathnb = 5
  }else if(grepl("^j.*k.*a.*b.*c.*d.*e$",path)){
    pathnb = 4
  }else if(grepl("^h.*c.*b.*a.*k.*j.*i$",path)){
    pathnb = 6
  }
  else if(grepl("^.*e$",path)){
    pathnb = 7
  }else if(grepl("^.*i$",path)){
    pathnb = 7
  }else{
    ## A =7
    pathnb=7
  }
  
  return(pathnb)
}


# Handle incomplete paths in the begining or when recording is lost
updateACAPathNbmse=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
  for(i in 1:(length(allpaths[,1]))){
    #ses=as.numeric(allpaths[i,"Session"])
    #trial=i-which(allpaths[,"Session"]==ses)[1]+1
    #l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    #R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    
    
    allpaths[i,5] = getPathNumber(allpaths[i,1])
    
    if(allpaths[i,5] == 4)
    {
      allpaths[i,6] = 1
    }
    else
    {
      allpaths[i,6] = 0
    }
    
    if(grepl("^f",allpaths[i,1])||grepl("^d",allpaths[i,1])){
      allpaths[i,7]=1
    }
    else if(grepl("^h",allpaths[i,1])||grepl("^j",allpaths[i,1])){
      allpaths[i,7]=2
    }
    ## (to assign states for incomplete paths seen at the end/begining of records)
    else if(i>1){
      
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,7]=1
      }
      else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,7]=2
      }
      ## If cannot be estimated, then do by default : assume the trial = Path5
      else if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,7]=1
      }
      else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,7]=2
      }
      
    }else if(i==1){
      if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,7]=2
      }else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,7]=1
      }
      
    }
    
  }
  return(allpaths)
}

getTurnDuration=function(path, state, enregRows)
{
  
  turntimes=list();
  if (state == 0)
  {
    if (path == 0)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      c2 = c/2
      h = enregRows[3,2] - enregRows[3,1]
      c2h = c2 + h
      
      
      turntimes = list("dc1"=dc1, "c2h" = c2h)
      
    }
    else if (path == 1)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      a2 = a/2
      kj = sum(enregRows[4:5,2] - enregRows[4:5,1])
      a2kj = a2 + kj
      
      
      turntimes = list("fga1"=fga1, "a2kj" = a2kj)
      
    }
    else if (path == 2)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      kj = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2kj = a2+kj
      
      turntimes = list("dc1"=dc1, "c2ba1" = c2ba1, "a2kj"=a2kj)
      
      
    }
    else if (path == 3)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      h = enregRows[6,2] - enregRows[6,1]
      c2h = c2 + h
      
      turntimes = list("fga1"=fga1, "a2bc1" = a2bc1, "c2h"=c2h)
      
    }
    else if (path == 4)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      d = enregRows[6,2] - enregRows[6,1]
      c2d = c2 + d
      
      turntimes = list("fga1"=fga1, "a2bc1" = a2bc1, "c2d"=c2d)
    }
    else if (path == 5)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      gf = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2gf = a2 + gf
      
      turntimes = list("dc1"=dc1, "c2ba1" = c2ba1, "a2gf"=a2gf)
    }
  }
  else if (state == 1)
  {
    if (path == 0)
    {
      
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      c2 = c/2
      d = enregRows[3,2] - enregRows[3,1]
      c2d = c2 + d
      
      
      turntimes = list("hc1"=hc1, "c2d" = c2d)
    }
    else if (path == 1)
    {
      
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      a2 = a/2
      gf = sum(enregRows[4:5,2] - enregRows[4:5,1])
      a2gf = a2 + gf
      
      
      turntimes = list("jka1"=jka1, "a2gf" = a2gf)
    }
    else if (path == 2)
    {
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      gf = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2gf = a2 + gf
      
      turntimes = list("hc1"=hc1, "c2ba1" = c2ba1, "a2gf"=a2gf)
      
    }
    else if (path == 3)
    {
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      d = enregRows[6,2] - enregRows[6,1]
      c2d = c2 + d
      
      
      turntimes = list("jka1"=jka1, "a2bc1" = a2bc1, "c2d"=c2d)
      
    }
    else if (path == 4)
    {
      
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      h = enregRows[6,2] - enregRows[6,1]
      c2h = c2 + h
      
      turntimes = list("jka1"=jka1, "a2bc1" = a2bc1, "c2h"=c2h)
    }
    else if (path == 5)
    {
      
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      kj = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2kj = a2 + kj
      
      turntimes = list("hc1"=hc1, "c2ba1" = c2ba1, "a2kj"=a2kj)
    }
  }
  
  return (turntimes);
}


getTurnsMatrix=function(allpaths,enreg,turnsModel)
{
  totalTurns = 3*length(allpaths[,1])
  turnTimes = matrix(0,totalTurns,6)
  colnames(turnTimes) <- c("Action", "Path", "State","Turn","Session", "Duration" )
  turnIdx = 1;
  boxIndices = as.numeric(allpaths[,3])
  uniqSess = unique(as.numeric(allpaths[,4]))
  
  for(ses in uniqSess)
  {
    idx_ses = which(as.numeric(allpaths[,4])==ses)
    pathCount_ses = length(allpaths[idx_ses,1])
    
    for(i in 1:pathCount_ses)
    {
      path = as.numeric(allpaths[idx_ses[i],5])-1
      state = as.numeric(allpaths[idx_ses[i],7])-1
      
      pathSlot = ""
      nodeList = ""
      stateSlot = ""
      
      if(path == 0)
      {
        pathSlot = "Path0"
      }
      else if(path == 1)
      {
        pathSlot = "Path1"
      }
      else if(path == 2)
      {
        pathSlot = "Path2"
      }
      else if(path == 3)
      {
        pathSlot = "Path3"
      }
      else if(path == 4)
      {
        pathSlot = "Path4"
      }
      else if(path == 5)
      {
        pathSlot = "Path5"
      }
      else if(path == 6)
      {
        next
      }
      
      if(state==0)
      {
        nodeList = "nodes.S0"
        stateSlot = "S0"
      }
      else
      {
        nodeList = "nodes.S1"
        stateSlot = "S1"
      }
      
      
      turns = slot(slot(turnsModel, stateSlot),pathSlot)
      
      if(i==1)
      {
        idx = seq(1,boxIndices[idx_ses[i]])
      }
      else
      {
        idx = seq((boxIndices[idx_ses[i]-1]+1), boxIndices[idx_ses[i]])
      }
      
      
      enregRows = enreg[[ses]]$tab[idx,]
      
      turntimes = getTurnDuration(path, state, enregRows)
      
      
      if(length(turns) >0)
      {
        for(j in 1:length(turns))
        {
          turnTimes[turnIdx,1] = idx_ses[i]
          turnTimes[turnIdx,2] = path
          turnTimes[turnIdx,3] = state
          actId = which(slot(turnsModel, nodeList) == turns[j]) - 1
          turnTimes[turnIdx,4] = actId
          turnTimes[turnIdx,5] = ses
          turnTimes[turnIdx,6] = turntimes[[j]]
          turnIdx = turnIdx+1
        }
      }
      
    }
    
  }
  
  turnTimes = turnTimes[-(turnIdx:totalTurns),]
  
  return(turnTimes)
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


### Just for sim=1
### get TurnTimes from hybrid turn data
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


boxplotMse = function(mat_res, model,rat){
  
  if(model == 1){
    jpeg(paste("boxplot_ACA_",rat,".jpeg",sep=""))
  }else if(model == 2){
    jpeg(paste("boxplot_GB_",rat,".jpeg",sep=""))
  }else if(model == 3){
    jpeg(paste("boxplot_GB_ACA_",rat,".jpeg",sep=""))
  }else if(model == 4){
    jpeg(paste("boxplot_ACA2_",rat,".jpeg",sep=""))
  }else if(model == 5){
    jpeg(paste("boxplot_ACA3_",rat,".jpeg",sep=""))
  }
  
  boxplot(as.numeric(mat_res[,1]),as.numeric(mat_res[,3]),as.numeric(mat_res[,5]), as.numeric(mat_res[,7]), xaxt="n")
  axis(side=1, at=c(1,2,3,4), labels = c("ACA","GB", "ACA2", "ACA3"))
  dev.off()
}




generatePlots=function(rat,window, ACAprobMatrix, GBprobMatrix,  SARSAprobMatrix, ACA3probMatrix, allpaths_num){
  
  rle_sess = rle(allpaths_num[,5])
  last_paths<-cumsum(rle_sess$lengths)
  allpaths_num<-allpaths_num[-last_paths,]
  
  empiricalProbMatrix = baseModels::empiricalProbMat(allpaths_num, window = window)
  
  state1=which(allpaths_num[,2]==1)
  state2=which(allpaths_num[,2]==2)
  rle_state1 = rle(allpaths_num[state1,5])
  rle_state2 = rle(allpaths_num[state2,5])
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("Prob_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      
      if(state==1){
        rle_sess = rle_state1
        stateidx= state1
      }else{
        rle_sess = rle_state2
        stateidx=state2
      }
      
      plot(GBprobMatrix[stateidx,(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability", xaxt='n', xlab='')
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      
      axis(1, at=seq(1,length(stateidx),by=100))
      mtext(paste("State", state, "Trials"), side = 1, line = 2, cex=0.9)
      axis(3, line=0,at=c(1,cumsum(rle_sess$lengths[-length(rle_sess$lengths)])), labels = rle_sess$values)
      mtext("Session Nb", side = 3, line = 2, cex=0.9)
      abline(v=c(1,cumsum(rle_sess$lengths[-length(rle_sess$lengths)])), lty=3)
      title(paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ), line = 3.3, cex=0.4)
      
      lines(empiricalProbMatrix[stateidx,(act+6*(state-1))],col='blue',type='l',lty=1, lwd=1)
      lines(ACAprobMatrix[stateidx,(act+6*(state-1))],col='green',type='l', lwd=1)
      lines(SARSAprobMatrix[stateidx,(act+6*(state-1))],col='orange',type='l', lwd=1)
      lines(ACA3probMatrix[stateidx,(act+6*(state-1))],col='red',type='l', lwd=1)
      
      
      if(act==4||act==10){
        legend("bottomright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for SARSA","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.8,lty = c(1,1,1,1,1))
        
      }else{
        legend("topright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for SARSA","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.8,lty = c(1,1,1,1,1))
        
      }
      
      
      dev.off()
    }
  }
  
}

generateModelProbPlots=function(rat, window, res1, res2,models, allpaths_num){
  
  rle_sess = rle(allpaths_num[,5])
  last_paths<-cumsum(rle_sess$lengths)
  #allpaths_num1<-allpaths_num[-last_paths,]
  
  empiricalProbMatrix = baseModels::empiricalProbMat(allpaths_num, window = window)
  
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      pdf(file=paste("Prob_",rat,"_Path", act, "_State",state,".pdf",sep=""))
      
      
      cols <- brewer.pal(8,'Dark2')
      
      if(act==4||act==1)
      {
        ylim=c(0,1)
      }
      else
      {
        ylim=c(0,0.6)
      }
      
      ### Ugly - update this "aca3TurnData"
      if(any(grepl("Turns",models)))
      {
        pathids = res2$aca3TurnData@ProbMatrix[,17]
        uniqPathIds = unique(pathids)
        #uniqPathIds = uniqPathIds[which(uniqPathIds %in% allpaths_num[,6])]
        empiricalMatIdx = which(allpaths_num[,6] %in% uniqPathIds & allpaths_num[,2] == state)
        pathIdsInState = allpaths_num[which(allpaths_num[,6] %in% uniqPathIds & allpaths_num[,2] == state),6]
      }
      else
      {
        empiricalMatIdx = which(allpaths_num[,2]==state)
        pathIdsInState = allpaths_num[which(allpaths_num[,2]==state),6]
      }
      
      if(state==1)
      {
        box = "box E"
      }
      else
      {
        box = "box I"
      }
      actNb = act+ ((state-1)*6)
      plot(1, type="n", xlab="Trials", ylab="Probability", xlim=c(0, length(which(allpaths_num[,2]==state))), ylim=ylim, main = paste0(rat,": Path ",actNb,", ",box),cex.lab=1.3)
      lines(empiricalProbMatrix[empiricalMatIdx,(act+6*(state-1))])
      
      i=0
      for(m in models)
      {
        i = i+1
        if(m == "aca")
        {
          probmatrix = res1$acamse@ProbMatrix
        }
        else if(m == "gb")
        {
          probmatrix = res1$gbmse@ProbMatrix
        }
        else if(m == "aca2")
        {
          probmatrix = res1$aca2mse@ProbMatrix
        }
        else if(m == "aca3")
        {
          probmatrix = res1$aca3mse@ProbMatrix
        }
        else if(m == "sarsa")
        {
          probmatrix = res1$sarsamse@ProbMatrix
        }
        else if(m == "acaTurns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$acaTurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "gbTurns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$gbTurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "aca2Turns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$aca2TurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "aca3Turns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$aca3TurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "sarsaTurns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$TurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        
        
        lines(probmatrix[which(probmatrix[,13] %in% pathIdsInState),(act+6*(state-1))],col=cols[i],ylab="Probability",lwd=2)
        
      }
      modelnames = models
      turnIndices = grep("Turns",modelnames)
      modelnames[-turnIndices] = paste(modelnames[-turnIndices],"Paths",sep="")
      if(act ==4)
      {
        legend("bottomright", legend=c(modelnames,"Empirical"),col=c(cols[1:i],cols[8]),cex=1.5,lty = rep(1,(i+1)),lwd=2)
      }
      else
      {
        legend("topright", legend=c(modelnames,"Empirical"),col=c(cols[1:i],cols[8]),cex=1.5,lty = rep(1,(i+1)),lwd=2)
      }
      
      dev.off()
    }
  }
  
}


generateEmpiricalPlots=function(rat,empiricalProbMatrix2,endLearningStage){
  x2<-empiricalProbMatrix2[,13]
  x2_rle <- rle(x2)
  x1<-cumsum(empiricalProbMatrix2[,15]-empiricalProbMatrix2[,14]+1)
  endLearningIdx = findInterval(endLearningStage, x1)
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("EmpProb_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      
      plot(empiricalProbMatrix2[,(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability", xaxt='n', xlab='Session Nb')
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      
      axis(1, line=0,at=cumsum(x2_rle$lengths), labels = x2_rle$values)
      abline(v=cumsum(x2_rle$lengths), lty=3)
      abline(v=endLearningIdx,col='red',lwd=3)
      title(paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ), line = 2, cex=0.4)
      
      dev.off()
    }
  }
  
}

getEmpProbMat=function(allpaths,window,sim){
  colLen = length(allpaths[,1])
  empProbMat = numeric(colLen)
  stateOne = 1
  stateTwo = 2
  if(sim == 1){
    stateOne = 0
    stateTwo = 1
  }
  state1Idx <- which(allpaths[,2]==stateOne)
  state2Idx <- which(allpaths[,2]==stateTwo)
  
  for(trial in c(1:colLen)){
    
    action = allpaths[trial,1]
    state = allpaths[trial,2]
    curr_state_idx <- which(allpaths[1:trial,2]==state)
    if(length(curr_state_idx) > window){
      curr_state_idx <- curr_state_idx[(length(curr_state_idx)-window):length(curr_state_idx)]
      
    }
    empProbMat[trial] <- length(which(allpaths[curr_state_idx,1]== action))/length(curr_state_idx)
    
  }
  return(empProbMat)
}

getStartIndex = function(generated_data){
  start_index=0
  l<-which(SMA(generated_data[,3],30)>=0.6)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      start_index=k[[set]][1]
      break
    }
  }
  return(start_index)
}

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
    #end_index = round(length(generated_data[,1]))/2 
    end_index = max(s1[end_index1],s2[end_index2])
  }
 
 return(end_index) 
}

simulateTurnTime=function(turnTimes, allpaths,turnId, turnNb)
{
  grp1 = c(0,2,7,8,10,15)
  grp2 = c(1,5,6,9,13,14)
  grp3 = c(3,4,11,12)
  
  endStage1 = getEndIndex(allpaths,sim=2,limit=0.5)
  turnIdxStage1 = last(which(turnTimes[,1]<=endStage1))
  endStage2 = getEndIndex(allpaths,sim=2,limit=0.95)
  turnIdxStage2 = last(which(turnTimes[,1]<=endStage2))
  endStage3 = length(allpaths[,1])
  turnIdxStage3 = length(turnTimes[,1])
  #pathstages=c(1,endStage1,endStage2,endStage3)
  turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
  
  out<-cut(turnNb, breaks=turnstages,right = FALSE,include.lowest = TRUE)
  as.numeric(out)
  start = turnstages[as.numeric(out)]
  end = turnstages[as.numeric(out)+1]-1
  
  if(turnId %in% grp1)
  {
    x=turnTimes[which(turnTimes[start:end,4] %in% grp1),6]
  }
  else if(turnId %in% grp2)
  {
    x=turnTimes[which(turnTimes[start:end,4] %in% grp2),6] 
  }
  else if(turnId %in% grp3)
  {
    x=turnTimes[which(turnTimes[start:end,4] %in% grp3),6] 
  }
  x=x[!x %in% boxplot.stats(x)$out]
  sampledTime = sample(x,1)
  return(sampledTime)
}


simulatePathTime=function(turnTimes, allpaths,path, pathNb)
{
  grp1 = c(1)
  grp2 = c(2,3,4,5,6)
  
  endStage1 = getEndIndex(allpaths,sim=2,limit=0.5)
  turnIdxStage1 = last(which(turnTimes[,1]<=endStage1))
  endStage2 = getEndIndex(allpaths,sim=2,limit=0.95)
  turnIdxStage2 = last(which(turnTimes[,1]<=endStage2))
  endStage3 = length(allpaths[,1])
  turnIdxStage3 = length(turnTimes[,1])
  
  pathstages=c(1,endStage1,endStage2,endStage3)
  #turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
  
  out<-cut(pathNb, breaks=pathstages,right = FALSE,include.lowest = TRUE)
  as.numeric(out)
  start = pathstages[as.numeric(out)]
  end = pathstages[as.numeric(out)+1]-1
  
  if(path %in% grp1)
  {
    allpathIdx=which(allpaths[start:end,1] %in% grp1)
  }
  else if(path %in% grp2)
  {
    allpathIdx=which(allpaths[start:end,1] %in% grp2)
  }
  x=allpaths[allpathIdx,4]
  remove = which(x %in% boxplot.stats(x)$out)
  allpathIdx = allpathIdx[-remove]
  sampledId = sample(allpathIdx,1)
  pathNbSelected = allpaths[sampledId,6]
  turndurations = turnTimes[which(turnTimes[,1]==pathNbSelected),6]
  return(turndurations)
}

enregCombine=function(enreg,rat){
  allpaths <- matrix("",0,4)
  colnames(allpaths) <- c("Path","Time","boxId","sessNb")
  boxTimes <- vector()
  ### Loop through all enreg[[ses]] of current rat
  for(ses in 1:length(enreg)){
    
    allpaths_ses = strsplit(enreg[[ses]]$short,"(?<=[ei])(?=(jk)|(ja)|(jb)|(fg)|(fb)|(fa)|(dc)|(hc)|(jik))",perl=TRUE)[[1]]
    boxIndices = c(1,cumsum(nchar(allpaths_ses)))
    allpaths_ses = cbind(allpaths_ses,rep(0,length(allpaths_ses)),rep(0,length(allpaths_ses)))
    
    for(i in 1:(length(boxIndices)-1))
    {
      range = boxIndices[i+1]-boxIndices[i]
      if(range > 0)
      {
        range = range-1
      }
      idx = seq((boxIndices[i+1]-range), boxIndices[i+1])
      enregRows = enreg[[ses]]$tab[idx,]
      if(is.matrix(enregRows))
      {
        pathTime = sum((enregRows[,2]-enregRows[,1])[-length(enregRows[,2])]) ## Remove last box which is i/e
      }
      else
      {
        pathTime = enregRows[2] - enregRows[1]
      }
      
      allpaths_ses[i,2] = pathTime
      allpaths_ses[i,3] = boxIndices[i+1]
      
    }
    
    allpaths_ses = cbind(allpaths_ses,ses)
    allpaths <- rbind(allpaths,allpaths_ses)
  }
  
  return(list("allpaths" = allpaths, "boxTimes" = boxTimes))
}


populateRatModel=function(rat,allpaths,enreg,turnsModel)
{
  
  allpaths = updateACAPathNbmse(allpaths)
  
  allpaths_num = matrix(as.numeric(unlist(allpaths[,c(5,7,6,2,4)])),nrow=nrow(allpaths[,c(5,7,6,2,4)]))
  allpaths_num = cbind(allpaths_num,c(1:length(allpaths_num[,1])))
  allpaths_num = cbind(allpaths_num,as.numeric(allpaths[,3]))
  #debug(getTurnsMatrix)
  turnTimes = getTurnsMatrix(allpaths,enreg,turnsModel)
  
  ratdata = new("RatData", rat = rat,allpaths = allpaths_num,turnTimes = turnTimes)
  
  hybrid1 = convertTurnTimes(ratdata,TurnModel,Hybrid1,sim=2)
  hybrid2 = convertTurnTimes(ratdata,TurnModel,Hybrid2,sim=2)
  hybrid3 = convertTurnTimes(ratdata,TurnModel,Hybrid3,sim=2)
  hybrid4 = convertTurnTimes(ratdata,TurnModel,Hybrid4,sim=2)
  
  ratdata@hybridModel1 = hybrid1
  ratdata@hybridModel2 = hybrid2
  ratdata@hybridModel3 = hybrid3
  ratdata@hybridModel4 = hybrid4
  
  return(ratdata)
  
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


