library(dplyr)
library(RColorBrewer)
library(TTR)
library(pracma)
#library(ggnewscale)
library(ggplot2)
#library(scales)
#library(gridExtra)
library(grid)
library(gridExtra)
#library(gtable)
library(reshape)


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



generatePlots=function(ratdata,allmodelRes,window,plot.dir){
  
  allpaths = ratdata@allpaths
  rle_sess = rle(allpaths[,5])
  last_paths<-cumsum(rle_sess$lengths)
  allpaths1<-allpaths[-last_paths,]
  
  #empiricalProbMat = baseModels::empiricalProbMat(allpaths1, window = window)
  empiricalProbMat = getEmpProbMat(allpaths,30,2)
  TurnsMat = allmodelRes@Turns@aca2@probMatrix
  PathsMat = allmodelRes@Paths@aca2@probMatrix
  Hybrid1Mat = allmodelRes@Hybrid1@aca2@probMatrix
  Hybrid2Mat = allmodelRes@Hybrid2@aca2@probMatrix
  Hybrid3Mat = allmodelRes@Hybrid3@aca2@probMatrix
  Hybrid4Mat = allmodelRes@Hybrid4@aca2@probMatrix
  
  rat=ratdata@rat
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      pdf(paste(plot.dir,"/Prob_",rat,"_Path", act, "_State",state,".pdf",sep=""))
      
      par(mfrow=c(3,2))
      
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      
      # axis(1, at=seq(1,length(stateidx),by=100))
      # mtext(paste("State", state, "Trials"), side = 1, line = 2, cex=0.9)
      # axis(3, line=0,at=c(1,cumsum(rle_sess$lengths[-length(rle_sess$lengths)])), labels = rle_sess$values)
      # mtext("Session Nb", side = 3, line = 2, cex=0.9)
      # abline(v=c(1,cumsum(rle_sess$lengths[-length(rle_sess$lengths)])), lty=3)
      
      cols=c("black","blue","darkgreen","red", "darkmagenta","darkorange","deeppink")
      #cols <- brewer.pal(7,'Dark2')
      sIdx <- which(PathsMat[,(act+6*(state-1))]>0)
      plot(sIdx,PathsMat[which(PathsMat[,(act+6*(state-1))]>0),(act+6*(state-1))],col=cols[2],type='l',lty=1,ylim=c(0,1),ylab="Probability", main="Path Model",xlab="Trial")
      lines(sIdx,empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col='black',type='l')
      
      plot(sIdx,TurnsMat[which(TurnsMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col=cols[3],type='l',ylim=c(0,1),main="Turns Model",ylab="Probability",xlab="Trial")
      lines(sIdx,empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col='black',type='l',ylim=c(0,1))
      
      plot(sIdx,Hybrid1Mat[which(Hybrid1Mat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col=cols[4],type='l',ylim=c(0,1),main="Hybrid1 Model",ylab="Probability",xlab="Trial")
      lines(sIdx,empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col='black',type='l',ylim=c(0,1))
      
      plot(sIdx,Hybrid2Mat[which(Hybrid2Mat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col=cols[5],type='l',ylim=c(0,1),main="Hybrid2 Model",ylab="Probability",xlab="Trial")
      lines(sIdx,empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col='black',type='l',ylim=c(0,1))
      
      plot(sIdx,Hybrid3Mat[which(Hybrid3Mat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col=cols[6],type='l',ylim=c(0,1),main="Hybrid3 Model",ylab="Probability",xlab="Trial")
      lines(sIdx,empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col='black',type='l',ylim=c(0,1))
      
      plot(sIdx,Hybrid4Mat[which(Hybrid4Mat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col=cols[7],type='l',ylim=c(0,1),main="Hybrid4 Model",ylab="Probability",xlab="Trial")
      lines(sIdx,empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],col='black',type='l',ylim=c(0,1))
      
      if(state==1)
      {
        box = "E"
      }
      else
      {
        box = "I"
      }
      title(paste("Path",act,", Box", box, ", ", rat,sep="" ),  cex=0.4,line = -2, outer = TRUE)
      # legend=c("Empirical", "Path Model","Turn Model","Hybrid1", "Hybrid2","Hybrid3","Hybrid4")
      # 
      # if(act==4||act==10){
      #   legend("bottomright", legend=legend,col=cols,cex=0.8,lty = c(1,1,1,1,1,1,1))
      #   
      # }else{
      #   legend("topright", legend=legend,col=cols,cex=0.8,lty = c(1,1,1,1,1,1,1))
      #   
      # }
      
      
      dev.off()
    }
  }
}


generateLikelihoodPlots=function(ratdata,allmodelRes,testData,plot.dir){
  
  allpaths = ratdata@allpaths
  
  
  
  TurnsLik = allmodelRes@Turns@aca2@likelihood
  PathsLik = allmodelRes@Paths@aca2@likelihood
  Hybrid1Lik = allmodelRes@Hybrid1@aca2@likelihood
  Hybrid2Lik = allmodelRes@Hybrid2@aca2@likelihood
  Hybrid3Lik = allmodelRes@Hybrid3@aca2@likelihood
  Hybrid4Lik = allmodelRes@Hybrid4@aca2@likelihood
  
  min_method = getMinimumLikelihood(ratdata,allmodelRes,testData,sim=2)
  if(grepl("Paths",min_method,fixed = TRUE))
  {
    minLik = PathsLik
  }
  else if(grepl("Hybrid1",min_method,fixed = TRUE))
  {
    minLik = Hybrid1Lik
  }
  else if(grepl("Hybrid2",min_method,fixed = TRUE))
  {
    minLik = Hybrid2Lik
  }
  else if(grepl("Hybrid3",min_method,fixed = TRUE))
  {
    minLik = Hybrid3Lik
  }
  else if(grepl("Hybrid4",min_method,fixed = TRUE))
  {
    minLik = Hybrid4Lik
  }
  else if(grepl("Turns",min_method,fixed = TRUE))
  {
    minLik = TurnsLik
  }
  
  rat=ratdata@rat
  endIndx = getEndIndex(rat,ratdata@allpaths,sim=2,limit=0.95)
  
  models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")

  for(model in models)
  {
    for(state  in c(1:2)){
      pdf(paste(plot.dir,"/Likelihoodplot_",rat,"_",model,"_State",state,".pdf",sep=""))
      par(mfrow=c(3,2))
      for(act  in c(1:6)){
        
        if(model == "Paths")
        {
          likVec = PathsLik
        }
        else if(model == "Hybrid1")
        {
          likVec = Hybrid1Lik
        }
        else if(model == "Hybrid2")
        {
          likVec = Hybrid2Lik
        }
        else if(model == "Hybrid3")
        {
          likVec = Hybrid3Lik
        }
        else if(model == "Hybrid4")
        {
          likVec = Hybrid4Lik
        }
        else if(model == "Turns")
        {
          likVec = TurnsLik
        }
        
        cols=c("blue","darkgreen","red", "darkmagenta","darkorange","deeppink")
        
        idx<-which(allpaths[,1]==act & allpaths[,2]==state)
        if(length(idx)>0)
        {
          plot(idx,exp(likVec[idx]),col=cols[1],type='l',ylim=c(0,1),ylab = "Probability", xlab="Trials", main=paste0("Path ", act))
          lines(idx, exp(minLik[idx]))
          
          abline(v=endIndx,lwd=2,lty=2)
          
          if(state==1)
          {
            box = "E"
          }
          else
          {
            box = "I"
          }
          
        }
        else
        {
          next
        }
        
      }
      
      title(paste("Likelihoodplot, box=", box, ", ", model, ", ", rat,sep="" ),  cex=0.4,line = -2, outer = TRUE)
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


generateEmpiricalPlots=function(ratdata,window){
  allpaths = ratdata@allpaths
  rle_sess = rle(allpaths[,5])
  last_paths<-cumsum(rle_sess$lengths)
  allpaths1<-allpaths[-last_paths,]
  empiricalProbMat = getEmpProbMat(allpaths1,window)
  endIdx = getEndIndex(ratdata@allpaths,sim=2,limit=0.95)
  sessInfo<-ratdata@allpaths[,5:6]
  act=4
  state=1
  actIds <- which(empiricalProbMat[,(act+6*(state-1))]>-1)
  sessInfoIds<-which(sessInfo[,2] %in% empiricalProbMat[actIds,13])
  r<-rle(sessInfo[sessInfoIds,1])
  plot(empiricalProbMat[which(empiricalProbMat[,(act+6*(state-1))]>-1),(act+6*(state-1))],ylim=c(0,1),ylab="Probability", xaxt='n', xlab='Session Nb',type='l',,cex.lab=1.3)
  axis(1, line=0,at=cumsum(r$lengths), labels = r$values)
  abline(v=cumsum(r$lengths), lty=3)
  endLearningIdx = findInterval(endIdx, sessInfoIds)
  abline(v=endLearningIdx,col='red',lwd=3)
  
}

plotSuccessRates=function(ratDataList)
{
  successRateList = list()
  for(i in c(2:7))
  {
    ratdata = ratDataList[[i]]
    rat = ratdata@rat
    sessions = unique(ratdata@allpaths[,5])
    successRate = numeric()
    for(sess in sessions)
    {
      idx = which(ratdata@allpaths[,5]== sess)
      rewards = sum(ratdata@allpaths[idx,3])
      sess_success = rewards/length(idx)
      successRate = c(successRate,sess_success)
    }
    successRateList[[i]] = successRate
  }
  maxlen <- max(lengths(successRateList))
  successRateList2 <- lapply(successRateList, function(lst) c(lst, rep(0, maxlen - length(lst))))
  df <- data.frame(matrix(unlist(successRateList2), ncol=length(successRateList2), byrow=FALSE))
  df[df==0] <- NA
  pdf(file=paste("SuccessRate.pdf",sep=""),width=11, height=7)
  colors=c("black","red","blue","green","orange","violet")
  matplot(df[,2:7],type='l',col=colors,lty=c(1,1,1,1,1,1),lwd=c(2,2,2,2,2), xlab = "Session", ylab="", cex.lab=1.6,cex.axis=1.5)
  title(ylab="Success Rate", line=2.95, cex.lab=1.6,cex.axis=1.5)
  legend=c("rat1", "rat2","rat3","rat4", "rat5","rat6")
  legend("bottomright", legend=legend, cex=1.7, col=colors, lwd = c(2,2,2,2,2,2),lty=c(1,1,1,1,1,1), bg="white")
  dev.off()
}

plotThetaHat=function(ratdata,res.dir,plot.dir)
{
  rat=ratdata@rat
  end_index80 = getEndIndex2(ratdata@allpaths, sim=2, limit=0.80)
  setwd(res.dir)
  paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
  load(paramTestData)
  setwd(plot.dir)
  pdf(file=paste("ParameterTest_",rat,".pdf",sep=""),width=8, height=8)
  par(mfrow=c(3,2))
  models <- c("Paths", "Hybrid1", "Hybrid2", "Hybrid3", "Hybrid4", "Turns")
  for(i in c(1:6))
  {
    rowEnd = paramTest[[i]][[1]][,1]
    alpha = paramTest[[i]][[1]][,2]
    gamma1 = paramTest[[i]][[1]][,3]
    #gamma2 = paramTest[[i]]$model@gamma2
    model = models[i]
    plot(rowEnd, alpha,type='l',ylim = c(0,1),col='black', ylab = "Parameter value",xlab="Trials", main=model,lty=1,lwd=1,cex.axis = 1.5, cex.lab = 1.3)
    lines(rowEnd, gamma1,type='l',col='red',lty=1,lwd=1)
    
    #abline(v=end_index80,col='green',lty=1,lwd=2)
 
  }
 
   par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
   plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')

  legend=c(expression(alpha), expression(gamma))
  legend("bottom", legend=legend, cex=1.5, col=c("black","red"), lwd = c(1,1),lty=c(1,1),horiz=T,xpd = T)

  
  #legend=c(expression(hat(alpha)), expression(alpha),expression(hat(gamma[1])), expression(gamma[1]),expression(hat(gamma[2])), expression(gamma[2])) 
  #legend("center", legend=legend, cex=1.5, col=c("black","black","green","green","red","red"), lwd = c(1,2,1,2,1,2),lty=c(1,2,1,2,1,2),horiz=FALSE,y.intersp=1.2)
  #title(paste0("Parameter estimation, ",rat), line = -1, outer = TRUE)
  #par(xpd=FALSE)
  dev.off()
}

getCI=function(X)
{
  if(length(X) < 30)
  {
    X.mean <- mean(X)
    X.sd <- sd(X)
    X.se <- X.sd/sqrt(length(X))
    alpha = 0.95
    degrees.freedom = length(X) - 1
    t.score = qt(0.975, df=degrees.freedom)
    #print(t.score)
    
    margin.error <- t.score * X.se
    lower.bound <- X.mean - margin.error
    upper.bound <- X.mean + margin.error
    
  }
  else
  {
    X.mean <- mean(X)
    X.sd <- sd(X)
    X.se <- X.sd/sqrt(length(X))
    margin.error <- qnorm(0.975)*X.se
    lower.bound <- X.mean - margin.error
    upper.bound <- X.mean + margin.error
  }
  return(c(lower.bound,upper.bound))
}
  

plotSimParamEstimation=function(ratdata,res.dir,plot.dir)
{
  rat=ratdata@rat
  setwd(res.dir)
  dfData=list.files(".", pattern=paste0(rat,".*ParamEs_Stability_df.Rdata"), full.names=FALSE)
  eightyCI <- getSimLearningEndIndices(rat,dfData,res.dir)
  listDfData <- list()
  for(i in c(1:length(dfData)))
  {
    print(dfData[i])
    load(dfData[i])
    listDfData[[i]] <- df
  }
  dfcombined <- bind_rows(listDfData)
  #load(paramTestData)
  #setwd(plot.dir)
  pdf(file=paste(plot.dir,"/","SimParamEstimation_",rat,".pdf",sep=""),width=8, height=8)
  models <- c("Paths", "Hybrid1", "Hybrid2", "Hybrid3", "Hybrid4", "Turns")
  #par(mfrow=c(2,2))
  
  #layout( matrix(c(1,3,0,2,0,0),ncol=2,nrow=3), heights=c(2,2,0.2) ) 
  layout( matrix(c(1,3,0,2,4,0),ncol=2,nrow=3), heights=c(2,2,0.2) )
 #layout( matrix(c(1,0,1,0),ncol=2,nrow=2), heights=c(1,0.2) )

  iterations=as.integer(floor(length(ratdata@allpaths[,1])/100))
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

  for(model in models)
  {
    dfModel <- dfcombined[which(dfcombined[,1]==model),]
    nbSims <- length(which(dfModel[,2]==maxVecs[1]))
    print(sprintf("model=%s, nbSims=%i",model,nbSims)) 
    if(nbSims > 0)
    {
     alpha_upper_bounds <- c()
     alpha_lower_bounds <- c()
     gamma_upper_bounds <- c()
     gamma_lower_bounds <- c()
     for(iter in maxVecs)
     {
       #if(iter==iterations)
       #{
         #rowEnd = length(ratdata@allpaths[,1])
       #   rowEnd = iter*100
       #}else{
       #  rowEnd = iter*100
       #}
       rowEnd = iter
       simulation_alphas <- dfModel[which(dfModel[,2]==rowEnd),4]
       simulation_gammas <- dfModel[which(dfModel[,2]==rowEnd),5]

       alpha_bounds <- getCI(simulation_alphas)


       alpha_upper_bounds <- c(alpha_upper_bounds,alpha_bounds[2])
       alpha_lower_bounds <- c(alpha_lower_bounds,alpha_bounds[1])

       gamma_bounds <- getCI(simulation_gammas)

       gamma_upper_bounds <- c(gamma_upper_bounds,gamma_bounds[2])
       gamma_lower_bounds <- c(gamma_lower_bounds,gamma_bounds[1])

  #     if(any(is.nan(alpha_bounds)) || any(is.nan(gamma_bounds)))
  #     {
  #       print(sprintf("NaN found, model=%s, rowEnd=%i",model,rowEnd))
  #       print(sprintf("alpha_bounds:"))
  #       print(alpha_bounds)
  #       print(sprintf("gamma_bounds:"))
  #       print(gamma_bounds)
  #
  #     } 
       
  #     if(any((gamma_bounds)<0) || any((gamma_bounds)>1))
  #     {
  #       print(sprintf("gamma out of range, model=%s, rowEnd=%i, mean=%f, simulation_gammas:",model,rowEnd, mean(simulation_gammas)))
  #       #print(sprintf("alpha_bounds:"))
  #       print(simulation_gammas)
  #       print(sprintf("gamma_bounds:"))
  #       print(gamma_bounds)
  #
  #     }
     }
     
     paramTestData=list.files(".", pattern=paste0(rat,".*.ParamRes.Rdata"), full.names=FALSE)
     print(paramTestData)
     load(paramTestData)
     index = which(models %in% model)
     print(sprintf("model=%s, index=%i", model, index))
     resMat <- paramTest[[index]][[1]]
     n.rows <- length(resMat[,1])
     alpha =  resMat[n.rows,2]
     gamma1 = resMat[n.rows,3]
    
     true80 <- getEndIndex2(ratdata@allpaths,sim=2,limit=0.8)
     upperbound80 <- eightyCI[[index]][2]
     lowerbound80 <- eightyCI[[index]][1]

     print(sprintf("alpha=%f,gamma1=%s, model=%s, index=%i",alpha,gamma1,model,index))
     #print(sprintf("alpha_upper_bounds len = %i",length(alpha_upper_bounds)))
     #print(sprintf("gamma_upper_bounds:"))
     #print(gamma_upper_bounds)
     #print(sprintf("gamma_lower_bounds:"))
     #print(gamma_lower_bounds)
     #xaxis <- c(1:iterations)*100
     xaxis <- maxVecs
     title <- paste(model, ", nbSim=",nbSims,collapse="")
     plot(xaxis,alpha_upper_bounds,type ='l',lty=2,col="black",ylim=c(0,1),main=title,xlab="Trials",ylab="Parameters",cex.axis = 1.5, cex.lab = 1.3)
     lines(xaxis,alpha_lower_bounds, lty=2, col="black")
     lines(xaxis,gamma_upper_bounds, lty=2, col='red')
     lines(xaxis,gamma_lower_bounds, lty=2, col='red')
     abline(h=alpha,col="black")
     abline(h=gamma1, col='red')
     
     #abline(v=true80,col='green')  
     #abline(v=lowerbound80, col='green', lty=2)
     #abline(v=upperbound80, col='green', lty=2)
     
     #ggplot((dfModel[,c(2,4,5)]),aes(x=iter,y=alpha)) + geom_boxplot(aes(fill=factor(iter)))
     #ggplot((dfModel[,c(2,4,5)]),aes(x=iter,y=gamma)) + geom_boxplot(aes(fill=factor(iter)))

    }  
    
  }
 
   par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
   plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')

  #par(mar=c(0, 0, 0, 0))
  #plot.new()

  legend=c(expression(alpha), expression(paste(alpha, " conf_int")), expression(gamma), expression(paste(gamma, " conf_int")))
  legend('bottom', legend=legend, cex=1.5, col=c("black","black","red","red"), lwd = c(1,1,1,1),lty=c(1,2,1,2),horiz=T,xpd = T)
 
  #legend=c(expression(alpha),expression(gamma[1])) 
  #legend("center", legend=legend, cex=1.5, col=c("black","red"), lwd = c(1,1),lty=c(1,1),horiz=FALSE,y.intersp=1.2)
  #title(paste0("Param comparison, model=",model," ", rat), line = -1, outer = TRUE)
  #par(xpd=FALSE)
  dev.off()
  
}





getSimLearningEndIndices=function(rat, dfData, res.dir)
{
  rat = ratdata@rat
  setwd(res.dir)
  dfData=list.files(".", pattern=paste0(rat,".*ParamEs_df.Rdata"), full.names=FALSE)
  PathIndices <- c()
  Hybrid1Indices <- c()
  Hybrid2Indices <- c()
  Hybrid3Indices <- c()
  Hybrid4Indices <- c()
  TurnsIndices <- c()
  
  for(k in c(1:length(dfData)))
  {
    load(dfData[k])
    allData<-unlist(generatedDataList)
    
    for(i in c(1:length(allData)))
    {
      generated_data = allData[[i]]
      end_index = getEndIndex2(generated_data@allpaths,sim=1,limit = 0.80)
      model = allData[[i]]@simModel
      if(model == "Paths")
      {
        PathIndices <- c(PathIndices,end_index)
      }
      else if(model == "Hybrid1")
      {
        Hybrid1Indices <- c(Hybrid1Indices,end_index)
      }
      else if(model == "Hybrid2")
      {
        Hybrid2Indices <- c(Hybrid2Indices,end_index)
      }
      else if(model == "Hybrid3")
      {
        Hybrid3Indices <- c(Hybrid3Indices,end_index)
      }
      else if(model == "Hybrid4")
      {
        Hybrid4Indices <- c(Hybrid4Indices,end_index)
      }
      else if(model == "Turns")
      {
        TurnsIndices <- c(TurnsIndices,end_index)
      }
      
    }
    
  }
  modelIndices <- list(PathIndices,Hybrid1Indices,Hybrid2Indices,Hybrid3Indices,Hybrid4Indices,TurnsIndices)
  modelCI <- list()
  for(j in c(1:6))
  {
    if(j==2)
    {
     print(modelIndices[[j]])
    }
    bounds <- getCI(modelIndices[[j]])
    modelCI[[j]] <- bounds
    
  }
   return(modelCI)
  
}

plotSimProbBoxPlots=function(ratdata,res.dir,plot.dir)
{
  rat=ratdata@rat
  setwd(res.dir)
  dfData=list.files(".", pattern=paste0(rat,".*ParamEs_df.Rdata"), full.names=FALSE)
  combinedResList <- list()
  for(i in c(1:length(dfData)))
  {
    print(dfData[i])
    load(dfData[i])
    combinedResList <- append(combinedResList,resList)
  }
  #combinedResList <- bind_rows(listDfData)
  #load(paramTestData)
  #setwd(plot.dir)
  #pdf(file=paste(plot.dir,"/","SimParamEstimation_",rat,".pdf",sep=""),width=11, height=7)
  
  
  
  #models <- c("Paths", "Hybrid1", "Hybrid2", "Hybrid3", "Hybrid4", "Turns")
  #par(mfrow=c(3,2))
  
  #iterations=as.integer(floor(length(ratdata@allpaths[,1])/100))
  
  PathProbMat <- matrix(0,,14)
  Hybrid1ProbMat <- matrix(0,,14)
  Hybrid2ProbMat <- matrix(0,,14)
  Hybrid3ProbMat <- matrix(0,,14)
  Hybrid4ProbMat <- matrix(0,,14)
  TurnsProbMat <- matrix(0,,14)

  colNames <- c("straight.LF","reverse.LF","v.LF","good.LF","loop.LF","backward loop.LF","straight.RF","reverse.RF","v.RF","good.RF","loop.RF","backward loop.RF","iter","model")

  colnames(PathProbMat) <- c("straight.LF","reverse.LF","v.LF","good.LF","loop.LF","backward loop.LF","straight.RF","reverse.RF","v.RF","good.RF","loop.RF","backward loop.RF","iter","model")
  colnames(Hybrid1ProbMat) <- colNames 
  colnames(Hybrid2ProbMat) <- colNames
  colnames(Hybrid3ProbMat) <- colNames
  colnames(Hybrid4ProbMat) <- colNames
  colnames(TurnsProbMat) <- colNames
 

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
 
  for(k in c(1:length(combinedResList)))
  {
    iter = combinedResList[[k]]$iter
    print(sprintf("iter=%i",iter))
    modelDataRes = combinedResList[[k]]$res
    model = modelDataRes@Model
    probRow = combinedResList[[k]]$probRow
    probRow = probRow[1:12]
    probRow[13] = iter
    probRow[14] = model
    print(sprintf("model=%s, iter=%i",model,iter))
    if(model == "Paths")
    {
      PathProbMat = rbind(PathProbMat,probRow)
    }
    else if(model == "Hybrid1")
    {
      Hybrid1ProbMat = rbind(Hybrid1ProbMat,probRow)
    }
    else if(model == "Hybrid2")
    {
      Hybrid2ProbMat = rbind(Hybrid2ProbMat,probRow)
    }
    else if(model == "Hybrid3")
    {
      Hybrid3ProbMat = rbind(Hybrid3ProbMat,probRow)
    }
    else if(model == "Hybrid4")
    {
      Hybrid4ProbMat = rbind(Hybrid4ProbMat,probRow)
    }
    else if(model == "Turns")
    {
      TurnsProbMat = rbind(TurnsProbMat,probRow)
    }
    
  }
  
  PathProbMat.df <- as.data.frame(PathProbMat)
  Hybrid1ProbMat.df <- as.data.frame(Hybrid1ProbMat)
  Hybrid2ProbMat.df <- as.data.frame(Hybrid2ProbMat)
  Hybrid3ProbMat.df <- as.data.frame(Hybrid3ProbMat)
  Hybrid4ProbMat.df <- as.data.frame(Hybrid4ProbMat)
  TurnsProbMat.df <- as.data.frame(TurnsProbMat) 
  
  PathProbMat.df[,1:13] <- lapply(PathProbMat.df[,1:13], function(x) as.numeric(as.character(x)))
  Hybrid1ProbMat.df[,1:13] <- lapply(Hybrid1ProbMat.df[,1:13], function(x) as.numeric(as.character(x)))
  Hybrid2ProbMat.df[,1:13] <- lapply(Hybrid2ProbMat.df[,1:13], function(x) as.numeric(as.character(x)))
  Hybrid3ProbMat.df[,1:13] <- lapply(Hybrid3ProbMat.df[,1:13], function(x) as.numeric(as.character(x)))
  Hybrid4ProbMat.df[,1:13] <- lapply(Hybrid4ProbMat.df[,1:13], function(x) as.numeric(as.character(x)))
  TurnsProbMat.df[,1:13] <- lapply(TurnsProbMat.df[,1:13], function(x) as.numeric(as.character(x)))
  
  X<-do.call("rbind", list(PathProbMat.df[-1,],Hybrid1ProbMat.df[-1,],Hybrid2ProbMat.df[-1,],Hybrid3ProbMat.df[-1,],Hybrid4ProbMat.df[-1,],TurnsProbMat.df[-1,]))
 
  iter_unique = unique(X[,13])
  print(sprintf("iter_unique=%s",paste(iter_unique, collapse=" "))) 
  X.melt<- melt(X,id.vars = c("iter","model"))
  
  #p <- ggplot(data = PathProbMat.df)+geom_boxplot(aes(x=as.factor(iter), y=value))+facet_wrap(~as.factor(variable), nrow=5)
  #p <- ggplot(data = X.melt)+geom_boxplot(aes(x=as.factor(iter), y=value),outlier.size = 0.1)+facet_grid(model ~ variable)+theme(
  #  strip.background = element_blank(),
  #  axis.text.x = element_blank()
  #)+labs(y = "Probability Difference", x = "Paths")  

  p <- ggplot(data = X.melt)+geom_boxplot(aes(x=as.factor(iter), y=value),,outlier.size = 0.1)+facet_grid(model~variable,labeller = label_wrap_gen(width=10))+scale_x_discrete(labels=maxVecs,guide = guide_axis(n.dodge=4)) + theme(
    axis.text.x = element_text(size = 5))+labs(y = "Normalized Prob Err", x = "Trials") 

  pdf(paste(plot.dir,"/","BoxPlotSim_",rat,".pdf",sep=""),width=11, height=7)
  print(p)
  dev.off()
  
  
}

printMatRes=function(ratdata,testData,res.dir)
{
  rat=ratdata@rat
  print(sprintf("Rat is %s", rat))
  #models = testData@Models
  #creditAssignment = testData@creditAssignment
  
  #modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))
  modelNames = testData@Models
  mat_res_f = matrix(0, length(modelNames), length(modelNames))
  colnames(mat_res_f) <- modelNames
  rownames(mat_res_f) <- modelNames
  
  setwd(res.dir)
  dfData=list.files(".", pattern=paste0(rat,".*mat_res.Rdata"), full.names=FALSE)
  #mat_res_f <- matrix()
  for(i in c(1:length(dfData)))
  {
    load(dfData[i])
    mat_res_f <- mat_res_f + mat_res
  }
  print(mat_res_f)
  
}

boxDistances = function(path,state)
{
  if(path == 4 & state == 1)
  {
    f = 39.16
    g = 50
    a = 44.5
    b = 85
    c = 42
    h = 36
    return(list(f=f,g=g,a=a,b=b,c=c,h=h))
  }
  else if(path == 4 & state == 2)
  {
    j = 39.16
    k = 50
    a = 44.5
    b = 85
    c = 42
    d = 35
    return(list(j=j,k=k,a=a,b=b,c=c,d=d))
  }
  else if(path == 5 & state == 1)
  {
    f = 39.16
    g = 50
    a = 44.5
    b = 85
    c = 42
    d = 35
    return(list(f=f,g=g,a=a,b=b,c=c,d=d))
  }
  else if(path == 5 & state == 2)
  {
    j = 39.16
    k = 50
    a = 44.5
    b = 85
    c = 42
    h = 36
    return(list(j=j,k=k,a=a,b=b,c=c,h=h))
  }
}

plotRatSpeed = function(ratDataList, ratboxList,plot.dir)
{
  #pdf(paste(plot.dir,"/BoxSpeeds_",rat,".pdf",sep=""))
  #par(mfrow=c(2,2))
  #ratcolor = c("black","red","blue","green","orange")
  Paths = c( "fgabch", "fgabcd", "jkabcd","jkabch")
  PathNb = rbind(c(4, 1), c(5, 1), c(4, 2), c(5, 2))
  for(path in c(1:4))
  {
    
    pdf(file=paste("Speeds_",Paths[path],".pdf",sep=""),width=11, height=11)
    df <- data.frame(boxes= character(6), rat_103= numeric(6), rat_106= numeric(6),rat_112= numeric(6),rat_113= numeric(6),rat_114= numeric(6))
    mazeboxes <- strsplit(Paths[path], "")[[1]]
    df[,1] = mazeboxes
    
    
    for(r in c(2:6))
    {
      ratdata <- ratDataList[[r]]
      ratboxes <- ratboxList[[r]]
      rat = ratdata@rat
      
      endIdx = getEndIndex(ratdata@allpaths,sim=2,limit=0.95)
      sessions = unique(ratdata@allpaths[,5])
      learningEndSess = ratdata@allpaths[endIdx,5]
      learningStageLen = length(sessions[sessions <= learningEndSess])
      #stage1 = sessions
      stage1 = sessions[sessions<=(learningStageLen)]
      stage2 = sessions[sessions>(learningStageLen)]
      
      
      
      corrPathIdx = which(ratdata@allpaths[, 1] == PathNb[path,1] & ratdata@allpaths[, 2] == PathNb[path,2])
      if(length(corrPathIdx)<2)
      {
        next
      }
      corrPathList = ratdata@allpaths[corrPathIdx, ]
      boxSpeed = matrix(0,,(nchar(Paths[path])+1))
      for (i in 1:length(corrPathList[,1]))
      {
        speeds <- c()
        sessNb = corrPathList[i, 5]
        if (corrPathIdx[i] == 1)
        {
          boxIdStart = 1
        }
        else
        {
          boxIdStart = ratdata@allpaths[(corrPathIdx[i] - 1), 7]
        }
        
        boxIdEnd = ratdata@allpaths[corrPathIdx[i], 7]
        if(boxIdEnd < boxIdStart)
        {
          boxIdStart = 1
        }
        
        boxData <- ratboxes[[sessNb]]$tab[boxIdStart:boxIdEnd, ]
        
        boxDist <- boxDistances(PathNb[path,1],PathNb[path,2])
        for(box in mazeboxes)
        {
          boxId = which(names == box)
          boxTime = boxData[which(boxData[,3] == boxId),2] - boxData[which(boxData[,3] == boxId),1]
          if(boxTime == 0)
          {
            speeds <- c(speeds,NA) 
          }
          else
          {
            speeds <- c(speeds,(boxDist[[box]]*100/boxTime))
            #speeds <- c(speeds,(boxTime/100))
          }
          
        }
        speeds <- c(speeds, ratdata@allpaths[corrPathIdx[i], 5])
        boxSpeed <- rbind(boxSpeed, speeds)
      }
      
      stage1Idx = which(boxSpeed[,ncol(boxSpeed)] %in% stage1)
      stage2Idx = which(boxSpeed[,ncol(boxSpeed)] %in% stage2)
      #stage3Idx = which(boxSpeed[,ncol(boxSpeed)] %in% stage3)
      meanSpeed1 <-  colMeans(boxSpeed[stage1Idx,(1:nchar(Paths[path]))],na.rm = TRUE)
      meanSpeed2 <-  colMeans(boxSpeed[stage2Idx,(1:nchar(Paths[path]))],na.rm = TRUE)
      #meanSpeed3 <-  colMeans(boxSpeed[stage3Idx,(1:nchar(Paths[path]))],na.rm = TRUE)
      #max = max(meanSpeed1,meanSpeed2,meanSpeed3)
      max = max(meanSpeed1,meanSpeed2)
      
      #g <- g + geom_bar(aes(x=boxes,y=meanSpeed2), stat="identity", position ="identity", alpha=.6, fill=fillcolor[r])
      #lines(meanSpeed2,col='red')
      df[,r] = meanSpeed2
      
    }
    
    #stage2 = sessions[sessions>(learningStageLen/2) & sessions <= learningStageLen]
    #stage3 = sessions[sessions > learningStageLen]   
    
    #lines(meanSpeed3,col='blue')
    #legend=c(paste0(range(stage1)[1],"-",range(stage1)[2]), paste0(range(stage2)[1],"-",range(stage2)[2],"*"), paste0(range(stage3)[1],"-",range(stage3)[2]))
    #legend=c(paste0(range(stage1)[1],"-",range(stage1)[2]), paste0(range(stage2)[1],"-",range(stage2)[2]))
    
    #legend("topright", legend=legend, cex=0.7, col=c("black","red","blue"), lty=c(1,1,1),title="Sessions")
    
    melted<-melt(df, id="boxes")
    #print(ggplot(melted,aes(x=boxes,y=value,fill=variable)) + geom_bar(stat="identity", position = "dodge", alpha=.8))
    print(ggplot(melted,aes(x=variable,y=value,fill=factor(boxes,levels=mazeboxes))) + geom_bar(stat="identity", position = "dodge", alpha=.8)+  labs(fill = "Boxes"))
    dev.off()
  }
  
}


plotTurnProb=function(ratdata,allmodelRes,testModel)
{
  rat = ratdata@rat
  testModelName = testModel@Name
  modelData = slot(allmodelRes,testModelName)@aca3
  S0.nodes <- testModel@nodes.S0[-1]
  S1.nodes <- testModel@nodes.S1[-1]
  mat <- TurnsNew::getAca3ProbMatrix(ratdata,modelData,testModel,sim=2,debug = F)
  
  pdf(paste(plot.dir,"/TurnProb_",rat, "_State_E.pdf",sep=""))
  par(mfrow=c(2,4))
  for(i in c(1:7))
  {
    plot(mat[which(mat[,i]>-1),i],main=S0.nodes[i],type='l',xaxt='n',ylab='Prob',xlab="")
    axis(1, line=0,at=seq(1,length(which(mat[,i]>-1)),by=100), labels = mat[which(mat[,i]>-1),15][seq(1,length(which(mat[,i]>-1)),by=100)])
    
  }
  mtext(paste0(rat,", State 1"), side = 3, line = -2, outer = TRUE,cex=0.8)
  dev.off()
  
  pdf(paste(plot.dir,"/TurnProb_",rat, "_State_I.pdf",sep=""))
  par(mfrow=c(2,4))
  for(i in c(1:7))
  {
    col = 7+i
    plot(mat[which(mat[,col]>-1),col],main=S1.nodes[i],type='l',xaxt='n',ylab='Prob',xlab="")
    axis(1, line=0,at=seq(1,length(which(mat[,col]>-1)),by=100), labels = mat[which(mat[,col]>-1),15][seq(1,length(which(mat[,col]>-1)),by=100)])
    
  }
  mtext(paste0(rat,", State 2"), side = 3, line = -2, outer = TRUE)
  dev.off()
}

plotPCA=function(ratdata,allmodelRes)
{
  rat=ratdata@rat
  ## All 6 models together
  #pdf(file=paste("PCA_",rat,".pdf",sep=""),width=11, height=11,onefile=FALSE)
  for(state in c(1,2))
  {
    if (state==1) pdf(file=paste("PCA_",rat,"boxE.pdf",sep=""),width=11, height=11,onefile=FALSE)
    if (state==2) pdf(file=paste("PCA_",rat,"boxI.pdf",sep=""),width=11, height=11,onefile=FALSE)    
    
    endIdx = getEndIndex(ratdata@allpaths,sim=2,limit=0.95)
    state_idx = which(ratdata@allpaths[,2] == state)
    state_idx = which(allmodelRes@Paths@aca3@probMatrix[,13] %in% state_idx)
    n=length(state_idx) 
    
    rangeEnd = endIdx/2
    
    if(state==1) colIdx= c(1:6)
    if(state==2) colIdx= c(7:12)
    
    X1=allmodelRes@Paths@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X2=allmodelRes@Turns@aca3@probMatrix[state_idx[state_idx> rangeEnd],colIdx]
    X3=allmodelRes@Hybrid1@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X4=allmodelRes@Hybrid2@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X5=allmodelRes@Hybrid3@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X6=allmodelRes@Hybrid4@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    
    X=rbind(X1,X2,X3,X4,X5,X6)
    Xtilde=apply(X,2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
    pca1=princomp(Xtilde)
    
    matLen = length(X1[,1])
    x <- seq(0, 1, length.out = matLen)
    yellows<-seq_gradient_pal("white", "yellow")(x)
    x <- seq(0, 1, length.out = matLen)
    violets<-seq_gradient_pal("white", "violet")(x)
    r1 = c(1:matLen)
    r2 = c((matLen+1):(2*matLen))
    r3 = c((2*matLen+1):(3*matLen))
    r4 = c((3*matLen+1):(4*matLen))
    r5 = c((4*matLen+1):(5*matLen))
    r6 = c((5*matLen+1):(6*matLen))
    
    p1<-ggplot() +  geom_path(data = as.data.frame(pca1$scores[r2,1:2]),size=2, aes(x = pca1$scores[r2,1], y = pca1$scores[r2,2],color=r1))+ scale_colour_gradientn(name ="Turns",guide = guide_colourbar(direction = "vertical"), colors=brewer.pal(5, "Reds"))  + new_scale_color()+
      geom_path(data = as.data.frame(pca1$scores[r3,1:2]),size=2, aes(x = pca1$scores[r3,1], y = pca1$scores[r3,2],color=r1))+ scale_colour_gradientn(name ="Hybrid1",guide = guide_colourbar(direction = "vertical"), colors=brewer.pal(5, "Blues")) + new_scale_color()+
      geom_path(data = as.data.frame(pca1$scores[r4,1:2]),size=2, aes(x = pca1$scores[r4,1], y = pca1$scores[r4,2],color=r1))+ scale_colour_gradientn(name ="Hybrid2",guide = guide_colourbar(direction = "vertical"), colors=brewer.pal(5, "Greys")) + new_scale_color()+
      geom_path(data = as.data.frame(pca1$scores[r5,1:2]),size=2, aes(x = pca1$scores[r5,1], y = pca1$scores[r5,2],color=r1))+ scale_colour_gradientn(name ="Hybrid3",guide = guide_colourbar(direction = "vertical"), colors=violets) + new_scale_color()+
      geom_path(data = as.data.frame(pca1$scores[r6,1:2]),size=2, aes(x = pca1$scores[r6,1], y = pca1$scores[r6,2],color=r1))+ scale_colour_gradientn(name ="Hybrid4",guide = guide_colourbar(direction = "vertical"), colors=yellows)+ new_scale_color()+
      geom_path(data = as.data.frame(pca1$scores[r1,1:2]), size=2, aes(x = pca1$scores[r1,1], y = pca1$scores[r1,2],color=r1))+scale_colour_gradientn(name ="Path",guide = guide_colourbar(direction = "vertical"), colours = brewer.pal(5, "Greens")) +
      geom_point(data=as.data.frame(pca1$scores[r1[1],1:2]), mapping=aes(x=pca1$scores[r1[1],1],y=pca1$scores[r1[1],2]), colour="green", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca1$scores[r2[1],1:2]), mapping=aes(x=pca1$scores[r2[1],1],y=pca1$scores[r2[1],2]), colour="red", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca1$scores[r3[1],1:2]), mapping=aes(x=pca1$scores[r3[1],1],y=pca1$scores[r3[1],2]), colour="blue", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca1$scores[r4[1],1:2]), mapping=aes(x=pca1$scores[r4[1],1],y=pca1$scores[r4[1],2]), colour="grey", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca1$scores[r5[1],1:2]), mapping=aes(x=pca1$scores[r5[1],1],y=pca1$scores[r5[1],2]), colour="violet",shape = 17,  size=4)+
      geom_point(data=as.data.frame(pca1$scores[r6[1],1:2]), mapping=aes(x=pca1$scores[r6[1],1],y=pca1$scores[r6[1],2]), colour="yellow", shape = 17, size=4)+
      xlab("Component 1") +     ylab("Component 2") + theme(legend.direction = "vertical", legend.box = "horizontal",legend.position = "right")+ggtitle("All models")+  theme(plot.title = element_text(hjust = 0.5))
    
    
    
    ## 3 models together - Path, Hybrid2, Hybrid3
    X1=allmodelRes@Paths@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X4=allmodelRes@Hybrid2@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X5=allmodelRes@Hybrid3@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    
    X=rbind(X1,X4,X5)
    Xtilde=apply(X,2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
    pca2=princomp(Xtilde)
    
    matLen = length(X1[,1])
    x <- seq(0, 1, length.out = matLen)
    yellows<-seq_gradient_pal("white", "yellow")(x)
    x <- seq(0, 1, length.out = matLen)
    violets<-seq_gradient_pal("white", "violet")(x)
    r11 = c(1:matLen)
    r22 = c((matLen+1):(2*matLen))
    r33 = c((2*matLen+1):(3*matLen))
    
    p2<-ggplot() + 
      geom_path(show.legend = FALSE,data = as.data.frame(pca2$scores[r22,1:2]),size=2, aes(x = pca2$scores[r22,1], y = pca2$scores[r22,2],color=r11))+ scale_colour_gradientn(name ="Hybrid2",guide = guide_colourbar(direction = "horizontal"), colors=brewer.pal(5, "Greys")) + new_scale_color()+
      geom_path(show.legend = FALSE,data = as.data.frame(pca2$scores[r33,1:2]),size=2, aes(x = pca2$scores[r33,1], y = pca2$scores[r33,2],color=r11))+ scale_colour_gradientn(name ="Hybrid3",guide = guide_colourbar(direction = "horizontal"), colors=violets) + new_scale_color()+
      geom_path(show.legend = FALSE,data = as.data.frame(pca2$scores[r11,1:2]), size=2, aes(x = pca2$scores[r11,1], y = pca2$scores[r11,2],color=r11))+scale_colour_gradientn(name ="Path",guide = guide_colourbar(direction = "horizontal"), colours = brewer.pal(5, "Greens")) +
      geom_point(data=as.data.frame(pca2$scores[r11[1],1:2]), mapping=aes(x=pca2$scores[r11[1],1],y=pca2$scores[r1[1],2]), colour="green", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca2$scores[r22[1],1:2]), mapping=aes(x=pca2$scores[r22[1],1],y=pca2$scores[r2[1],2]), colour="grey", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca2$scores[r33[1],1:2]), mapping=aes(x=pca2$scores[r33[1],1],y=pca2$scores[r3[1],2]), colour="violet", shape = 17, size=4)+
      xlab("Component 1") +     ylab("Component 2") + ggtitle("Paths, Hybrid2 and Hybrid3")+  theme(plot.title = element_text(hjust = 0.5))
    
    
    ## 2 models together - Path, Hybrid2
    
    X1=allmodelRes@Paths@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X4=allmodelRes@Hybrid2@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    
    X=rbind(X1,X4)
    Xtilde=apply(X,2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
    pca3=princomp(Xtilde)
    
    matLen = length(X1[,1])
    x <- seq(0, 1, length.out = matLen)
    yellows<-seq_gradient_pal("white", "yellow")(x)
    x <- seq(0, 1, length.out = matLen)
    violets<-seq_gradient_pal("white", "violet")(x)
    r1111 = c(1:matLen)
    r2222 = c((matLen+1):(2*matLen))
    
    p3<-ggplot() + 
      geom_path(show.legend = FALSE,data = as.data.frame(pca3$scores[r2222,1:2]),size=2, aes(x = pca3$scores[r2222,1], y = pca3$scores[r2222,2],color=r1111))+ scale_colour_gradientn(name ="Hybrid2",guide = guide_colourbar(direction = "horizontal"), colors=brewer.pal(5, "Greys")) + new_scale_color()+
      geom_path(show.legend = FALSE,data = as.data.frame(pca3$scores[r1111,1:2]), size=2, aes(x = pca3$scores[r1111,1], y = pca3$scores[r1111,2],color=r1111))+scale_colour_gradientn(name ="Path",guide = guide_colourbar(direction = "horizontal"), colours = brewer.pal(5, "Greens")) + 
      geom_point(data=as.data.frame(pca3$scores[r1111[1],1:2]), mapping=aes(x=pca3$scores[r1111[1],1],y=pca3$scores[r1111[1],2]), colour="green", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca3$scores[r2222[1],1:2]), mapping=aes(x=pca3$scores[r2222[1],1],y=pca3$scores[r2222[1],2]), colour="grey",shape = 17,  size=4)+
      xlab("Component 1") +     ylab("Component 2") + ggtitle("Paths and Hybrid2")+  theme(plot.title = element_text(hjust = 0.5))
    
    
    
    
    
    ## 2 models together - Path, Hybrid3 
    
    X1=allmodelRes@Paths@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    X5=allmodelRes@Hybrid3@aca3@probMatrix[state_idx[state_idx > rangeEnd],colIdx]
    
    X=rbind(X1,X5)
    Xtilde=apply(X,2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
    pca4=princomp(Xtilde)
    
    matLen = length(X1[,1])
    x <- seq(0, 1, length.out = matLen)
    yellows<-seq_gradient_pal("white", "yellow")(x)
    x <- seq(0, 1, length.out = matLen)
    violets<-seq_gradient_pal("white", "violet")(x)
    r111 = c(1:matLen)
    r222 = c((matLen+1):(2*matLen))
    
    p4<-ggplot() + 
      geom_path(show.legend = FALSE,data = as.data.frame(pca4$scores[r222,1:2]),size=2, aes(x = pca4$scores[r222,1], y = pca4$scores[r222,2],color=r111))+ scale_colour_gradientn(name ="Hybrid3",guide = guide_colourbar(direction = "horizontal"), colors=violets) + new_scale_color()+
      geom_path(show.legend = FALSE,data = as.data.frame(pca4$scores[r111,1:2]), size=2, aes(x = pca4$scores[r111,1], y = pca4$scores[r111,2],color=r111))+scale_colour_gradientn(name ="Path",guide = guide_colourbar(direction = "horizontal"), colours = brewer.pal(5, "Greens")) +
      geom_point(data=as.data.frame(pca4$scores[r111[1],1:2]), mapping=aes(x=pca4$scores[r111[1],1],y=pca4$scores[r111[1],2]), colour="green", shape = 17, size=4)+
      geom_point(data=as.data.frame(pca4$scores[r222[1],1:2]), mapping=aes(x=pca4$scores[r222[1],1],y=pca4$scores[r222[1],2]), colour="violet", shape = 17, size=4)+
      xlab("Component 1") +     ylab("Component 2") + ggtitle("Paths and Hybrid3")+  theme(plot.title = element_text(hjust = 0.5))
    
    
    
    #if(state==1) title = paste0(rat,", box E")
    #if(state==2) title = paste0(rat,", box I")
    
    legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")
    grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2+ theme(legend.position="none"),p3+ theme(legend.position="none"), p4+ theme(legend.position="none"),nrow=2),legend, ncol=1,heights=c(6,1))
    #grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2+ theme(legend.position="none"),p3+ theme(legend.position="none"), p4+ theme(legend.position="none"),nrow=2),legend, ncol=1,heights=c(6,1),top = textGrob(title,gp=gpar(fontsize=20,font=3)))
    
    dev.off()
  }
  
  #ggsave(paste("PCA_",rat,".pdf",sep=""),width=11, height=11)
}

plotPCA3=function(ratdata,res.dir,allModelRes,plot.dir)
{
  rat=ratdata@rat
  setwd(res.dir)
  dfData=list.files(".", pattern=paste0(rat,".*resList.Rdata"), full.names=FALSE)
  combinedResList <- list()
  for(i in c(1))
  {
    print(dfData[i])
    load(dfData[i])
    combinedResList <- append(combinedResList,resList)
  }
  allmodelRes <- new("AllModelRes")
  models.populated = c()
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  
  probMat_S1 <- data.frame(matrix(0,0,10))
  probMat_S2 <- data.frame(matrix(0,0,10))
  
  for(k in c(1:length(combinedResList)))
  {
    ##Extracting generator model from resList[[k]][[1]]
    genData = combinedResList[[k]][[1]]$data
    genModel = combinedResList[[k]][[1]]$data@simModel
    ## Find the index of modelData corresponding to generator model, retrieve the result of holdout cut at trial 800
    index = which(models==genModel)
    testModelData = combinedResList[[k]][[index]]$res
    testModel = testModelData@Model
    #print(sprintf("genModel=%s, k=%i, testModel=%s",genModel,k,testModel))
    if(!testModel %in% models.populated)
    {
      models.populated = c(models.populated,genModel)
      print(sprintf("testModel=%s",testModel))
      #modelData <- slot(slot(allmodelRes,"Paths"),"aca2")
      
      state1_idx = which(genData@allpaths[,2] == 0)
      state2_idx = which(genData@allpaths[,2] == 1)
      
      probMatS1_model=testModelData@probMatrix[state1_idx,c(1:6)]
      n=length(probMatS1_model[,1])
      probMatS1_model = cbind(probMatS1_model,rep(testModel,n),state1_idx,rep(0,n),rep("simulation",n))
      
      curr_idx = length(probMat_S1[,1])
      probMat_S1 <- rbind(probMat_S1, probMatS1_model)
      idx1 = curr_idx+1
      probMat_S1[idx1,9] = 1
      idx2 = curr_idx+min(which(state1_idx >= 801))
      probMat_S1[idx2,9] = 1
      idx3 = length(probMat_S1[,1])
      probMat_S1[idx3,9] = 1
      
      #print(sprintf("S1 idx1=%i,idx2=%i",idx1,idx2))
      
      
      probMatS2_model=testModelData@probMatrix[state2_idx,c(7:12)]
      n=length(probMatS2_model[,1])
      probMatS2_model = cbind(probMatS2_model,rep(testModel,n),state2_idx,rep(0,n),rep("simulation",n))
      
      curr_idx = length(probMat_S2[,1])
      probMat_S2 <- rbind(probMat_S2, probMatS2_model)
      idx1 = curr_idx+1
      probMat_S2[idx1,9] = 1
      idx2 = curr_idx+min(which(state2_idx >= 801))
      probMat_S2[idx2,9] = 1
      idx3 = length(probMat_S2[,1])
      probMat_S2[idx3,9] = 1
      
      #print(sprintf("S2 idx1=%i,idx2=%i",idx1,idx2))
      
    }
    
    if(length(models.populated)==6)
    {
      break
    }
  }
  
  empProbMat <- getEmpProbMat(ratdata@allpaths,100,2)
  state1_idx = which(ratdata@allpaths[,2] == 1)
  state2_idx = which(ratdata@allpaths[,2] == 2)
  
  probMatS1_model=empProbMat[state1_idx,c(1:6)]
  n=length(probMatS1_model[,1])
  probMatS1_model = cbind(probMatS1_model,rep("EmpProb",n),state1_idx,rep(0,n),rep("TrueProb",n))
  
  curr_idx = length(probMat_S1[,1])
  probMat_S1 <- rbind(probMat_S1, probMatS1_model)
  idx1 = curr_idx+1
  probMat_S1[idx1,9] = 1
  idx2 = curr_idx+min(which(state1_idx >= 801))
  probMat_S1[idx2,9] = 1
  idx3 = length(probMat_S1[,1])
  probMat_S1[idx3,9] = 1
  
  probMatS2_model=empProbMat[state2_idx,c(7:12)]
  n=length(probMatS2_model[,1])
  probMatS2_model = cbind(probMatS2_model,rep("EmpProb",n),state2_idx,rep(0,n),rep("TrueProb",n))
  
  curr_idx = length(probMat_S2[,1])
  probMat_S2 <- rbind(probMat_S2, probMatS2_model)
  idx1 = curr_idx+1
  probMat_S2[idx1,9] = 1
  idx2 = curr_idx+min(which(state2_idx >= 801))
  probMat_S2[idx2,9] = 1
  idx3 = length(probMat_S2[,1])
  probMat_S2[idx3,9] = 1 

  probMat_S1 <- rbind(probMat_S1,c(0,0,0,1,0,0,"Asymptotic",0,0))
  probMat_S2 <- rbind(probMat_S2,c(0,0,0,1,0,0,"Asymptotic",0,0))
  
  cols.num <- c(1,2,3,4,5,6,8,9)
  probMat_S1[,cols.num] <- lapply(cols.num,function(x) as.numeric(probMat_S1[[x]]))
  probMat_S2[,cols.num] <- lapply(cols.num,function(x) as.numeric(probMat_S2[[x]]))


  n=length(probMat_S1[,1])
  Xtilde=apply(probMat_S1[,c(1:6)],2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
  pca_s1=princomp(Xtilde[,c(1:5)])

  df_s1 <- data.frame(Comp.1=double(),
                      Comp.2=double(), 
                      Model=character(), 
                      PathIdx = integer(),
                      PlotPoint = integer(),
                      IsSim = character(),
                      stringsAsFactors=FALSE)
 
  
  df_s1 <- as.data.frame(cbind(pca_s1$scores[c(1:n-1),c(1:2)],V3=probMat_S1[c(1:n-1),7],V4=probMat_S1[c(1:n-1),8],V5=probMat_S1[c(1:n-1),9],V6=probMat_S1[c(1:n-1),10]))
  
  cols.num <- c(1,2,4,5)
  df_s1[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s1[[x]]))
  
  p1<-ggplot(data = df_s1)+geom_path(aes_(x=df_s1$Comp.1, y=df_s1$Comp.2,color=df_s1[,4]))+ geom_text(aes_(x=df_s1$Comp.1, y=df_s1$Comp.2),label = ifelse(df_s1$V5==1, df_s1[,4], ""),hjust = 0, nudge_x = 0.1)+
    scale_colour_gradientn(colours = terrain.colors(10),name = "Trials") + facet_grid(~df_s1$V3) + geom_point(aes_(x=pca_s1$scores[n,1], y=pca_s1$scores[n,2]))+
    xlab("Component 1") + ylab("Component 2")+ggtitle("Box-E PCA")
  

  
  n=length(probMat_S2[,1]) 
  Xtilde=apply(probMat_S2[,c(1:6)],2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
  pca_s2=princomp(Xtilde[,c(1:5)])
  
  
  df_s2 <- data.frame(Comp.1=double(),
                      Comp.2=double(), 
                      Model=character(), 
                      PathIdx = integer(),
                      PlotPoint = integer(),
                      IsSim = character(),
                      stringsAsFactors=FALSE)
  

  df_s2 <- as.data.frame(cbind(pca_s2$scores[c(1:n-1),c(1:2)],V3=probMat_S2[c(1:n-1),7],V4=probMat_S2[c(1:n-1),8],V5=probMat_S2[c(1:n-1),9],V6=probMat_S2[c(1:n-1),10]))
  
  cols.num <- c(1,2,4,5)
  df_s2[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s2[[x]]))
  
  p2<-ggplot(data = df_s2)+geom_path(aes_(x=df_s2$Comp.1, y=df_s2$Comp.2,color=df_s2[,4]))+ geom_text(aes_(x=df_s2$Comp.1, y=df_s2$Comp.2),label = ifelse(df_s2$V5==1, df_s2[,4], ""),hjust = 0, nudge_x = 0.1)+
    scale_colour_gradientn(colours = terrain.colors(10),name = "Trials")+facet_grid(~df_s2$V3)+ geom_point(aes_(x=pca_s2$scores[n,1], y=pca_s2$scores[n,2]))+
    xlab("Component 1") + ylab("Component 2")+ggtitle("Box-I PCA")
  
  P2<-grid.arrange(p1,p2,nrow=2)
  setwd(plot.dir)
  ggsave(paste("PCA_",rat,".pdf",sep=""), P2,width=11, height=12)
  
  print(loadings(pca_s1))
  print(loadings(pca_s2))
}

plotPCA3a=function(ratdata,res.dir,allModelRes,plot.dir)
{
   rat=ratdata@rat
  setwd(res.dir)
  dfData=list.files(".", pattern=paste0(rat,".*resList.Rdata"), full.names=FALSE)
  combinedResList <- list()
  for(i in c(1))
  {
    print(dfData[i])
    load(dfData[i])
    combinedResList <- append(combinedResList,resList)
  }
  allmodelRes <- new("AllModelRes")
  models.populated = c()
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  
  modelProbMats <- data.frame(matrix(0,0,16))

  for(k in c(1:length(combinedResList)))
  {
    ##Extracting generator model from resList[[k]][[1]]
    genData = combinedResList[[k]][[1]]$data
    genModel = combinedResList[[k]][[1]]$data@simModel
    ## Find the index of modelData corresponding to generator model, retrieve the result of holdout cut at trial 800
    index = which(models==genModel)
    testModelData = combinedResList[[k]][[index]]$res
    testModelName = testModelData@Model
    #print(sprintf("genModel=%s, k=%i, testModel=%s",genModel,k,testModel))
    if(!testModelName %in% models.populated)
    {
      models.populated = c(models.populated,genModel)
      print(sprintf("testModel=%s",testModelName))
      #modelData <- slot(slot(allmodelRes,"Paths"),"aca2")
      

      #probMat_model=testModelData@probMatrix[,c(1:12)]
      testModel=slot(allModels,testModelName)
      probMat_model=TurnsNew::getProbMatrix2(genData, testModelData, testModel, sim=1)
      n=length(probMat_model[,1])
      probMat_model = cbind(probMat_model,c(1:n),rep(testModelName,n),rep(0,n),rep("SimProb",n))
      
      curr_idx = length(modelProbMats[,1])
      modelProbMats <- rbind(modelProbMats, probMat_model)
      idx1 = curr_idx+1
      modelProbMats[idx1,16] = 1
      idx2 = curr_idx+801
      modelProbMats[idx2,16] = 1
      idx3 = length(modelProbMats[,1])
      modelProbMats[idx3,16] = 1
      
    }
    
    if(length(models.populated)==6)
    {
      break
    }
  }
  
  empProbMat <- getEmpProbMat3a(ratdata@allpaths,40,2)
  empProbMat <- rbind(empProbMat,c(0,0,0,1,0,0,0,0,0,1,0,0))

  
  
  cols.num <- c(1:13)
  modelProbMats[,cols.num] <- lapply(cols.num,function(x) as.numeric(modelProbMats[[x]]))
  
  pca = prcomp(modelProbMats[,1:12],scale=T,center = T)
  empPCA = scale(empProbMat[,c(1:12)], pca$center, pca$scale) %*% pca$rotation
  
  textVec <- rep(0,length(empProbMat[,1]))
  textVec[1]=1
  textVec[801]=1
  textVec[length(empProbMat[,1])-1]=1
  df_s1 <- as.data.frame(cbind(pca$x[,1:2], modelProbMats[,15],modelProbMats[,13],modelProbMats[,16]))
  df_s1 <- rbind(df_s1,cbind(empPCA[,1:2], rep("Empirical",length(empPCA[,1])),empProbMat[,13],V5=textVec))
  
  cols.num <- c(1,2,4,5)
  df_s1[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s1[[x]]))
  
  cols=c("black", "blue","green","red","violet","brown","yellow")
  #df_s1 <- df_s1[which(df_s1$V4 > 800),]
  # P <- ggplot(data = df_s1) + geom_point(size=1,aes(x=df_s1$PC1,y=df_s1$PC2,color=df_s1$V3))+ 
  #   scale_color_manual(values = cols)+
  #  labs(x="PCA Component 1", y="PCA Component 2", col="Models")+ggtitle(ratdata@rat)
  
  # P <- ggplot(data = df_s1)+geom_path(aes_(x=df_s1$PC1, y=df_s1$PC2,color=df_s1$V4))+
  #   scale_colour_gradientn(colours = terrain.colors(10),name = "Trials")+facet_grid(~df_s1$V3)+ 
  #   coord_cartesian(xlim = c(min(df_s1$PC1),max(df_s1$PC1)), ylim=c(min(df_s1$PC2),max(df_s1$PC2)))+
  #   xlab("Component 1") + ylab("Component 2")
  
  n=length(df_s1[,1])
  P <- ggplot(data = df_s1)+ geom_path(aes_(x=df_s1$PC1, y=df_s1$PC2,color=df_s1$V4))+
    geom_text(aes_(x=df_s1$PC1, y=df_s1$PC2),label = ifelse(df_s1$V5==1, df_s1[,4], ""),vjust = 1,hjust = 1)+
    geom_point(aes(x=df_s1$PC1[n], y=df_s1$PC2[n]))+
    scale_colour_gradientn(colours = terrain.colors(10),name = "Trials")+facet_grid(~df_s1$V3)+ 
    coord_cartesian(xlim = c(min(df_s1$PC1),max(df_s1$PC1)), ylim=c(min(df_s1$PC2),max(df_s1$PC2)))+
    xlab("Component 1") + ylab("Component 2")
  
  print(P)
  setwd(plot.dir)
  ggsave(paste("PCA_",rat,".pdf",sep=""), P,width=10, height=10)

}



plotPCA5=function(ratdata,allModelRes,model.data.dir)
{
   rat=ratdata@rat
  setwd(model.data.dir)
  dfData=list.files(".", pattern=paste0("allmodelRes_",rat,".Rdata"), full.names=FALSE)
  
  
  selectedModels = list("rat_101"="Hybrid3","rat_103"="Hybrid2","rat_106"="Hybrid3","rat_112"="Hybrid3","rat_113"="Hybrid3","rat_114"="Hybrid3","robert"="Hybrid3")
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  
  probMat_S1 <- data.frame(matrix(0,0,10))
  probMat_S2 <- data.frame(matrix(0,0,10))
  
  for(m in models)
  {
    modelData = getModelData(allModelRes,m,"aca2")

    state1_idx = which(ratdata@allpaths[,2] == 1)
    state2_idx = which(ratdata@allpaths[,2] == 2)
    
    probMatS1_model=modelData@probMatrix[state1_idx,c(1:6)]
    n=length(probMatS1_model[,1])
    probMatS1_model = cbind(probMatS1_model,rep(m,n),state1_idx,rep(0,n),rep("TrueProb",n))
    
    curr_idx = length(probMat_S1[,1])
    probMat_S1 <- rbind(probMat_S1, probMatS1_model)
    idx1 = curr_idx+1
    probMat_S1[idx1,9] = 1
    idx2 = curr_idx+min(which(state1_idx >= 801))
    probMat_S1[idx2,9] = 1
    idx3 = length(probMat_S1[,1])
    probMat_S1[idx3,9] = 1
    
    #print(sprintf("S1 idx1=%i,idx2=%i",idx1,idx2))
    
    
    probMatS2_model=modelData@probMatrix[state2_idx,c(7:12)]
    n=length(probMatS2_model[,1])
    probMatS2_model = cbind(probMatS2_model,rep(m,n),state2_idx,rep(0,n),rep("TrueProb",n))
    
    curr_idx = length(probMat_S2[,1])
    probMat_S2 <- rbind(probMat_S2, probMatS2_model)
    idx1 = curr_idx+1
    probMat_S2[idx1,9] = 1
    idx2 = curr_idx+min(which(state2_idx >= 801))
    probMat_S2[idx2,9] = 1
    idx3 = length(probMat_S2[,1])
    probMat_S2[idx3,9] = 1
    
    #print(sprintf("S2 idx1=%i,idx2=%i",idx1,idx2))
    
  }
  
  
  probMatRowsS1 <- length(probMat_S1[,1]) 
  probMatRowsS2 <- length(probMat_S2[,1]) 
  
  empProbMat <- getEmpProbMat(ratdata@allpaths,100,2)
  state1_idx = which(ratdata@allpaths[,2] == 1)
  state2_idx = which(ratdata@allpaths[,2] == 2)
  
  empProbMatS1=empProbMat[state1_idx,c(1:6)]
  n=length(empProbMatS1[,1])
  empProbMatS1 = cbind(empProbMatS1,rep("EmpProb",n),state1_idx,rep(0,n),rep("TrueProb",n))
  
  curr_idx = length(probMat_S1[,1])
  probMat_S1 <- rbind(probMat_S1, empProbMatS1)
  idx1 = curr_idx+1
  probMat_S1[idx1,9] = 1
  idx2 = curr_idx+min(which(state1_idx >= 801))
  probMat_S1[idx2,9] = 1
  idx3 = length(probMat_S1[,1])
  probMat_S1[idx3,9] = 1
  
  empProbMatS2=empProbMat[state2_idx,c(7:12)]
  n=length(empProbMatS2[,1])
  empProbMatS2 = cbind(empProbMatS2,rep("EmpProb",n),state2_idx,rep(0,n),rep("TrueProb",n))
  
  curr_idx = length(probMat_S2[,1])
  probMat_S2 <- rbind(probMat_S2, empProbMatS2)
  idx1 = curr_idx+1
  probMat_S2[idx1,9] = 1
  idx2 = curr_idx+min(which(state2_idx >= 801))
  probMat_S2[idx2,9] = 1
  idx3 = length(probMat_S2[,1])
  probMat_S2[idx3,9] = 1
  
  
  probMat_S1 <- rbind(probMat_S1,c(0,0,0,1,0,0,"Asymptotic",0,0))
  probMat_S2 <- rbind(probMat_S2,c(0,0,0,1,0,0,"Asymptotic",0,0))
  
  cols.num <- c(1,2,3,4,5,6,8,9)
  probMat_S1[,cols.num] <- lapply(cols.num,function(x) as.numeric(probMat_S1[[x]]))
  probMat_S2[,cols.num] <- lapply(cols.num,function(x) as.numeric(probMat_S2[[x]]))
  
  
  #empPCAS1<-scale(empS1[,c(1:5)], pca_res$center, pca_res$scale) %*% pca_res$rotation
  #emp_pca<-predict(object=pca_res, newdata = empS1[,c(1:5)])
  pca_s1 = prcomp(probMat_S1[1:probMatRowsS1,c(1:5)],scale=T,center = T)
  empPCAS1 = scale(empProbMat[state1_idx,c(1:5)], pca_s1$center, pca_s1$scale) %*% pca_s1$rotation
  
  #n=length(probMat_S1[1:probMatRowsS1,1])
  #Xtilde=apply(probMat_S1[1:probMatRowsS1,c(1:6)],2,function(x){(x-mean(x))/(sd(x)*sqrt(n-1)/sqrt(n))})
  #pca_s1=princomp(Xtilde[1:probMatRowsS1,c(1:5)])
  
  
  pca_s2 = prcomp(probMat_S2[1:probMatRowsS2,c(1:5)],scale=T,center = T)
  empPCAS2 = scale(empProbMat[state2_idx,c(7:11)], pca_s2$center, pca_s2$scale) %*% pca_s2$rotation


  df_s1 <- data.frame(Comp.1=double(),
                      Comp.2=double(), 
                      Model=character(), 
                      PathIdx = integer(),
                      PlotPoint = integer(),
                      IsSim = character(),
                      stringsAsFactors=FALSE)
  
  
  
  selModelIdx <- which(probMat_S1[,7]==selectedModels[[ratdata@rat]])
  remove800 <- which(probMat_S1[selModelIdx,8] < 800)
  selModelIdx <- selModelIdx[-c(remove800)]
  
  df_s1 <- as.data.frame(cbind(pca_s1$x[selModelIdx,c(1:2)],V3=rep(selectedModels[[ratdata@rat]],length(selModelIdx)),V4=probMat_S1[selModelIdx,8],V5=probMat_S1[selModelIdx,9],V6=probMat_S1[selModelIdx,10]))
  df_s1 <- rbind(df_s1,cbind(empPCAS1[-c(1:length(remove800)),c(1:2)],V3=rep("Empirical",length(selModelIdx)),V4=probMat_S1[selModelIdx,8],V5=probMat_S1[selModelIdx,9],V6=probMat_S1[selModelIdx,10]))
  #df_s1 <- rbind(df_s1,cbind(empPCAS1[,c(1:2)],V3=rep("Empirical",length(selModelIdx)),V4=probMat_S1[selModelIdx,8],V5=probMat_S1[selModelIdx,9],V6=probMat_S1[selModelIdx,10]))
  
  
  cols.num <- c(1,2,4,5)
  df_s1[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s1[[x]]))
  
  p1<-ggplot(data = df_s1)+geom_path(aes_(x=df_s1$PC1, y=df_s1$PC2,color=df_s1[,4]))+ geom_text(aes_(x=df_s1$PC1, y=df_s1$PC2),label = ifelse(df_s1$V5==1, df_s1[,4], ""),hjust = 0, nudge_x = 0.1)+
    scale_colour_gradientn(colours = terrain.colors(10),name = "Trials") + facet_grid(~df_s1$V3) +     xlab("Component 1") + ylab("Component 2")+ggtitle("Box-E PCA")
  
  
  df_s2 <- data.frame(Comp.1=double(),
                      Comp.2=double(), 
                      Model=character(), 
                      PathIdx = integer(),
                      PlotPoint = integer(),
                      IsSim = character(),
                      stringsAsFactors=FALSE)
  
  
  selModelIdx <- which(probMat_S2[,7]==selectedModels[[ratdata@rat]])
  remove800 <- which(probMat_S2[selModelIdx,8] < 800)
  selModelIdx <- selModelIdx[-c(remove800)]
  
  df_s2 <- as.data.frame(cbind(pca_s2$x[selModelIdx,c(1:2)],V3=rep(selectedModels[[ratdata@rat]],length(selModelIdx)),V4=probMat_S2[selModelIdx,8],V5=probMat_S2[selModelIdx,9],V6=probMat_S2[selModelIdx,10]))
  df_s2 <- rbind(df_s2,cbind(empPCAS2[-c(1:length(remove800)),c(1:2)],V3=rep("Empirical",length(selModelIdx)),V4=probMat_S2[selModelIdx,8],V5=probMat_S2[selModelIdx,9],V6=probMat_S2[selModelIdx,10]))
  #df_s2 <- rbind(df_s2,cbind(empPCAS2[,c(1:2)],V3=rep("Empirical",length(selModelIdx)),V4=probMat_S2[selModelIdx,8],V5=probMat_S2[selModelIdx,9],V6=probMat_S2[selModelIdx,10]))
  
  cols.num <- c(1,2,4,5)
  df_s2[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s2[[x]]))
  
  p2<-ggplot(data = df_s2)+geom_path(aes_(x=df_s2$PC1, y=df_s2$PC2,color=df_s2[,4]))+ geom_text(aes_(x=df_s2$PC1, y=df_s2$PC2),label = ifelse(df_s2$V5==1, df_s2[,4], ""),hjust = 0, nudge_x = 0.1)+
    scale_colour_gradientn(colours = terrain.colors(10),name = "Trials")+facet_grid(~df_s2$V3)+     xlab("Component 1") + ylab("Component 2")+ggtitle("Box-I PCA")
  
  P2<-grid.arrange(p1,p2,nrow=2,top=textGrob(ratdata@rat,gp=gpar(fontsize=20,font=3)))
  setwd(plot.dir)
  ggsave(paste("PCA_",rat,".pdf",sep=""), P2,width=11, height=12)

}

plotPCA7=function(ratdata,allModelRes,model.data.dir,plot.dir)
{
  rat=ratdata@rat
  setwd(model.data.dir)
  dfData=list.files(".", pattern=paste0("allmodelRes_",rat,".Rdata"), full.names=FALSE)
  
  
  selectedModels = list("rat_101"="Hybrid3","rat_103"="Hybrid2","rat_106"="Hybrid3","rat_112"="Hybrid3","rat_113"="Hybrid3","rat_114"="Hybrid3","robert"="Hybrid3")
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  count = list("rat_101"=50,"rat_103"=200,"rat_106"=200,"rat_112"=200,"rat_113"=100,"rat_114"=100,"robert"=50)
  
  modelProbMats <- data.frame(matrix(0,0,15))

  for(m in models)
  {
    modelData = getModelData(allModelRes,m,"aca2")
    testModel = slot(allModels,m)

    probMat <- TurnsNew::getProbMatrix2(ratdata, modelData, testModel, sim=2)
    
    n=length(probMat[,1])
    modelProbMats=rbind(modelProbMats,cbind(probMat,rep(m,n)))
    
  }

  empProbMat <- getEmpProbMat3(ratdata@allpaths,100,2)
  
  cols.num <- c(1:13)
  modelProbMats[,cols.num] <- lapply(cols.num,function(x) as.numeric(modelProbMats[[x]]))
  
  pca = prcomp(modelProbMats[,1:12],scale=T,center = T)
  empPCA = scale(empProbMat[,c(1:12)], pca$center, pca$scale) %*% pca$rotation
  
  selModelIdx <- which(modelProbMats[,14]==selectedModels[[ratdata@rat]])
  
  df_s1 <- as.data.frame(cbind(pca$x[,1:2], modelProbMats[,14],modelProbMats[,13]))
  df_s1 <- rbind(df_s1,cbind(empPCA[,1:2], rep("Empirical",length(selModelIdx)),empProbMat[,13]))
  
  cols.num <- c(1,2,4)
  df_s1[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s1[[x]]))
  cols=c("red", "blue","green","yellow","violet","brown","black")
  df_s1 <- df_s1[which(df_s1$V4 > 800),]
   P <- ggplot(data = df_s1) + geom_point(size=3,aes(x=df_s1$PC1,y=df_s1$PC2,color=df_s1$V3))+ geom_point(data = subset(df_s1, V3 == selectedModels[[ratdata@rat]]),size=3,aes(x=PC1,y=PC2,color = V3)) + scale_color_manual(values = cols)+
    labs(x="PCA Component 1", y="PCA Component 2", col="Models")
  
  
  setwd(plot.dir)
  ggsave(paste("PCA_",rat,".pdf",sep=""), P,width=11, height=12)
}

plotPCA7a=function(ratdata,allModelRes,model.data.dir)
{
  rat=ratdata@rat
  setwd(model.data.dir)
  dfData=list.files(".", pattern=paste0("allmodelRes_",rat,".Rdata"), full.names=FALSE)
  
  
  selectedModels = list("rat_101"="Hybrid3","rat_103"="Hybrid2","rat_106"="Hybrid3","rat_112"="Hybrid3","rat_113"="Hybrid3","rat_114"="Hybrid3","robert"="Hybrid3")
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  count = list("rat_101"=50,"rat_103"=200,"rat_106"=200,"rat_112"=200,"rat_113"=100,"rat_114"=100,"robert"=50)
  
  modelProbMats <- data.frame(matrix(0,0,15))

  for(m in models)
  {
    modelData = getModelData(allModelRes,m,"aca2")
    testModel = slot(allModels,m)

    probMat <- TurnsNew::getProbMatrix2(ratdata, modelData, testModel, sim=2)
    
    #print(sprintf("S2 idx1=%i,idx2=%i",idx1,idx2))
    n=length(probMat[,1])
    modelProbMats=rbind(modelProbMats,cbind(probMat,rep(m,n)))
    
  }

  #empProbMat <- getEmpProbMat4(ratdata@allpaths,50,2)
  empProbMat <- getEmpProbMat3(ratdata@allpaths,40,2)
  
  cols.num <- c(1:13)
  modelProbMats[,cols.num] <- lapply(cols.num,function(x) as.numeric(modelProbMats[[x]]))
  
  pca = prcomp(modelProbMats[,1:12],scale=T,center = T)
  empPCA = scale(empProbMat[,c(1:12)], pca$center, pca$scale) %*% pca$rotation
  
  selModelIdx <- which(modelProbMats[,14]==selectedModels[[ratdata@rat]])
  
  df_s1 <- as.data.frame(cbind(pca$x[,1:2], modelProbMats[,14],modelProbMats[,13]))
  df_s1 <- rbind(df_s1,cbind(empPCA[,1:2], rep("Empirical",length(selModelIdx)),empProbMat[,13]))
  
  cols.num <- c(1,2,4)
  df_s1[,cols.num] <- lapply(cols.num,function(x) as.numeric(df_s1[[x]]))
  
  cols=c("black", "blue","green","red","violet","brown","yellow")
  df_s1 <- df_s1[which(df_s1$V4 > 800),]
   # P <- ggplot(data = df_s1) + geom_point(size=1,aes(x=df_s1$PC1,y=df_s1$PC2,color=df_s1$V3))+ 
   #   scale_color_manual(values = cols)+
   #  labs(x="PCA Component 1", y="PCA Component 2", col="Models")+ggtitle(ratdata@rat)
  
  P <- ggplot(data = df_s1) + geom_point(data = subset(df_s1, V3 != "Empirical"),size=2,aes(x=PC1,y=PC2,color = V3))+
    geom_point(data = subset(df_s1, V3 == selectedModels[[ratdata@rat]]),size=2,aes(x=PC1,y=PC2,color = V3))+
    geom_point(shape=1,data = subset(df_s1, V3 == "Empirical"),size=2,aes(x=PC1,y=PC2,color = V3))+
    scale_color_manual(values = cols)+
    labs(x="PCA Component 1", y="PCA Component 2", col="Models")+
    theme( axis.text = element_text( size = 12), axis.title = element_text( size = 12),
           legend.title = element_text( size = 12),
           legend.text = element_text( size = 12))
  print(P)
  setwd(plot.dir)
  ggsave(paste("PCA_",rat,".pdf",sep=""), P,width=8, height=8)
}


getEmpProbMat3a=function(allpaths,window,sim){
  totalActions = length(allpaths[,1])
  empProbMat = matrix(0,totalActions,13)
  
  if(sim==1)
  {
    allpaths[,c(1:2)] = allpaths[,c(1:2)] + 1
  }
  
  sessions = unique(allpaths[,5])
  trial = 0
  
  for(ses in sessions)
  {
    
    allpaths_ses = allpaths[which(allpaths[,5] == ses),]
    
    for(trial_ses in c(1:length(allpaths_ses[,1])))
    {
      trial = trial + 1 
      empProbMat[trial,13]= allpaths_ses[trial_ses,6]
      
      if(trial > 1 )
      {
        empProbMat[trial,1:12] = empProbMat[trial-1,1:12]
      }
      
      
      
      curr_state <- allpaths_ses[trial_ses,2]
      ses_currStateIndices <- which(allpaths_ses[,2] == curr_state)
      
      curr_stateId <- which(allpaths_ses[ses_currStateIndices,6]==trial)
      

      startId = curr_stateId - window/2
      if(startId <= 0)
      {
        startId = 1
      }
      endId = curr_stateId + window/2
      if(endId > length(ses_currStateIndices))
      {
        endId = length(ses_currStateIndices)
      }
      
      trialSet <- ses_currStateIndices[startId:endId]
      undefinedPaths <- which(allpaths_ses[trialSet,1] == 7)
      trialSet <- trialSet[! trialSet %in% trialSet[undefinedPaths]]
      
      
      for(path in c(1:6))
      {
        empProbMat[trial,(path+6*(curr_state-1))] = mean(as.numeric(allpaths_ses[trialSet,1] %in% path)) 
        
      }
      
      
    }
   
  }
  
  
  return(empProbMat)
}


getEmpProbMat3=function(allpaths,window,sim){
  totalActions = length(allpaths[,1])
  empProbMat = matrix(-1,totalActions,13)
  
  if(sim==1)
  {
    allpaths[,c(1:2)] = allpaths[,c(1:2)] + 1
  }
  
  for(trial in c(1:totalActions))
  {
    empProbMat[trial,13]= allpaths[trial,6]

    for(state in c(1:2))
    {
      stateIdx <- which(allpaths[1:trial,2]==state)
      if(length(stateIdx) <= window)
      {
        trialSet = stateIdx
      }
      else
      {
        trialSet = tail(stateIdx,window)
      }
      
      for(path in c(1:6))
      {
        if(length(trialSet)==0)
        {
          empProbMat[trial,(path+6*(state-1))] = 0
        }
        else
        {
          empProbMat[trial,(path+6*(state-1))] = mean(as.numeric(allpaths[trialSet,1] %in% path)) 
        }
        
      }
      
    }
    
  }
  
  return(empProbMat)
}

getEmpProbMat=function(allpaths,window,sim)
{
  totalActions = length(allpaths[,1])
  empProbMat = matrix(-1,totalActions,13)
  
  if(sim==1)
  {
    allpaths[,c(1:2)] = allpaths[,c(1:2)] + 1
  }
  
  for(trial in c(1:totalActions)){
    empProbMat[trial,13]= allpaths[trial,6]
    currState = allpaths[trial,2]
    
    stateIdx <- which(allpaths[1:trial,2]==currState)
     
    
    if(length(stateIdx) <= window)
    {
      trialSet = stateIdx
    }
    else
    {
      trialSet = tail(stateIdx,window)
    }
    

    for(path in c(1:6))
    {
      empProbMat[trial,(path+6*(currState-1))] = mean(as.numeric(allpaths[trialSet,1] %in% path))
    }
  }
  
  return(empProbMat)
}


getStartIndex = function(generated_data){
  start_index=0
  l<-which(SMA(generated_data[,3],20)>=0.6)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>20){
      start_index=k[[set]][1]
      break
    }
  }
  return(start_index)
}

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

getEndIndex2= function(allpaths, sim, limit){
  
  end_index = -1
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
      break
    }
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
      if(boxIndices[i] == 1)
      {
        range = boxIndices[i+1]
      }
      else
      {
        range = boxIndices[i+1]-boxIndices[i]  
      }
      
      
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

  generated_data@hybridModel1 = convertTurnTimes(generated_data,TurnModel,Hybrid1,sim=1)
  generated_data@hybridModel2 = convertTurnTimes(generated_data,TurnModel,Hybrid2,sim=1)
  generated_data@hybridModel3 = convertTurnTimes(generated_data,TurnModel,Hybrid3,sim=1)
  generated_data@hybridModel4 = convertTurnTimes(generated_data,TurnModel,Hybrid4,sim=1)
  
  return(generated_data)
  
}

