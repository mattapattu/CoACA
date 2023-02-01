checkEpisodes=function(ratdata)
{
  resetVector=T
  changeState = F
  returnToInitState = F
  allpaths = ratdata@allpaths
  sessions = unique(ratdata@allpaths[,5])
  episodeList <- list()
  episodeNb = 0
  pathNb = 0
  for(ses in sessions)
  {
    ses_allpaths = allpaths[which(allpaths[,5]==ses),]
    S = ses_allpaths[1,2]
    ses.len = length(ses_allpaths[,1])
    episodeVec <- c()
    for(trial in c(1:ses.len))
    {
      if(resetVector)
      {
        initState = S
        resetVector = FALSE;
      }
      
      if(S == 1){
        path = paste0("Path",ses_allpaths[trial,1],".LF")
      }else
      {
        path = paste0("Path",ses_allpaths[trial,1],".RF")
      }
      episodeVec <- c(episodeVec,path)
        
      
      if(trial < ses.len)
      {
        S_prime = ses_allpaths[(trial+1),2] 
      }
      
      
      if(S_prime!=initState){
        changeState = T
      }else if(S_prime==initState && changeState){
        returnToInitState = T
      }
      
      S = S_prime
      pathNb = pathNb+1
      
      if(returnToInitState || (trial == ses.len))
      {
        changeState = F
        returnToInitState = F
        resetVector = T
        episodeNb  = episodeNb + 1
        
        episodeList[[episodeNb]] <- list(episodeVec=episodeVec,episodeNb=episodeNb, ses=ses, pathNb=pathNb)
        
        
        episodeVec <- c()
        
        
      }
    }
  }
  
  return(episodeList)
  
}

computePathFiringRates=function(enreg,rat,ratdata){
  
  boxTimes <- vector()
  
  neuronalFiringRates <- vector(mode = "list", length = length(enreg))
  pathFiringNeurons <- vector(mode = "list", length = length(enreg))
  
  ### Loop through all enreg[[ses]] of current rat
  for(ses in 1:length(enreg)){
    
    EnregNeurons_ses <- EnregNeurons[[rat]][[ses]]
    EnregNeurons_ses <- EnregNeurons_ses[which(EnregNeurons_ses[,3] != 0),, drop=FALSE]
    uniq.neurons.ses <- unique(EnregNeurons_ses[,c(2,3)])
    if(length(uniq.neurons.ses[,1]) > 1)
    {
      uniq.neurons.ses <- uniq.neurons.ses[order(uniq.neurons.ses[,1],uniq.neurons.ses[,2]),]
    }
    
    
    allpaths_ses = ratdata@allpaths[which(ratdata@allpaths[,5]==ses),]
    ses_paths = strsplit(enreg[[ses]]$short,"(?<=[ei])(?=(jk)|(ja)|(jb)|(fg)|(fb)|(fa)|(dc)|(hc)|(jik))",perl=TRUE)[[1]]
    boxIndices = c(1,cumsum(nchar(ses_paths)))
    
    
    colnames <- c("Path", "State", "Duration","Session", paste("tet",uniq.neurons.ses[,1],".",uniq.neurons.ses[,2],sep=""))
    
    firingRateMat <- matrix(0,length(allpaths_ses[,1]),length(colnames))
    colnames(firingRateMat) <- colnames
    
    firingNeuronsMat <- matrix(0,length(allpaths_ses[,1]),length(colnames))
    colnames(firingNeuronsMat) <- colnames
    
    for(i in 1:(length(boxIndices)-1))
    {
      #print(sprintf("ses=%i, actNb=%i, path=%i, pathString=%s, state=%i",ses,i,allpaths_ses[i,1],ses_paths[i],allpaths_ses[i,2]))
      
      if(i == 1)
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
        enregRowsFiringRate = enregRows[-nrow(enregRows),, drop=FALSE]
        firingRates = getFiringRates(rat,enregRowsFiringRate,ses,pathTime)
        firingNeurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
        
      }
      else
      {
        pathTime = enregRows[2] - enregRows[1]
        enregRowsFiringRate = enregRows
        firingRates = rep(0,nrow(uniq.neurons.ses))
        firingNeurons = rep(0,nrow(uniq.neurons.ses))
      }
      
      

      firingRateMat[i,] = c(allpaths_ses[i,c(1,2,4,5)], firingRates)
      firingNeuronsMat[i,] = c(allpaths_ses[i,c(1,2,4,5)],firingNeurons)
    }
    
    
    
    neuronalFiringRates[[ses]] <- firingRateMat
    pathFiringNeurons[[ses]] <- firingNeuronsMat
  }
  
  return(list("pathFiringRates"=neuronalFiringRates, "pathFiringNeurons"=pathFiringNeurons))
}


compute_Turns_n_Hybrid_FiringRates=function(ratdata,ratNb,enreg,turnsModel,ratPathNeuronalData)
{
  
  #debug(getTurnsFiringRates)
  resList = getTurnsFiringRates(ratdata@allpaths,enreg,turnsModel, ratNb)
  turnFiringRates = resList$turnFiringRates
  turnFiringNeurons = resList$turnFiringNeurons
  
  rat = ratdata@rat
  
  hybrid1res = convertTurnNeuronsToHybrid(ratdata,ratNb,turnFiringNeurons,TurnModel,Hybrid1,sim=2)
  hybrid2res = convertTurnNeuronsToHybrid(ratdata,ratNb,turnFiringNeurons,TurnModel,Hybrid2,sim=2)
  hybrid3res = convertTurnNeuronsToHybrid(ratdata,ratNb,turnFiringNeurons,TurnModel,Hybrid3,sim=2)
  hybrid4res = convertTurnNeuronsToHybrid(ratdata,ratNb,turnFiringNeurons,TurnModel,Hybrid4,sim=2)
  
  firingNeuronData = new("RatNeuronalData", rat = rat,Paths = ratPathNeuronalData$pathFiringNeurons,Turns = turnFiringNeurons)
  
  firingNeuronData@Hybrid1 = hybrid1res$firingNeuronsMat
  firingNeuronData@Hybrid2 = hybrid2res$firingNeuronsMat
  firingNeuronData@Hybrid3 = hybrid3res$firingNeuronsMat
  firingNeuronData@Hybrid4 = hybrid4res$firingNeuronsMat
  
  firingRateData = new("RatNeuronalData", rat = rat,Paths = ratPathNeuronalData$pathFiringRates,Turns = turnFiringNeurons)
  
  firingRateData@Hybrid1 = hybrid1res$firingRateMat
  firingRateData@Hybrid2 = hybrid2res$firingRateMat
  firingRateData@Hybrid3 = hybrid3res$firingRateMat
  firingRateData@Hybrid4 = hybrid4res$firingRateMat
  

  return(list("firingNeuronData"=firingNeuronData, "firingRateData"=firingRateData))
  
}

getTurnsFiringRates=function(allpaths,enreg,turnsModel, rat)
{
  boxIndices = as.numeric(allpaths[,7])
  uniqSess = unique(as.numeric(allpaths[,5]))
  
  turnFiringRates <- vector(mode = "list", length = length(uniqSess))
  turnFiringNeurons <- vector(mode = "list", length = length(uniqSess))
  
  
  
  for(ses in uniqSess)
  {
    EnregNeurons_ses <- EnregNeurons[[rat]][[ses]]
    EnregNeurons_ses <- EnregNeurons_ses[which(EnregNeurons_ses[,3] != 0),, drop=FALSE]
    uniq.neurons.ses <- unique(EnregNeurons_ses[,c(2,3)])
    if(length(uniq.neurons.ses[,1]) > 1)
    {
      uniq.neurons.ses <- uniq.neurons.ses[order(uniq.neurons.ses[,1],uniq.neurons.ses[,2]),]
    }
    
    colnames <- c("Action", "Path", "State","Turn","Session", "Duration", paste("tet",uniq.neurons.ses[,1],".",uniq.neurons.ses[,2],sep=""))

    idx_ses = which(as.numeric(allpaths[,5])==ses)
    pathCount_ses = length(allpaths[idx_ses,1])
    
    totalTurns = pathCount_ses*4
    
    firingRateMat <- matrix(0,totalTurns,length(colnames))
    firingNeuronsMat <- matrix(0,totalTurns,length(colnames))
    matIdx = 1
    
    for(i in 1:pathCount_ses)
    {
      path = as.numeric(allpaths[idx_ses[i],1])-1
      state = as.numeric(allpaths[idx_ses[i],2])-1
      
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
      
      #print(sprintf("ses=%i, actNb=%i, path=%i, state=%i",ses,i,allpaths[i,1],allpaths[i,2]))
      
      
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
      
      #debug(getTurnDuration)
      resList = convertNeuronToTurns(path, state, enregRows, rat, ses)
      
      turnTimes = resList$turntimes
      firingRates = resList$firingRates
      firingNeurons = resList$firingNeurons
      
      
      
      if(length(turns) >0)
      {
        for(j in 1:length(turns))
        {
          actId = which(slot(turnsModel, nodeList) == turns[j]) - 1
          firingRateMat[matIdx,]=c(idx_ses[i],path,state,actId,ses,turnTimes[[j]], firingRates[[j]])
          firingNeuronsMat[matIdx,] = c(idx_ses[i],path,state,actId,ses,turnTimes[[j]], firingNeurons[[j]])
          matIdx = matIdx+1
        }
      }
      
    }
    
    firingRateMat = firingRateMat[-(matIdx:totalTurns),]
    firingNeuronsMat = firingNeuronsMat[-(matIdx:totalTurns),]
    
    colnames(firingRateMat) <- colnames
    colnames(firingNeuronsMat) <- colnames
    
    
    turnFiringRates[[ses]] <- firingRateMat
    turnFiringNeurons[[ses]] <- firingNeuronsMat
  }
  

  return(list("turnFiringRates"=turnFiringRates,"turnFiringNeurons"=turnFiringNeurons))
}


convertNeuronToTurns=function(path, state, enregRows, rat, ses)
{
  
  turntimes=list();
  startTimes=list()
  endTimes=list()
  
  if (state == 0)
  {
    if (path == 0)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      st_dc1 = enregRows[1,1]
      et_dc1 = enregRows[2,1] + c1
      
      enregRowsFiringRate = enregRows[1:2,]
      enregRowsFiringRate[2,2] = et_dc1
      dc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,dc1)
      dc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      c2 = c/2
      h = enregRows[3,2] - enregRows[3,1]
      c2h = c2 + h
      
      st_c2h = et_dc1
      et_c2h = enregRows[3,2]
      
      enregRowsFiringRate = enregRows[2:3,]
      enregRowsFiringRate[1,1] = st_c2h
      c2h_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2h)
      c2h_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      turntimes = list("dc1"=dc1, "c2h" = c2h)
      firingRates = list("dc1"=dc1_fr, "c2h" = c2h_fr)
      firingNeurons = list("dc1"=dc1_neurons, "c2h" = c2h_neurons)
    }
    else if (path == 1)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      st_fga1 = enregRows[1,1]
      et_fga1 = enregRows[3,1] + a1
      
      enregRowsFiringRate = enregRows[1:3,]
      enregRowsFiringRate[3,2] = et_fga1
      fga1_fr = getFiringRates(rat,enregRowsFiringRate,ses,fga1)
      fga1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      a2 = a/2
      kj = sum(enregRows[4:5,2] - enregRows[4:5,1])
      a2kj = a2 + kj
      
      st_a2kj = et_fga1
      et_a2kj = enregRows[5,2]
      
      enregRowsFiringRate = enregRows[3:5,]
      enregRowsFiringRate[1,1] = st_a2kj
      a2kj_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2kj)
      a2kj_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("fga1"=fga1, "a2kj" = a2kj)
      firingRates = list("fga1"=fga1_fr, "a2kj" = a2kj_fr)
      firingNeurons = list("fga1"=fga1_neurons, "a2kj" = a2kj_neurons)
      
    }
    else if (path == 2)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      st_dc1 = enregRows[1,1]
      et_dc1 = enregRows[2,1] + c1
      
      
      enregRowsFiringRate = enregRows[1:2,]
      enregRowsFiringRate[2,2] = et_dc1
      dc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,dc1)
      dc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      st_c2ba1 = et_dc1
      et_c2ba1 = enregRows[4,1] + a1
      
      
      enregRowsFiringRate = enregRows[2:4,]
      enregRowsFiringRate[1,1] = st_c2ba1
      enregRowsFiringRate[3,2] = et_c2ba1
      c2ba1_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2ba1)
      c2ba1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      a2 = a/2
      kj = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2kj = a2+kj
      
      st_a2kj = et_c2ba1
      et_a2kj = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[4:6,]
      enregRowsFiringRate[1,1] = st_a2kj
      a2kj_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2kj)
      a2kj_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("dc1"=dc1, "c2ba1" = c2ba1, "a2kj"=a2kj)
      firingRates = list("dc1"=dc1_fr, "c2ba1"=c2ba1_fr, "a2kj"=a2kj_fr)
      firingNeurons = list("dc1"=dc1_neurons, "c2ba1"=c2ba1_neurons, "a2kj"=a2kj_neurons)
      
      
    }
    else if (path == 3)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      st_fga1 = enregRows[1,1]
      et_fga1 = enregRows[3,1] + a1
      
      enregRowsFiringRate = enregRows[1:3,]
      enregRowsFiringRate[3,2] = et_fga1
      fga1_fr = getFiringRates(rat,enregRowsFiringRate,ses,fga1)
      fga1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      st_a2bc1 = et_fga1
      et_a2bc1 = enregRows[5,1] + c1
      
      enregRowsFiringRate = enregRows[3:5,]
      enregRowsFiringRate[1,1] = st_a2bc1
      enregRowsFiringRate[3,2] = et_a2bc1
      a2bc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2bc1)
      a2bc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      c2 = c/2
      h = enregRows[6,2] - enregRows[6,1]
      c2h = c2 + h
      
      st_c2h = et_a2bc1
      et_c2h = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[5:6,]
      enregRowsFiringRate[1,1] = st_c2h
      c2h_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2h)
      c2h_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("fga1"=fga1, "a2bc1" = a2bc1, "c2h"=c2h)
      firingRates = list("fga1"=fga1_fr, "a2bc1" = a2bc1_fr, "c2h"=c2h_fr)
      firingNeurons = list("fga1"=fga1_neurons, "a2bc1" = a2bc1_neurons, "c2h"=c2h_neurons)
      
    }
    else if (path == 4)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      st_fga1 = enregRows[1,1]
      et_fga1 = enregRows[3,1] + a1
      
      enregRowsFiringRate = enregRows[1:3,]
      enregRowsFiringRate[3,2] = et_fga1
      fga1_fr = getFiringRates(rat,enregRowsFiringRate,ses,fga1)
      fga1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      st_a2bc1 = et_fga1
      et_a2bc1 = enregRows[5,1] + c1
      
      enregRowsFiringRate = enregRows[3:5,]
      enregRowsFiringRate[1,1] = st_a2bc1
      enregRowsFiringRate[3,2] = et_a2bc1
      a2bc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2bc1)
      a2bc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      c2 = c/2
      d = enregRows[6,2] - enregRows[6,1]
      c2d = c2 + d
      
      st_c2d = et_a2bc1
      et_c2d = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[5:6,]
      enregRowsFiringRate[1,1] = st_c2d
      c2d_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2d)
      c2d_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("fga1"=fga1, "a2bc1" = a2bc1, "c2d"=c2d)
      firingRates = list("fga1"=fga1_fr, "a2bc1" = a2bc1_fr, "c2d"=c2d_fr)
      firingNeurons = list("fga1"=fga1_neurons, "a2bc1" = a2bc1_neurons, "c2d"=c2d_neurons)
    }
    else if (path == 5)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      st_dc1 = enregRows[1,1]
      et_dc1 = enregRows[2,1] + c1
      
      enregRowsFiringRate = enregRows[1:2,]
      enregRowsFiringRate[2,2] = et_dc1
      dc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,dc1)
      dc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      st_c2ba1 = et_dc1
      et_c2ba1 = enregRows[4,1] + a1
      
      enregRowsFiringRate = enregRows[2:4,]
      enregRowsFiringRate[1,1] = st_c2ba1
      enregRowsFiringRate[3,2] = et_c2ba1
      c2ba1_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2ba1)
      c2ba1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      a2 = a/2
      gf = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2gf = a2 + gf
      
      st_a2gf = et_c2ba1
      et_a2gf = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[4:6,]
      enregRowsFiringRate[1,1] = st_a2gf
      a2gf_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2gf)
      a2gf_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      turntimes = list("dc1"=dc1, "c2ba1" = c2ba1, "a2gf"=a2gf)
      firingRates = list("dc1"=dc1_fr, "c2ba1" = c2ba1_fr, "a2gf"=a2gf_fr)
      firingNeurons = list("dc1"=dc1_neurons, "c2ba1" = c2ba1_neurons, "a2gf"=a2gf_neurons)
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
      
      st_hc1 = enregRows[1,1]
      et_hc1 = enregRows[2,1] + c1
      
      enregRowsFiringRate = enregRows[1:2,]
      enregRowsFiringRate[2,2] = et_hc1
      hc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,hc1)
      hc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      c2 = c/2
      d = enregRows[3,2] - enregRows[3,1]
      c2d = c2 + d
      
      st_c2d = et_hc1
      et_c2d = enregRows[3,2]
      
      enregRowsFiringRate = enregRows[2:3,]
      enregRowsFiringRate[1,1] = st_c2d
      c2d_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2d)
      c2d_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("hc1"=hc1, "c2d" = c2d)
      firingRates = list("hc1"=hc1_fr, "c2h" = c2d_fr)
      firingNeurons = list("hc1"=hc1_neurons, "c2h" = c2d_neurons)
    }
    else if (path == 1)
    {
      
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      st_jka1 = enregRows[1,1]
      et_jka1 = enregRows[3,1] + a1
      
      enregRowsFiringRate = enregRows[1:3,]
      enregRowsFiringRate[3,2] = et_jka1
      jka1_fr = getFiringRates(rat,enregRowsFiringRate,ses,jka1)
      jka1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      a2 = a/2
      gf = sum(enregRows[4:5,2] - enregRows[4:5,1])
      a2gf = a2 + gf
      
      st_a2gf = et_jka1
      et_a2gf = enregRows[5,2]
      
      enregRowsFiringRate = enregRows[3:5,]
      enregRowsFiringRate[1,1] = st_a2gf
      a2gf_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2gf)
      a2gf_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      turntimes = list("jka1"=jka1, "a2gf" = a2gf)
      firingRates = list("jka1"=jka1_fr, "a2gf" = a2gf_fr)
      firingNeurons = list("jka1"=jka1_neurons, "a2gf" = a2gf_neurons)
      
    }
    else if (path == 2)
    {
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      st_hc1 = enregRows[1,1]
      et_hc1 = enregRows[2,1] + c1
      
      enregRowsFiringRate = enregRows[1:2,]
      enregRowsFiringRate[2,2] = et_hc1
      hc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,hc1)
      hc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      st_c2ba1 = et_hc1
      et_c2ba1 = enregRows[4,1] + a1
      
      enregRowsFiringRate = enregRows[2:4,]
      enregRowsFiringRate[1,1] = st_c2ba1
      enregRowsFiringRate[3,2] = et_c2ba1
      c2ba1_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2ba1)
      c2ba1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      a2 = a/2
      gf = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2gf = a2 + gf
      
      st_a2gf = et_c2ba1
      et_a2gf = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[4:6,]
      enregRowsFiringRate[1,1] = st_a2gf
      a2gf_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2gf)
      a2gf_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      turntimes = list("hc1"=hc1, "c2ba1" = c2ba1, "a2gf"=a2gf)
      firingRates = list("hc1"=hc1_fr, "c2ba1" = c2ba1_fr, "a2gf"=a2gf_fr)
      firingNeurons = list("hc1"=hc1_neurons, "c2ba1" = c2ba1_neurons, "a2gf"=a2gf_neurons)
      
    }
    else if (path == 3)
    {
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      st_jka1 = enregRows[1,1]
      et_jka1 = enregRows[3,1] + a1
      
      enregRowsFiringRate = enregRows[1:3,]
      enregRowsFiringRate[3,2] = et_jka1
      jka1_fr = getFiringRates(rat,enregRowsFiringRate,ses,jka1)
      jka1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      st_a2bc1 = et_jka1
      et_a2bc1 = enregRows[5,1] + c1
      
      enregRowsFiringRate = enregRows[3:5,]
      enregRowsFiringRate[1,1] = st_a2bc1
      enregRowsFiringRate[3,2] = et_a2bc1
      a2bc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2bc1)
      a2bc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      c2 = c/2
      d = enregRows[6,2] - enregRows[6,1]
      c2d = c2 + d
      
      st_c2d = et_a2bc1
      et_c2d = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[5:6,]
      enregRowsFiringRate[1,1] = st_c2d
      c2d_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2d)
      c2d_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("jka1"=jka1, "a2bc1" = a2bc1, "c2d"=c2d)
      firingRates = list("jka1"=jka1_fr, "a2bc1" = a2bc1_fr, "c2d"=c2d_fr)
      firingNeurons = list("jka1"=jka1_neurons, "a2bc1" = a2bc1_neurons, "c2d"=c2d_neurons)
      
    }
    else if (path == 4)
    {
      
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      st_jka1 = enregRows[1,1]
      et_jka1 = enregRows[3,1] + a1
      
      enregRowsFiringRate = enregRows[1:3,]
      enregRowsFiringRate[3,2] = et_jka1
      jka1_fr = getFiringRates(rat,enregRowsFiringRate,ses,jka1)
      jka1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      st_a2bc1 = et_jka1
      et_a2bc1 = enregRows[5,1] + c1
      
      enregRowsFiringRate = enregRows[3:5,]
      enregRowsFiringRate[1,1] = st_a2bc1
      enregRowsFiringRate[3,2] = et_a2bc1
      a2bc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2bc1)
      a2bc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      c2 = c/2
      h = enregRows[6,2] - enregRows[6,1]
      c2h = c2 + h
      
      st_c2h = et_a2bc1
      et_c2h = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[5:6,]
      enregRowsFiringRate[1,1] = st_c2h
      c2h_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2h)
      c2h_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("jka1"=jka1, "a2bc1" = a2bc1, "c2h"=c2h)
      firingRates = list("jka1"=jka1_fr, "a2bc1" = a2bc1_fr, "c2h"=c2h_fr)
      firingNeurons = list("jka1"=jka1_neurons, "a2bc1" = a2bc1_neurons, "c2h"=c2h_neurons)
      
    }
    else if (path == 5)
    {
      
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      st_hc1 = enregRows[1,1]
      et_hc1 = enregRows[2,1] + c1
      
      enregRowsFiringRate = enregRows[1:2,]
      enregRowsFiringRate[2,2] = et_hc1
      hc1_fr = getFiringRates(rat,enregRowsFiringRate,ses,hc1)
      hc1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      st_c2ba1 = et_hc1
      et_c2ba1 = enregRows[4,1] + a1
      
      enregRowsFiringRate = enregRows[2:4,]
      enregRowsFiringRate[1,1] = st_c2ba1
      enregRowsFiringRate[3,2] = et_c2ba1
      c2ba1_fr = getFiringRates(rat,enregRowsFiringRate,ses,c2ba1)
      c2ba1_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      
      a2 = a/2
      kj = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2kj = a2 + kj
      
      st_a2kj = et_c2ba1
      et_a2kj = enregRows[6,2]
      
      enregRowsFiringRate = enregRows[4:6,]
      enregRowsFiringRate[1,1] = st_a2kj
      a2kj_fr = getFiringRates(rat,enregRowsFiringRate,ses,a2kj)
      a2kj_neurons = getFiringNeurons(rat,enregRowsFiringRate,ses)
      
      turntimes = list("hc1"=hc1, "c2ba1" = c2ba1, "a2kj"=a2kj)
      firingRates = list("hc1"=hc1_fr, "c2ba1" = c2ba1_fr, "a2kj"=a2kj_fr)
      firingNeurons = list("hc1"=hc1_neurons, "c2ba1" = c2ba1_neurons, "a2kj"=a2kj_neurons)
    }
  }
  
  return (list("turntimes"=turntimes, "firingRates"=firingRates, "firingNeurons"=firingNeurons ));
}



getFiringRates=function(rat,enregRowsFiringRate,ses,pathTime)
{
  EnregNeurons_ses <- EnregNeurons[[rat]][[ses]]
  EnregNeurons_ses <- EnregNeurons_ses[which(EnregNeurons_ses[,3] != 0),, drop=FALSE]
  uniq.neurons.ses <- unique(EnregNeurons_ses[,c(2,3)])
  if(length(uniq.neurons.ses[,1]) > 1)
  {
    uniq.neurons.ses <- uniq.neurons.ses[order(uniq.neurons.ses[,1],uniq.neurons.ses[,2]),]
  }

  firingNeurons <- matrix(0,nrow(enregRowsFiringRate),nrow(uniq.neurons.ses))
  
  for(i in 1:nrow(enregRowsFiringRate))
  {
    boxEnter = enregRowsFiringRate[i,1]
    boxExit = enregRowsFiringRate[i,2]
    EnregNeurons_firing <- EnregNeurons_ses[which(EnregNeurons_ses[,1] > boxEnter & EnregNeurons_ses[,1] <= boxExit),, drop=FALSE]

    for(k in 1:nrow(uniq.neurons.ses))
    {
      enreg_neuron = EnregNeurons_firing[which(EnregNeurons_firing[,2]==uniq.neurons.ses[k,1] & EnregNeurons_firing[,3]==uniq.neurons.ses[k,2]),,drop=FALSE]
      firingNeurons[i,k] = nrow(enreg_neuron)
    }
    
  }
  
  firingNeuronsVec <- colSums(firingNeurons)

  firingRates = firingNeuronsVec*1000/pathTime
  return(firingRates)
}

getFiringNeurons=function(rat,enregRowsFiringRate,ses)
{
  EnregNeurons_ses <- EnregNeurons[[rat]][[ses]]
  EnregNeurons_ses <- EnregNeurons_ses[which(EnregNeurons_ses[,3] != 0),, drop=FALSE]
  uniq.neurons.ses <- unique(EnregNeurons_ses[,c(2,3)])
  if(nrow(uniq.neurons.ses) > 1)
  {
    uniq.neurons.ses <- uniq.neurons.ses[order(uniq.neurons.ses[,1],uniq.neurons.ses[,2]),]
  }
  
  firingNeurons <- matrix(0,nrow(enregRowsFiringRate),(nrow(uniq.neurons.ses)+2))
  
  for(i in 1:nrow(enregRowsFiringRate))
  {
    boxEnter = enregRowsFiringRate[i,1]
    boxExit = enregRowsFiringRate[i,2]
    EnregNeurons_firing <- EnregNeurons_ses[which(EnregNeurons_ses[,1] > boxEnter & EnregNeurons_ses[,1] <= boxExit),, drop=FALSE]
    
    firingNeurons[i,1] = boxEnter
    firingNeurons[i,2] = boxExit
    
    for(k in 3:ncol(firingNeurons))
    {
      enreg_neuron = EnregNeurons_firing[which(EnregNeurons_firing[,2]==uniq.neurons.ses[k-2,1] & EnregNeurons_firing[,3]==uniq.neurons.ses[k-2,2]),,drop=FALSE]
      firingNeurons[i,k] = nrow(enreg_neuron)
    }
    
  }
  #print(firingNeurons)
  firingNeuronsVec <- colSums(firingNeurons[,-c(1:2), drop=FALSE])
  
  return(firingNeuronsVec)
  
}

convertTurnNeuronsToHybrid=function(ratdata,ratNb,turnFiringNeurons, turnsModel, hybridModel, sim)
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
  

  if(sim != 1)
  {
    allpaths[,1:2] = allpaths[,1:2]-1
    
  }
  
  
  uniqSess = unique(as.numeric(allpaths[,5]))
  
  hybridFiringRates <- vector(mode = "list", length = length(uniqSess))
  hybridFiringNeurons <- vector(mode = "list", length = length(uniqSess))
  
  for(ses in uniqSess)
  {
    EnregNeurons_ses <- EnregNeurons[[ratNb]][[ses]]
    EnregNeurons_ses <- EnregNeurons_ses[which(EnregNeurons_ses[,3] != 0),, drop=FALSE]
    uniq.neurons.ses <- unique(EnregNeurons_ses[,c(2,3)])
    if(length(uniq.neurons.ses[,1]) > 1)
    {
      uniq.neurons.ses <- uniq.neurons.ses[order(uniq.neurons.ses[,1],uniq.neurons.ses[,2]),]
    }
    
    firingRateMat <- c()
    firingNeuronsMat <- c()
    
    allpaths_ses <- allpaths[which(as.numeric(allpaths[,5])==ses),]
    turnFiringNeurons_ses <- turnFiringNeurons[[ses]]
    
    currPath=""
    currState=""
    nodeList=""
    for(rowNb in 1:nrow(allpaths_ses)) 
    {
      row = allpaths_ses[rowNb,]
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
      
      #print(sprintf("ses=%i, rowNb=%i,path=%i",ses,rowNb,row[1]))
      actions = slot(slot(hybridModel, currState),currPath)
      for(j in 1:length(actions))
      {

        actId = which(slot(hybridModel, nodeList)==actions[j]) - 1
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
          
          select_turns = turnFiringNeurons_ses[which(turnFiringNeurons_ses[,1]==row[6]),]
          select_turns_to_merge = select_turns[res_idx,]
          firingNeurons = colSums(select_turns_to_merge[,-c(1:6), drop=FALSE])
          firingRates = firingNeurons*1000/sum(diff_times)
        }
        else
        {
          res_idx = which(turnVector %in% curr_action)
          turn_idx = which(turnsactNbvec == row[6])
          diff_times = turntimevec[turn_idx[res_idx]]
          
          select_turns = turnFiringNeurons_ses[which(turnFiringNeurons_ses[,1]==row[6]),]
          firingNeurons = select_turns[res_idx,-c(1:6),drop=FALSE]
          firingRates = firingNeurons*1000/diff_times
        }
        

        firingRateMat = rbind(firingRateMat,c(rowNb,row[1],row[2],actId,ses,sum(diff_times),firingRates))
        firingNeuronsMat = rbind(firingNeuronsMat,c(rowNb,row[1],row[2],actId,ses,sum(diff_times),firingNeurons))

        #actIdx = actIdx+1
      }
    }
    
    colnames(firingRateMat) <- c("Action", "Path", "State","Turn","Session","Duration", paste("tet",uniq.neurons.ses[,1],".",uniq.neurons.ses[,2],sep=""))
    colnames(firingNeuronsMat) <- c("Action", "Path", "State","Turn","Session","Duration", paste("tet",uniq.neurons.ses[,1],".",uniq.neurons.ses[,2],sep=""))
    
    hybridFiringRates[[ses]]   <- firingRateMat
    hybridFiringNeurons[[ses]]  <- firingNeuronsMat
    
  }
  
  return(list("firingRateMat"=hybridFiringRates,"firingNeuronsMat"=hybridFiringNeurons))
}

computePathNeuronZScores=function(ratdata,i, ratNeuronalData)
{
  rat= ratdata@rat
  firingRateZscoreData = new("RatNeuronalData", rat = rat)
  
  mazemodels <- c("Paths","Turns","Hybrid1","Hybrid2","Hybrid3","Hybrid4")
  ratFiringNeuronalData <- ratNeuronalData$firingNeuronData
  ratFiringNeuronalData_m <- slot(ratFiringNeuronalData, "Paths")
  unique_ses <- unique(ratdata@allpaths[,5])
  zscoreFirRates_m <- vector(mode = "list", length = length(unique_ses))
  for(ses in unique_ses)
  {
      #print(sprintf("ses=%i",ses))
    ratFiringNeuronalData_m_ses <- ratFiringNeuronalData_m[[ses]]
    tetIdx <- grep("tet",colnames(ratFiringNeuronalData_m_ses))
    ses_uniq.actions <- unique(ratFiringNeuronalData_m_ses[,c(1:2)])
    badpathRows <- which(ses_uniq.actions[,1]==7)
    if(length(badpathRows) > 0)
    {
      ses_uniq.actions = ses_uniq.actions[-badpathRows,]
    }
    
    zscoreMat_ses <- matrix(0,nrow(ses_uniq.actions),ncol(ratFiringNeuronalData_m_ses)-1)
    for(act in 1:nrow(ses_uniq.actions))
    {
      ratFiringNeuronalData_m_ses_act = ratFiringNeuronalData_m_ses[which(ratFiringNeuronalData_m_ses[,1]==ses_uniq.actions[act,1] & ratFiringNeuronalData_m_ses[,2]==ses_uniq.actions[act,2]),,drop=FALSE]
      zscoreVec <- ratFiringNeuronalData_m_ses_act[1,-which(colnames(ratFiringNeuronalData_m_ses_act) %in% c("Duration",colnames(ratFiringNeuronalData_m_ses_act)[tetIdx])),drop=FALSE]
      colNames.zscoremat<- colnames(zscoreVec)
      rowFirRates<- colSums(ratFiringNeuronalData_m_ses_act[,c("Duration",colnames(ratFiringNeuronalData_m_ses_act)[tetIdx]),drop=F])
      rowFirRates <- rowFirRates[-1]*1000/rowFirRates[1]
      zscoreVec <- c(zscoreVec,rowFirRates)
      zscoreMat_ses[act,] = zscoreVec
      colNames.zscoremat <- c(colNames.zscoremat,names(rowFirRates))
    }
    colnames(zscoreMat_ses) <- names(zscoreVec)
    zscoreMat_ses_tetIdx <- grep("tet",colnames(zscoreMat_ses))
    zscoreMat_ses[,zscoreMat_ses_tetIdx] = scale(zscoreMat_ses[,zscoreMat_ses_tetIdx],center=T,scale = T)
      
    zscoreFirRates_m[[ses]] <- zscoreMat_ses
  }

  return(zscoreFirRates_m)
  
}

compute_Turns_n_Hybrid_NeuronZScores=function(ratdata,i, ratNeuronalData,pathNeuronalZscores)
{
  rat= ratdata@rat
  firingRateZscoreData = new("RatNeuronalData", rat = rat)
  firingRateZscoreData@Paths = pathNeuronalZscores
  mazemodels <- c("Turns","Hybrid1","Hybrid2","Hybrid3","Hybrid4")
  for(m in mazemodels)
  {
    ratFiringNeuronalData <- ratNeuronalData$firingNeuronData
    ratFiringNeuronalData_m <- slot(ratFiringNeuronalData, m)
    unique_ses <- unique(ratdata@allpaths[,5])
    zscoreFirRates_m <- vector(mode = "list", length = length(unique_ses))
    for(ses in unique_ses)
    {
      #print(sprintf("ses=%i",ses))
      ratFiringNeuronalData_m_ses <- ratFiringNeuronalData_m[[ses]]
      tetIdx <- grep("tet",colnames(ratFiringNeuronalData_m_ses))
      ses_uniq.actions <- unique(ratFiringNeuronalData_m_ses[,c(3:4)])
      ses_uniq.actions <- ses_uniq.actions[order(ses_uniq.actions[,1],ses_uniq.actions[,2]),]
      zscoreMat_ses <- matrix(0,nrow(ses_uniq.actions),length(tetIdx)+2)
      pathCounter <- matrix(0,nrow(ses_uniq.actions),6)
      colnames(pathCounter) <- c("Path1","Path2","Path3","Path4","Path5","Path6")
      for(act in 1:nrow(ses_uniq.actions))
      {
        ratFiringNeuronalData_m_ses_act = ratFiringNeuronalData_m_ses[which(ratFiringNeuronalData_m_ses[,3]==ses_uniq.actions[act,1] & ratFiringNeuronalData_m_ses[,4]==ses_uniq.actions[act,2]),,drop=FALSE]
        pathCounter[act,] = tabulate(factor(ratFiringNeuronalData_m_ses_act[,2], levels = c(0:5)), length(c(0:5)))
        zscoreVec = ses_uniq.actions[act,]
        #colNames.zscoremat<- colnames(zscoreVec)
        rowFirRates<- colSums(ratFiringNeuronalData_m_ses_act[,c("Duration",colnames(ratFiringNeuronalData_m_ses_act)[tetIdx]),drop=F])
        rowFirRates <- rowFirRates[-1]*1000/rowFirRates[1]
        zscoreVec <- c(zscoreVec,rowFirRates)
        zscoreMat_ses[act,] = zscoreVec
        colNames.zscoremat<- names(zscoreVec)
      }
      colnames(zscoreMat_ses) <- colNames.zscoremat
      zscoreMat_ses_tetIdx <- grep("tet",colnames(zscoreMat_ses))
      zscoreMat_ses[,zscoreMat_ses_tetIdx] = scale(zscoreMat_ses[,zscoreMat_ses_tetIdx],center=T,scale = T)
      
      zscoreFirRates_m[[ses]] <- zscoreMat_ses
      zscoreFirRates_m[[ses]] <- cbind(zscoreFirRates_m[[ses]], pathCounter)
    }
    slot(firingRateZscoreData, m) = zscoreFirRates_m
    
  }
  
  return(firingRateZscoreData)
}