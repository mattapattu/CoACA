library(igraph)

generateSampleTrajectory=function()
{
  alpha = 0.1
  gamma1 = 0.9
  gamma2 = 0.8
  
  trials = 200
  
  
  H <- matrix(0,2,6)
  turnTimes <- matrix(0,0,7)
  colnames(turnTimes) <- c("Turn", "State", "Reward","Duration","Session","Action Nb", "Episode")
  
  allpaths <- matrix(0,0,7)
  colnames(allpaths) <- c("Path", "State", "Reward", "Path Duration", "sessId","Action Nb","Episode") 
  

  episodeNb = 1
  score_episode  = 0
  episodeActions <- c()
  episodeStates <- c()
  episodeDuration = 0
  resetVector = F
  changeState = F
  returnToInitState = F
  S = 0
  initState = S
  S_prime=-1
  for(i in c(1:trials))
  {
    if(resetVector)
    {
      initState = S
    }

    if(i>100)
    {
      ses = 2
    }
    else
    {
      ses = 1
    }
    
    probVec = exp(H[(S+1),])/sum(exp(H[(S+1),]))     
    
    A=sample(0:5,1,prob=probVec)
    S_prime=aca_getNextState(S,A)
    
    R=0
    if(A == 3)
    {
      R = 1
      score_episode = score_episode + 1
    }
    
    episodeActions <- c(episodeActions, A)
    episodeStates <- c(episodeStates,S)
  
    turns <- getTurnsFromPath(S,A,TurnModel)
    for(turn in c(1:length(turns)))
    {
      turnString <- turns[turn]
      turnId <- getTurnid(S,turnString,TurnModel)
      turnTime = 1
      turnReward = 0
      if(A==3 && turn==length(turns))
      {
        turnReward = 1
      }
      turnVec <- rep(0, 7)
      
      turnVec[1] = turnId
      turnVec[2] = S
      turnVec[3] = turnReward
      turnVec[4] = 1
      turnVec[5] = ses
      turnVec[6] = i
      turnVec[6] = episodeNb
      turnTimes <- rbind(turnTimes,turnVec)
      episodeDuration = episodeDuration + 1
    }
    
    
    allpathsRow <- rep(0,8)
    allpathsRow[1] = A
    allpathsRow[2] = S
    allpathsRow[3] = R
    allpathsRow[4] = length(turns)
    allpathsRow[5] = ses
    allpathsRow[6] = i
    allpathsRow[7] = episodeNb
    allpaths<- rbind(allpaths,allpathsRow)
    #print(sprintf("Current state = %i, Action = %i", S,A))
    if(S_prime!=initState){
      changeState = T
    }else if(S_prime==initState && changeState){
      returnToInitState = T
    }

    
    
    H = gamma1 * H
    ## Check if episode ended
    if(returnToInitState)
    {
      changeState = F
      returnToInitState = F
      resetVector = F
      episodeNb  = episodeNb + 1
      #print(sprintf("episodeNb=%i, episodeDuration=%f",episodeNb,episodeDuration))
      state0_idx=which(episodeStates==0)
      state1_idx=which(episodeStates==1)
      
      uniq_actions_s0 = unique(episodeActions[state0_idx])
      
      for(action_s0 in uniq_actions_s0){
        turns <- getTurnsFromPath(0,action_s0,TurnModel)
        pathDuration = length(turns)
        nbOfOccurence = length(which(action_s0==episodeActions[state0_idx]))
        activity = pathDuration*nbOfOccurence/episodeDuration
        H[1,(action_s0+1)]=H[1,(action_s0+1)]+alpha*(score_episode*activity)
        #print(sprintf("S=0,A=%i,pathDuration=%f, activity=%f, H[S,A]=%f",action_s0,pathDuration,activity,H[1,(action_s0+1)]))
      }
      
      
      uniq_actions_s1 = unique(episodeActions[state1_idx])
      
      for(action_s1 in uniq_actions_s1){
        
        turns <- getTurnsFromPath(1,action_s1,TurnModel)
        pathDuration = length(turns)
        nbOfOccurence = length(which(action_s1==episodeActions[state1_idx]))
        activity = pathDuration*nbOfOccurence/episodeDuration
        H[2,(action_s1+1)]=H[2,(action_s1+1)]+alpha*(score_episode*activity)
        #print(sprintf("S=0,A=%i,pathDuration=%f, activity=%f, H[S,A]=%f",action_s1,pathDuration,activity,H[2,(action_s1+1)]))
      }
      
      #print(H)
      ## reset rewards
      score_episode = 0
      episodeDuration = 0
      episodeActions <- c()
      episodeStates <- c()
      
    }
    ### End of episode checkd
    S=S_prime
  }
  simData = new("RatData", rat = "simulation",allpaths = allpaths, turnTimes = turnTimes)
  return(simData)
}
  

getNodeCredits=function(ratdata, modelData, testModel,sim=1)
{
  alpha = modelData@alpha
  gamma1 = modelData@gamma1
  gamma2 = modelData@gamma2
  
  trials = length(ratdata@allpaths[,1])
  
  
  #H <- matrix(0,2,6)
  probMatrix <- matrix(0,trials,13)
  
  episodeNb = 0
  episodeActions <- c()
  episodeStates <- c()
  episodeTurnTimes <- c()

  allpaths <- ratdata@allpaths
  model <- modelData@Model
  
  if(model == "Paths")
  {
    turnTimes = slot(ratdata,"allpaths")
  }
  else if(model == "Turns")
  {
    turnTimes = slot(ratdata,"turnTimes")
  }
  else if(model == "Hybrid1")
  {
    turnTimes = slot(ratdata,"hybridModel1")
  }
  else if(model == "Hybrid2")
  {
    turnTimes = slot(ratdata,"hybridModel2")
  }
  else if(model == "Hybrid3")
  {
    turnTimes = slot(ratdata,"hybridModel3")
  }
  else if(model == "Hybrid4")
  {
    turnTimes = slot(ratdata,"hybridModel4")
  }
  
 
 
  
  lik = rep(0,trials)
  
  S0 <- generateGraph(testModel,0)
  S1 <- generateGraph(testModel,1)
  
  
  trialId = 1
  uniqueses = unique(allpaths[,5])
  for(ses in uniqueses)
  {
    allpaths_ses <- allpaths[which(allpaths[,5]==ses),]
    trials_ses <- length(allpaths_ses[,1])
    turnTimes_ses <- turnTimes[which(turnTimes[,5]==ses),]
    turnTimeId = 0
    episodeDuration = 0
    score_episode  = 0
    resetVector = T
    changeState = F
    returnToInitState = F
    
    if(sim==2)
    {
      S = allpaths_ses[1,2] - 1
    }
    else
    {
      S = allpaths_ses[1,2]
    }
    for(i in c(1:trials_ses))
    {
      if(resetVector)
      {
        initState = S
        resetVector = F;
      }
      
      if(sim == 2)
      {
        A=allpaths_ses[i,1]-1
      }
      else
      {
        A=allpaths_ses[i,1]
      }
      
      
      if(i < trials_ses)
      {
        if(sim == 2)
        {
          S_prime=allpaths_ses[(i+1),2]-1
        }
        else
        {
          S_prime=allpaths_ses[(i+1),2]
        }
        
      }
      else
      {
        S_prime = -1
      }
      
      print(sprintf("ses=%i,i=%i,S=%i,A=%i,S_prime=%i",ses,i,S,A,S_prime))
      
      R=allpaths[i,3]
      if(A == 3)
      {
        R = 1
        score_episode = score_episode + 1
      }
      
      # if(A > 5)
      # {
      #   S = S_prime
      #   #next
      # }
      
      if(S==0)
      {
        g <- S0
      }
      else
      {
        g <- S1
      }
      
      pathProb = 1
      turns <- getTurnsFromPath(S,A,testModel)
      
      for(turn in turns)
      {
        turnTimeId = turnTimeId + 1 
        if(sim == 1)
        {
          curr_turn_time <- turnTimes_ses[turnTimeId,4]
        }
        else if (model == "Paths")
        {
          curr_turn_time = turnTimes_ses[turnTimeId,4];
        }
        else{
          curr_turn_time <- turnTimes_ses[turnTimeId,6]
        }
        
        
        episodeDuration = episodeDuration + curr_turn_time
        episodeActions <- c(episodeActions, turn)
        episodeStates <- c(episodeStates,S)
        episodeTurnTimes <- c(episodeTurnTimes,curr_turn_time)
        
        nodeId <- which(V(g)$name == turn)
        order1 <- ego(g, order = 1, nodes = nodeId, mode = c("all", "out", "in"),mindist = 0)[[1]]
        parent <- order1[2]
        parentId <- which(V(g)$name == parent$name)
        siblingNames<-ego(g, order = 1, nodes = parentId, mode = c("out"),mindist = 0)[[1]]
        siblingCredits <- siblingNames[-1]$credit
        curr_turn_credit <- V(g)[nodeId]$credit
        prob_turn <- exp(V(g)[nodeId]$credit)/sum(exp(siblingCredits))
        pathProb = pathProb*prob_turn
        
      }
      
      lik[trialId] = log(pathProb)
      trialId = trialId + 1
      
      #print(sprintf("Current state = %i, Action = %i", S,A))
      if(S_prime!=initState)
      {
        changeState = T
      }else if(S_prime==initState && changeState){
        returnToInitState = T
      }
      
      # probMatrix[i,13]=i
      # if(S==0)
      # {
      #   probMatrix[i,7:12]=0
      #   x <- exp(H[(S+1),])/sum(exp(H[(S+1),]))
      #   probMatrix[i,1:6] = x
      # }
      # else if(S==1)
      # {
      #   probMatrix[i,1:6]=0
      #   x <- exp(H[(S+1),])/sum(exp(H[(S+1),]))
      #   probMatrix[i,7:12]= x
      # }
      # 
      # if(! lik[i] %in% log(x))
      # {
      #   print(sprintf("Error: Check if likelihood matches probMatrix computation"))
      # }
      
      
      #H = gamma1 * H
      #V(S0)$credit <- V(S0)$credit*gamma1
      #V(S1)$credit <- V(S1)$credit*gamma1
      ## Check if episode ended
      if(returnToInitState)
      {
        changeState = F
        returnToInitState = F
        resetVector = T
        episodeNb  = episodeNb + 1
        print(sprintf("episodeNb=%i, episodeDuration=%f",episodeNb,episodeDuration))
        state0_idx=which(episodeStates==0)
        state1_idx=which(episodeStates==1)
        
        uniq_actions_s0 = unique(episodeActions[state0_idx])
        print(sprintf("Unqiue actions S0: %s",paste(uniq_actions_s0, collapse=" ")))
        
        for(action_s0 in uniq_actions_s0){
          #turns <- getTurnsFromPath(0,action_s0,TurnModel)
          nodeId <- which(V(S0)$name == action_s0)
          turnDuration = sum(episodeTurnTimes[which(episodeActions == action_s0 & episodeStates == 0)])
          #nbOfOccurence = length(which(action_s0==episodeActions[state0_idx]))
          
          activity = turnDuration/episodeDuration
          #H[1,(action_s0+1)]=H[1,(action_s0+1)]+alpha*(score_episode*activity)
          
          deltaH = alpha*(score_episode*activity)
          prevNodeCredit = V(S0)[nodeId]$credit
          V(S0)[nodeId]$credit <- V(S0)[nodeId]$credit + deltaH
          print(sprintf("Turn=%s,S=0,turnTime=%f, activity=%f,prevNodeCred=%f,deltaH=%f,nodeCredit=%f",action_s0,turnDuration,activity,prevNodeCredit,deltaH,V(S0)[nodeId]$credit))
        }
        
        
        uniq_actions_s1 = unique(episodeActions[state1_idx])
        print(sprintf("Unqiue actions S1: %s",paste(uniq_actions_s1, collapse=" ")))
        for(action_s1 in uniq_actions_s1){
          
          #turns <- getTurnsFromPath(1,action_s1,TurnModel)
          nodeId <- which(V(S1)$name == action_s1)
          turnDuration = sum(episodeTurnTimes[which(episodeActions == action_s1 & episodeStates == 1)])
          #nbOfOccurence = length(which(action_s1==episodeActions[state1_idx]))
          activity = turnDuration/episodeDuration
          #H[2,(action_s1+1)]=H[2,(action_s1+1)]+alpha*(score_episode*activity)
          prevNodeCredit = V(S1)[nodeId]$credit
          deltaH =  alpha*(score_episode*activity)
          V(S1)[nodeId]$credit <- V(S1)[nodeId]$credit + deltaH
          print(sprintf("Turn=%s,S=1,turnTime=%f,activity=%f,prevNodeCred=%f,deltaH=%f,nodeCredit=%f",action_s1,turnDuration,activity,prevNodeCredit,deltaH,V(S1)[nodeId]$credit))
        }
        
        #print(H)
        ## reset rewards
        score_episode = 0
        episodeDuration = 0
        episodeActions <- c()
        episodeStates <- c()
        episodeTurnTimes <- c()
      }
      ### End of episode checkd
      S=S_prime
    }
    
    V(S0)$credit <- V(S0)$credit*gamma1
    V(S1)$credit <- V(S1)$credit*gamma1
    
  }
  
  return(list(likelihood=lik,probMatrix=probMatrix,nodes = list(S0=as_ids(V(S0)),S1=as_ids(V(S1))),credits=list(S0=V(S0)$credit,S1=V(S1)$credit)))
}



generateGraph=function(testModel,state)
{
  if(state == 0)
  {
    edges = slot(testModel,"edges.S0")
  }
  else
  {
    edges = slot(testModel,"edges.S1")
  }
  edgeNb = length(edges)
  df <- data.frame(V1=character(), 
                   V2=character())  
  for(i in c(1:edgeNb))
  {
    curr_edge <- slot(edges[[i]],"edge")
    df<-rbind(df,data.frame(read.table(text=paste(curr_edge,collapse=' ')),stringsAsFactors=FALSE))
    
  }
  graph <- graph.data.frame(df)
  graph<-set_vertex_attr(graph, name="credit", value=0)
  graph<-set_vertex_attr(graph, name="probability", value=0)
  #plot(g0,layout=layout_(g0, as_tree()),vertex.shape="none",vertex.label.cex=0.8)
  return(graph)
}


plotGraphs = function(src.dir)
{
  source(paste(src.dir,"TurnModel.R", sep="/"))
  source(paste(src.dir,"HybridModel1.R", sep="/"))
  source(paste(src.dir,"HybridModel2.R", sep="/"))
  source(paste(src.dir,"HybridModel2.R", sep="/"))
  source(paste(src.dir,"HybridModel3.R", sep="/"))
  source(paste(src.dir,"HybridModel4.R", sep="/"))
  
  #modelFiles <- c("TurnModel.R","HybridModel1.R","HybridModel2.R","HybridModel2.R","HybridModel3.R","HybridModel4.R")
  models <- c("Turns","Hybrid1","Hybrid2","Hybrid3","Hybrid4")
  allModels = new("AllModels",Turns = TurnModel,Hybrid1 = Hybrid1,Hybrid2 = Hybrid2, Hybrid3 = Hybrid3,Hybrid4 = Hybrid4)
  
  for(model in models)
  {

    g0 <- generateGraph(slot(allModels,model),0)
    g1 <- generateGraph(slot(allModels,model),1)
    par(mfrow=c(1,2))
    plot(g0,layout=layout_(g0, as_tree()),vertex.shape="none",vertex.label.cex=0.8)
    plot(g1,layout=layout_(g1, as_tree()),vertex.shape="none",vertex.label.cex=0.8)
    title(main=print(paste0(model,", Model1")),outer=T, line=-2)
    
  }
}

getTurnsFromPath=function(state, action, testModel)
{
  if(state == 0)
  {
    graph = slot(testModel,"S0")
  }
  else
  {
    graph = slot(testModel,"S1")
  }
  
  if(action == 0)
  {
    Path = "Path0"
  }
  else if(action == 1)
  {
    Path = "Path1"
  }
  else if(action == 2)
  {
    Path = "Path2"
  }
  else if(action == 3)
  {
    Path = "Path3"
  }
  else if(action == 4)
  {
    Path = "Path4"
  }
  else if(action == 5)
  {
    Path = "Path5"
  }
  
  if(action > 5)
  {
    turns <- c()
  }
  else
  {
    turns <- slot(graph,Path)
  }
  return(turns)
}

getTurnid = function(state,turnString,testModel)
{
  if(state == 0)
  {
    graph = slot(testModel,"nodes.S0")
  }
  else
  {
    graph = slot(testModel,"nodes.S1")
  }
  
  turnId <- which(graph==turnString) -1
  
  return(turnId)
  
}


aca_getNextState=function(curr_state, action)
{
  new_state = -1;
  if (action == 4 || action == 5)
  {
    new_state = curr_state;
  }
  else if (curr_state == 0)
  {
    new_state = 1;
  }
  else if (curr_state == 1)
  {
    new_state = 0;
  }
  
  return (new_state);
}



testCode=function(ratdata)
{
  #models = c("Paths","Turns","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  # ratdata <- generateSampleTrajectory()
  # ratdata@hybridModel1 = convertTurnTimes(ratdata,TurnModel,Hybrid1,sim=1)
  # ratdata@hybridModel2 = convertTurnTimes(ratdata,TurnModel,Hybrid2,sim=1)
  # ratdata@hybridModel3 = convertTurnTimes(ratdata,TurnModel,Hybrid3,sim=1)
  # ratdata@hybridModel4 = convertTurnTimes(ratdata,TurnModel,Hybrid4,sim=1)
  models = c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns")
  creditAssgmnt = "aca2"
  for(model in models)
  {
    modelData <- new("ModelData", Model = model, creditAssignment = creditAssgmnt, sim = 2)
    modelData@alpha=0.2017198
    modelData@gamma1=0.9
    modelData@gamma2=1
    testModel = slot(allModels,model)
    # lik1<-exp(TurnsNew::getTurnsLikelihood(ratdata,modelData,testModel,sim=2))
    # lik2<-exp(getTurnsLikelihood(ratdata,modelData,testModel,sim=2))
    # res<- getNodeCredits(ratdata,modelData,testModel)
    # lik3 <- exp(res$likelihood)
    # print(sprintf("Model=%s,lik1=%f,lik2=%f,lik3=%f",model,lik1,lik2,lik3))
    
    res<- getNodeCredits(ratdata,modelData,testModel,sim=2)
    
    res2<-TurnsNew::debugAca2Likelihood(ratdata,modelData,testModel,sim=2)
    print(sort(res2$S0$nodes))
    print(res2$S0$credits[order(res2$S0$nodes)])
    print(res$credits$S0[order(res$nodes$S0)])
    

    if(identical(res2$S0$credits[order(res2$S0$nodes)],res$credits$S0[order(res$nodes$S0)]))
    {
      print(sprintf("S0 credits are identical for %s", model))
    }
    else
    {
      print(sprintf("S0 credits are not identical for %s", model))
    }

    print(sort(res2$S1$nodes))
    print(res2$S1$credits[order(res2$S1$nodes)])
    print(res$credits$S1[order(res$nodes$S1)])
    
    if(identical(res2$S1$credits[order(res2$S1$nodes)],res$credits$S1[order(res$nodes$S1)]))
    {
      print(sprintf("S1 credits are identical for %s", model))
    }
    else
    {
      print(sprintf("S1 credits are not identical %s", model))
    }
  }
  
}


