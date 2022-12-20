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