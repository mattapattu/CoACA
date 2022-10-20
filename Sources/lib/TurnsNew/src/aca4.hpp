
#ifndef __ACA4__
#define __ACA4__

#include "aca3CreditUpdate.hpp"
#include "utils.hpp"


std::vector<double> getAca4Likelihood(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug=false)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  
  if(model == "Paths")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  }
  else if(model == "Turns")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("turnTimes"));
  }
  else if(model == "Hybrid1")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel1"));
  }
  else if(model == "Hybrid2")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel2"));
  }
  else if(model == "Hybrid3")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel3"));
  }
  else if(model == "Hybrid4")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel4"));
  }
  
  //Rcpp::List nodeGroups = Rcpp::as<Rcpp::List>(testModel.slot("nodeGroups"));
  
  double alpha = Rcpp::as<double>(modelData.slot("alpha"));
  double gamma = Rcpp::as<double>(modelData.slot("gamma1"));
  //int episode = 0;
  double reward = Rcpp::as<double>(modelData.slot("gamma2"));
  reward = reward * 10;
  double power = Rcpp::as<double>(modelData.slot("lambda"));
  
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;
  
  std::vector<double> mseMatrix;
  //int mseRowIdx = 0;
  
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  
  arma::vec turnTime_method;
  if (sim == 1)
  {
    turnTime_method = turnTimes.col(3);
  }
  else if (model == "Paths")
  {
    turnTime_method = turnTimes.col(3);
  }
  else
  {
    turnTime_method = turnTimes.col(5);
  }
  
  int episode = 1;
  int actionCounter = 0;
  
  Graph S0(testModel, 0);
  //S0.printGraph();
  Graph S1(testModel, 1);
  //S1.printGraph();
  
  //Rcpp::Rcout <<"rootS1.turn ="<<rootS1->turn<<std::endl;
  //Rcpp::Rcout <<"rootS2.turn ="<<rootS2->turn<<std::endl;
  
  for (unsigned int session = 0; session < uniqSessIdx.n_elem; session++)
  {
    
    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout <<"sessId="<<sessId<<std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    
    arma::uvec turnTimes_idx; 
    if (model == "Paths")
    {
      turnTimes_idx = arma::find(sessionVec == sessId); ;
    }
    else
    {
      turnTimes_idx = arma::find(turnTimes.col(4) == sessId); 
    }
    arma::vec turn_times_session = turnTime_method.elem(turnTimes_idx);
    arma::uword session_turn_count = 0;
    
    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    double score_episode = 0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S;
    if(sim == 1)
    {
      S = states_sess(0); 
    }
    else
    {
      S = states_sess(0) - 1; 
    }
    int A = 0;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;
    
    std::vector<int> episodeStates;
    std::vector<int> episodeActions;
    
    for (int i = 0; i < nrow; i++)
    {
      
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }
      
      int R = rewards_sess(i);
      
      if (R > 0)
      {
        score_episode = score_episode + reward;
      }
      
      if (sim == 1)
      {
        A = actions_sess(i);
      }
      else
      {
        A = actions_sess(i) - 1;
      }
      
      int S_prime = 0;
      if(i < (nrow-1))
      {
        if (sim == 1)
        {
          S_prime = states_sess(i + 1);
        }
        else
        {
          S_prime = states_sess(i + 1) - 1;
        }
      }
      
      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }
      
      Rcpp::StringVector turns;
      if(S==0)
      {
        turns = S0.getTurnsFromPaths(A);
      } 
      else
      {
        turns = S1.getTurnsFromPaths(A);
      }
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"turns="<< turns << std::endl;
      
      Node *prevNode;
      Node *currNode;
      Graph graph;
      if (S == 0)
      {
        graph = S0;
        prevNode = graph.getNode("E");
      }
      else
      {
        graph = S1;
        prevNode = graph.getNode("I");
      }
      double pathProb = 1;
      for (int j = 0; j < nbOfTurns; j++)
      {
        actionCounter = actionCounter+1;
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph.getNode(currTurn);
        //currNode->credit = currNode->credit + 1; //Test
        //Rcpp::Rcout <<"currNode="<< currNode->node<<std::endl;
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turn_times_session(session_turn_count));
        
        Edge edge = graph.getEdge(prevNode->node, currNode->node);
        double prob_a = edge.probability;
        pathProb = pathProb* prob_a;      
        //Rcpp::Rcout <<"prob_a="<< prob_a << ", pathProb=" <<pathProb <<std::endl;
        
        session_turn_count++;
        prevNode = currNode;
      }
      if(A != 6)
      {
        double logProb = log(pathProb);
        mseMatrix.push_back(logProb);
      }
      else
      {
        mseMatrix.push_back(0);
      }
      
      if(debug)
      {
        Rcpp::Rcout <<"i="<< i << ", S=" << S <<", A=" << A<< ", reward=" << reward << ", pathProb=" << pathProb << std::endl;
      }
      
      episodeStates.push_back(S);
      episodeActions.push_back(A);
      
      //log_lik=log_lik+ logProb;
      
      //Check if episode ended
      if (returnToInitState || (i==nrow-1))
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;
        
        //episode = episode+1;
        
        
        std::stringstream actionString;
        std::stringstream stateString;
        std::copy(episodeActions.begin(), episodeActions.end(), std::ostream_iterator<int>(actionString, " "));
        std::copy(episodeStates.begin(), episodeStates.end(), std::ostream_iterator<int>(stateString, " "));
        //Rcpp::Rcout << "episodeNb=" << episode <<  "Paths:" << actionString.str() <<  "States:" << stateString.str() << std::endl;
        
        
        //double alpha_prime = getAlphaPrime(alpha,episodeNb);
        double alpha_prime = alpha/(double) std::pow(actionCounter,power);
        if(debug)
        {
          Rcpp::Rcout<< "alpha=" << alpha << ", actionCounter=" << actionCounter << ", power=" << power << ", alpha_prime=" << alpha_prime << ", score_episode="  << score_episode << std::endl;
        }
        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha_prime, score_episode, &S0, &S1);
        //Rcpp::Rcout << "Update S0 probabilities"<<std::endl;
        S0.updateEdgeProbs();
        //Rcpp::Rcout << "Update S1 probabilities"<<std::endl;
        S1.updateEdgeProbs();
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        //Rcpp::Rcout << "S0 credits: ";
        //S0.printCredits(true);
        //Rcpp::Rcout << std::endl;
        //Rcpp::Rcout << "S1 credits: ";
        //Rcpp::Rcout << std::endl;
        //S1.printCredits(true);
        
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
        episodeStates.clear();
        episodeActions.clear();
      }
      S = S_prime;
      //trial=trial+1;
    }
    S0.decayCredits(gamma);
    S1.decayCredits(gamma);
    S0.updateEdgeProbs(false);
    S1.updateEdgeProbs(false);
    
  }
  //Rcpp::Rcout << "S0 credits: " << std::endl;
  //S0.printCredits(debug);
  //Rcpp::Rcout << "S1 credits: " << std::endl;
  //S1.printCredits(debug);
  return (mseMatrix);
}


arma::mat getAca4ProbMatrix(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  
  if(model == "Paths")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  }
  else if(model == "Turns")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("turnTimes"));
  }
  else if(model == "Hybrid1")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel1"));
  }
  else if(model == "Hybrid2")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel2"));
  }
  else if(model == "Hybrid3")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel3"));
  }
  else if(model == "Hybrid4")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel4"));
  }
  //Rcpp::List nodeGroups = Rcpp::as<Rcpp::List>(testModel.slot("nodeGroups"));
  
  double alpha = Rcpp::as<double>(modelData.slot("alpha"));
  double gamma = Rcpp::as<double>(modelData.slot("gamma1"));
  double reward = Rcpp::as<double>(modelData.slot("gamma2"));
  reward = reward * 10;
  double power = Rcpp::as<double>(modelData.slot("lambda"));
  
  int episodeNb = 0;
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;
  
  arma::mat mseMatrix;
  //int mseRowIdx = 0;
  
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec allpaths_pathNb = allpaths.col(5);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  
  arma::vec turnTime_method;
  if (sim == 1)
  {
    turnTime_method = turnTimes.col(3);
  }
  else if (model == "Paths")
  {
    turnTime_method = turnTimes.col(3);
  }
  else
  {
    turnTime_method = turnTimes.col(5);
  }
  
  int episode = 1;
  int actionCounter = 0;
  
  Graph S0(testModel, 0);
  Graph S1(testModel, 1);
  
  Debugger logger;
  logger.setDebug(debug);
  //Rcpp::Rcout <<"Print S0"<<std::endl;
  //S0.printGraph();
  //Rcpp::Rcout <<"Print S1"<<std::endl;
  //S1.printGraph();
  
  //Rcpp::Rcout <<"rootS1.turn ="<<rootS1->turn<<std::endl;
  //Rcpp::Rcout <<"rootS2.turn ="<<rootS2->turn<<std::endl;
  
  for (unsigned int session = 0; session < uniqSessIdx.n_elem; session++)
  {
    
    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout <<"sessId="<<sessId<<std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec allpaths_pathNb_sess = allpaths_pathNb.elem(sessionIdx);
    
    arma::uvec turnTimes_idx; 
    if (model == "Paths")
    {
      turnTimes_idx = arma::find(sessionVec == sessId); ;
    }
    else
    {
      turnTimes_idx = arma::find(turnTimes.col(4) == sessId); 
    }
    arma::vec turn_times_session = turnTime_method.elem(turnTimes_idx);
    
    arma::uword session_turn_count = 0;
    
    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    double score_episode = 0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S;
    if(sim == 1)
    {
      S = states_sess(0); 
    }
    else
    {
      S = states_sess(0) - 1; 
    }
    int A = 0;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;
    //Rcpp::Rcout <<"nrow="<<nrow<<std::endl;
    for (int i = 0; i < nrow; i++)
    {
      
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }
      
      int R = rewards_sess(i);
      
      if (R > 0)
      {
        score_episode = score_episode + reward;
      }
      
      
      if (sim == 1)
      {
        A = actions_sess(i);
      }
      else
      {
        A = actions_sess(i) - 1;
      }
      
      int S_prime = 0;
      if(i < (nrow-1))
      {
        if (sim == 1)
        {
          S_prime = states_sess(i + 1);
        }
        else
        {
          S_prime = states_sess(i + 1) - 1;
        }
      }
      
      
      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }
      
      
      //Rcpp::Rcout <<"i="<< i << ", S=" << S <<", A=" << A << ", ses=" << sessId<<std::endl;
      std::ostringstream msg; 
      msg << "i="<< i << ", S=" << S <<", A=" << A <<", pathNb=" << allpaths_pathNb_sess(i);;
      //logger.Print(msg.str()); 
      
      Rcpp::StringVector turns;
      if(S==0)
      {
        turns = S0.getTurnsFromPaths(A);
      } 
      else
      {
        turns = S1.getTurnsFromPaths(A);
      }
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"Path="<< A << ", turns=" << turns<<std::endl;
      msg.str("");
      msg << "turns=";
      //logger.PrintRcppVec(msg.str(),turns); 
      
      Node *prevNode;
      Node *currNode;
      Graph graph;
      if (S == 0)
      {
        graph = S0;
        prevNode = graph.getNode("E");
      }
      else
      {
        graph = S1;
        prevNode = graph.getNode("I");
      }
      
      double pathProb = 1;
      for (int j = 0; j < nbOfTurns; j++)
      {
        actionCounter = actionCounter+1;
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph.getNode(currTurn);
        //Edge edge = graph.getEdge(prevNode->node, currNode->node);
        int turnId = graph.getNodeIndex(currTurn);
        msg.str("");
        msg << "currTurn=" << currTurn << ", turnId=" << turnId <<", session_turn_count=" <<session_turn_count;
        //logger.Print(msg.str()); 
        
        double turntime = turn_times_session(session_turn_count);
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turntime);
        
        msg  << ", turntime=" << turntime;
        //logger.Print(msg.str()); 
        
        
        Edge edge = graph.getEdge(prevNode->node, currNode->node);
        double prob_a = edge.probability;
        
        pathProb = pathProb* prob_a; 
        //Rcpp::Rcout <<"src=" <<prevNode->node << ", dest =" <<  currNode->node  << ", prob_a="<< prob_a << ", pathProb=" <<pathProb <<std::endl;     
        prevNode = currNode;
        session_turn_count++;
      }
      
      //Rcpp::Rcout << "pathProb=" << pathProb << std::endl;
      arma::rowvec probRow(13);
      probRow.fill(-1);
      probRow(12) = allpaths_pathNb_sess(i);
      
      for (int path = 0; path < 6; path++)
      {
        //Rcpp::Rcout << "path=" << path << ", state=" << S << std::endl;
        
        Rcpp::StringVector turnVec;
        if (S == 0)
        {
          turnVec = S0.getTurnsFromPaths(path);
          turnVec.push_front("E");
        }
        else
        {
          turnVec = S1.getTurnsFromPaths(path);
          turnVec.push_front("I");
        }
        //Rcpp::Rcout << "turnVec=" << turnVec << std::endl;
        // msg.str("");
        // msg << "turnVec=";
        // logger.PrintRcppVec(msg.str(),turnVec); 
        double pathProb = 1;
        for (int k = 0; k < (turnVec.length() - 1); k++)
        {
          std::string turn1 = Rcpp::as<std::string>(turnVec[k]);
          std::string turn2 = Rcpp::as<std::string>(turnVec[k + 1]);
          //Rcpp::Rcout << "turn1=" << turn1 << ", turn2=" << turn2 << std::endl;
          
          Edge e;
          if (S == 0)
          {
            e = S0.getEdge(turn1, turn2);
          }
          else
          {
            e = S1.getEdge(turn1, turn2);
          }
          
          //Rcpp::Rcout << "Edge prob=" << e.probability << std::endl;
          pathProb = e.probability * pathProb;
        }
        int index = path + (6 * S);
        //Rcpp::Rcout << "index=" << index << ", pathProb=" << pathProb << std::endl;
        probRow[index] = pathProb;
      }
      //Rcpp::Rcout << "probRow=" << probRow << std::endl;
      mseMatrix = arma::join_vert(mseMatrix, probRow);
      
      //log_lik=log_lik+ logProb;
      
      //Check if episode ended
      if (returnToInitState || (i==nrow-1))
      {
        //Rcpp::Rcout << "Inside end episode, episodeNb=" << episodeNb << std::endl;
        msg.str("");
        msg <<"Inside end episode";
        //logger.Print(msg.str()); 
        changeState = false;
        returnToInitState = false;
        
        episodeNb = episodeNb+1;
        //double alpha_prime = getAlphaPrime(alpha,episodeNb);
        
        // for(unsigned int m=0; m<episodeTurns.size(); m++)
        // {
        //   Rcpp::Rcout << episodeTurns[m] << "," <<episodeTurnStates[m] << "," <<episodeTurnTimes[m] << std::endl;
        // }
        double alpha_prime = alpha/(double) std::pow(actionCounter,power);
        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha_prime, score_episode, &S0, &S1);
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
        
        //S0.printCredits(false);
        //S1.printCredits(false);
        
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }
      // msg.str("");
      // msg <<"Here1";
      // logger.Print(msg.str());
      
      S = S_prime;
      // msg.str("");
      // msg <<"Here2";
      // logger.Print(msg.str());
      //trial=trial+1;
    }
    
    //Rcpp::Rcout << "End of session=" << sessId << std::endl;
    S0.decayCredits(gamma);
    S1.decayCredits(gamma);
    S0.updateEdgeProbs(false);
    S1.updateEdgeProbs(false);
    
    //Rcpp::Rcout <<  "Here3"<<std::endl;
  }
  
  return (mseMatrix);
}

#endif
