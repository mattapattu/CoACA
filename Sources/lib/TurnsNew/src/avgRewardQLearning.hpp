#ifndef __avgRewardQLearning__
#define __avgRewardQLearning__
#include "utils.hpp"
using namespace Rcpp;

//using namespace Rcpp;

//namespace sarsa{
Rcpp::List simulateQlearning(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, arma::vec turnStages, bool debug)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  if (model == "Turns")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("turnTimes"));
  }
  else if (model == "Hybrid1")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel1"));
  }
  else if (model == "Hybrid2")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel2"));
  }
  else if (model == "Hybrid3")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel3"));
  }
  else if (model == "Hybrid4")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel4"));
  }
  Rcpp::List nodeGroups = Rcpp::as<Rcpp::List>(testModel.slot("nodeGroups"));
  
  double alpha = Rcpp::as<double>(modelData.slot("alpha"));
  double gamma = Rcpp::as<double>(modelData.slot("gamma"));
  //double gamma2 = Rcpp::as<double>(modelData.slot("gamma2"));
  
  //Rcpp::Rcout << "model=" << model << ", turnMethod=" << turnMethod << std::endl;
  arma::mat R = arma::zeros(2, 6);
  R(0, 3) = 1;
  R(1, 3) = 1;
  
  arma::mat generated_PathData;
  arma::mat generated_TurnData;
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  //arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_duration = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec pathNb = allpaths.col(5);
  
  arma::vec all_turns = turnTimes.col(3);
  arma::vec turns_sessions = turnTimes.col(4);
  
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  Graph S0(testModel, 0);
  Graph S1(testModel, 1);
  
  int actionNb = 0;
  
  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {
    
    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" << session << ", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    
    arma::uvec turns_sessIdx = arma::find(turns_sessions == (sessId));
    arma::vec turns_sess = all_turns.elem(turns_sessIdx);
    
    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    double avg_score = 0;
    double score_episode = 0;
    int episode = 1;
    
    int S = states_sess(0) - 1;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;
    
    arma::mat generated_PathData_sess(nrow, 7);
    arma::mat generated_TurnsData_sess((nrow * 3), 7);
    generated_PathData_sess.fill(-1);
    generated_TurnsData_sess.fill(-1);
    unsigned int turnIdx = 0; // counter for turn model
    //All episodes in new session
    //Rcpp::Rcout << "nrow=" << nrow << std::endl;
    int totalPaths = nrow;
    while (actionNb <= totalPaths)
    {
      actionNb++;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }
      //Rcpp::Rcout <<"i=" <<i <<", S=" <<S  << std::endl;
      Node *currNode;
      Node *prevNode;
      std::vector<Edge> *edges;
      Rcpp::StringVector turnNames;
      Graph graph;
      
      Rcpp::IntegerVector turns_index;
      double pathDuration = 0;
      int turnReward = 0;
      int last_turn = 0;
      int S_prime = 0;
      
      //simulate an episode
      while (!returnToInitState)
      {
        if (S == 0)
        {
          graph = S0;
          edges = graph.getOutgoingEdges("E");
        }
        else
        {
          graph = S1;
          edges = graph.getOutgoingEdges("I");
        }
        
        if (!edges->empty())
        {
          Edge edgeSelected = softmax_action_sel(graph, *edges);
          std::string turnSelected = edgeSelected.dest->node;
          int turnNb = graph.getNodeIndex(turnSelected);
          currNode = edgeSelected.dest;
          arma::vec durationVec = simulateTurnDuration(turnTimes, allpaths, turnNb, (turnIdx + 1), turnStages, nodeGroups, debug);
          double turnTime = durationVec(1);
          turnNames.push_back(turnSelected);
          episodeTurns.push_back(currNode->node);
          episodeTurnStates.push_back(S);
          episodeTurnTimes.push_back(0);
          turns_index.push_back(turnIdx);
          pathDuration = pathDuration + turnTime;
          
          generated_TurnsData_sess(turnIdx, 0) = turnNb;
          generated_TurnsData_sess(turnIdx, 1) = S;
          generated_TurnsData_sess(turnIdx, 2) = 0;
          generated_TurnsData_sess(turnIdx, 3) = turnTime;
          //Rcpp::Rcout << "Turn=" << currTurn <<", turn duration="<< generated_TurnsData_sess(turnIdx, 3)<<std::endl;
          generated_TurnsData_sess(turnIdx, 4) = sessId;
          generated_TurnsData_sess(turnIdx, 5) = actionNb;
          generated_TurnsData_sess(turnIdx, 6) = durationVec(0);
          turnIdx++;
          
          edges = graph.getOutgoingEdges(currNode->node);
        }
        else
        {
          int A;
          if (S == 0)
          {
            A = S0.getPathFromTurns(turnNames);
          }
          else
          {
            A = S1.getPathFromTurns(turnNames);
          }
          
          generated_PathData_sess(actionNb, 0) = A;
          generated_PathData_sess(actionNb, 1) = S;
          //Rcpp::Rcout <<"R(S, A)=" <<R(S, A)<< std::endl;
          generated_PathData_sess(actionNb, 2) = R(S, A);
          generated_PathData_sess(actionNb, 3) = pathDuration;
          generated_PathData_sess(actionNb, 4) = sessId;
          generated_PathData_sess(actionNb, 5) = actionNb;
          
          if (R(S, A) == 1)
          {
            turnReward = 1;
            generated_TurnsData_sess((turnIdx - 1), 2) = 1;
            score_episode = score_episode + 1;
          }
          
          actionNb++;
          
          last_turn = generated_TurnsData_sess((turnIdx - 1), 0);
          S_prime = aca_getNextState(S, A, last_turn);
          
          if (S_prime != initState)
          {
            changeState = true;
          }
          else if (S_prime == initState && changeState)
          {
            returnToInitState = true;
          }
        }
        
        if (prevNode)
        {
          double prediction = gamma * currNode->credit;
          double td_err = turnReward + prediction - prevNode->credit;
          prevNode->credit = prevNode->credit + (alpha * td_err);
          turnReward = 0;
        }
        
        graph.updateEdgeProbs();
        
        prevNode = currNode;
        S = S_prime;
      }
      
      //arma::mat durationMat = simulatePathTime(turnTimes, allpaths, actionNb, A, pathStages,nodeGroups);
      
      //Rcpp::Rcout <<"A=" << A << ", S=" << S << ", sessId=" <<sessId<< std::endl;
      
      if (returnToInitState)
      {
        //Rcpp::Rcout << "Inside end episode" << std::endl;
        changeState = false;
        returnToInitState = false;
        avg_score = (avg_score * (episode - 1) + (score_episode - avg_score)) / episode;
        //Rcpp::Rcout << "episodeTurns.size=" <<episodeTurns.size() << ", episodeTurnStates.size()=" <<episodeTurnStates.size() << ", episodeTurnTimes.size=" << episodeTurnTimes.size()<< std::endl;
        // for(unsigned int i=0; i<episodeTurns.size(); i++)
        // {
        //   std::cout << episodeTurns[i] << "," <<episodeTurnStates[i] << "," <<episodeTurnTimes[i] << "; ";
        // }
        
        //Rcpp::Rcout << "Here7" << std::endl;
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }
      
      //Rcpp::Rcout << "Here1" << std::endl;
      //Rcpp::Rcout << "Here2" << std::endl;
    }
    
    //Rcpp::Rcout << "Here3" << std::endl;
    
    // if (turnIdx < (nrow * 2) - 1)
    // {
    //   generated_TurnsData_sess.shed_rows((turnIdx), ((nrow * 2) - 1));
    // }
    //Rcpp::Rcout << "Here4" << std::endl;
    generated_TurnData = arma::join_cols(generated_TurnData, generated_TurnsData_sess.rows(0, (turnIdx - 1)));
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    //Rcpp::Rcout << "Here5" << std::endl;
    generated_PathData = arma::join_cols(generated_PathData, generated_PathData_sess);
    //Rcpp::Rcout << "Here6" << std::endl;
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (Rcpp::List::create(Named("PathData") = generated_PathData, _["TurnData"] = generated_TurnData));
}

std::vector<double> getQLearningLik(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  
  if(model == "Paths")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  }
  else if (model == "Turns")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("turnTimes"));
  }
  else if (model == "Hybrid1")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel1"));
  }
  else if (model == "Hybrid2")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel2"));
  }
  else if (model == "Hybrid3")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel3"));
  }
  else if (model == "Hybrid4")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel4"));
  }
  
  //Rcpp::List nodeGroups = Rcpp::as<Rcpp::List>(testModel.slot("nodeGroups"));
  
  double alpha = Rcpp::as<double>(modelData.slot("alpha"));
  double beta = Rcpp::as<double>(modelData.slot("gamma1"));
  //double gamma2 = Rcpp::as<double>(modelData.slot("gamma2"));
  
  //Rcpp::Rcout <<  "alpha="<<alpha << ", beta=" <<beta  <<std::endl;
  
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
  
  double averageReward = 0;
  double rewardSum = 0;
  double durationSum = 0;
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
    if(sim != 1)
    {
      states_sess = states_sess - 1;
      actions_sess = actions_sess -1;
    }

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
    bool endOfCurrEpisode = false;
    
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    Node *prevNode=nullptr;
    Node *currNode=nullptr;
    Node * rootNode=nullptr; 
    Graph * graph=nullptr;
    //int prevTurnReward = 0;
    int currTurnReward = 0;
    int i = 0;
    while(i < (nrow))
    {
      initState = states_sess(i);

      S = states_sess(i);
      A = actions_sess(i);
      //int S_prime = states_sess(i + 1);
        
      if (S == 0)
      {
        graph = &S0;
        rootNode = graph->getNode("E"); 
      }
      else
      {
        graph = &S1;
        rootNode = graph->getNode("I"); 
      }
      int R = rewards_sess(i);
      if(R==1)
      {
        R = 1;
      }

      Rcpp::StringVector turns;
      turns = graph->getTurnsFromPaths(A);
      int nbOfTurns = turns.length();

      double pathProb = 1;
      for (int j = 0; j < nbOfTurns; j++)
      {
        actionCounter = actionCounter+1;
        if (j == (nbOfTurns - 1))
        {
          currTurnReward = R;
        }
        else
        {
          currTurnReward = 0;
        }
        
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph->getNode(currTurn);
        double turntime = turn_times_session(session_turn_count);
        turntime = round(turntime/1000);

        Edge edge;
        std::vector<Edge> *siblings = nullptr;
        if(j==0)
        {
          edge = graph->getEdge(rootNode->node, currNode->node);
          siblings = graph->getOutgoingEdges(rootNode->node);
        }
        else
        {
          edge = graph->getEdge(prevNode->node, currNode->node);
          siblings = graph->getOutgoingEdges(prevNode->node);
        }
        
        double prob_a = edge.probability;
        pathProb = pathProb * prob_a;
        
        
        bool isCurrTurnGreedy = false;
        double maxProb = 0;
        Node* maxSibling = nullptr;
        for (auto it = siblings->begin(); it != siblings->end(); ++it)
        {
          double prob = it->probability;
          if((prob - maxProb) >= 1e-9)
          {
            maxSibling = it->dest;
          }

        }
        if(maxSibling->node == currNode->node)
        {
          //Rcpp::Rcout <<"Max Sibling is curr node, action is greedy" << std::endl;
          isCurrTurnGreedy = true;
        }

        
        double qMax = 0;
        std::vector<Edge> *edges = graph->getOutgoingEdges(currTurn);
        
        if(edges->size() > 0) //If curr turn is an intermediate turn in the maze, determine qmax using edges
        {
          //Rcpp::Rcout <<"Number of edges greater than zero for current turn " << currTurn << std::endl;
         Node* selectedNode = nullptr;
         for (auto it = edges->begin(); it != edges->end(); ++it)
         {

           double destNodeCredit = it->dest->credit;
           if((destNodeCredit - qMax) >= 1e-9)
           {
             qMax = destNodeCredit;
             selectedNode = it->dest;
           }

         }
         if(selectedNode == nullptr)
         {
           //Rcpp::Rcout <<"No edge is selected because all edges have 0 value " << std::endl;
           Edge selectedEdge =  edges->at(0);
           selectedNode = selectedEdge.dest;
         }
         qMax = selectedNode->credit;
         //Rcpp::Rcout <<"Edge with max value is: " << selectedNode->node << std::endl;
        }
        else if(j == (nbOfTurns - 1))  //If curr turn leads to next box, then select qmax using actions from next box
        {
          //Rcpp::Rcout <<"Final turn of the path" << std::endl;
          if(i != (nrow-1))
          {
            //Rcpp::Rcout <<"Not the final path of the session" << std::endl;
            int S_prime = states_sess(i + 1);
            Node * newRootNode;
            Graph * newGraph=nullptr;
            if (S_prime == 0)
            {
              //Rcpp::Rcout <<"Next state is box E" << std::endl;
              newGraph = &S0;
              newRootNode = newGraph->getNode("E"); 
            }
            else
            {
              //Rcpp::Rcout <<"Next state is box I" << std::endl;
              newGraph = &S1;
              newRootNode = newGraph->getNode("I"); 
            }
            std::vector<Edge> *edges = newGraph->getOutgoingEdges(newRootNode->node);

            Node* selectedNode=nullptr;
            for (auto it = edges->begin(); it != edges->end(); ++it)
            {
              double destNodeCredit = it->dest->credit;
              if((destNodeCredit - qMax) >= 1e-9)
              {
                qMax = destNodeCredit;
                selectedNode = it->dest;
              }
              
            }
            if(selectedNode == nullptr)
            {
              //Rcpp::Rcout <<"All actions is box "<< rootNode->node << " have value 0." << std::endl;
              // If all edges have same qval, select edge[0]
              Edge selectedEdge =  edges->at(0);
              selectedNode = selectedEdge.dest;
            }
            qMax = selectedNode->credit;
            //Rcpp::Rcout <<"Max value action in box "<< rootNode->node << " is: " << selectedNode->node << std::endl;
          }
        }
        
          
        double td_err = currTurnReward - (averageReward*turntime) + qMax - currNode->credit;
        //Rcpp::Rcout <<"currTurn="  << currTurn <<", currTurnReward=" << currTurnReward  << ", turntime=" <<turntime <<  ", averageReward=" <<averageReward << ", qMax=" <<  qMax << ", td_err=" <<td_err << std::endl;
        
        double alpha_prime = alpha/(double) std::pow(actionCounter,0);
        currNode->credit = currNode->credit + (alpha_prime * td_err);
        
	double beta_prime = beta/(double) std::pow(actionCounter,0);
        averageReward = averageReward + (beta_prime*td_err);

        //if(isCurrTurnGreedy)
        //{
        //  rewardSum = rewardSum + currTurnReward;
        //  durationSum = durationSum + turntime;
        //  averageReward = rewardSum/(double) durationSum;
        //}
        

        prevNode = currNode;
        //prevTurnReward = currTurnReward;
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
        
        //S0.printCredits(true);
        //S1.printCredits(true);
        
        session_turn_count++;
      }
      
      
      
      if(A != 6)
      {
        double logProb = log(pathProb);
        mseMatrix.push_back(logProb);
      }
      else{
        mseMatrix.push_back(0);
      }
      
      //S = S_prime;
      i++;
    }
  }
  
  return (mseMatrix);
}


arma::mat getQLearningProbMat(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  
  if(model == "Paths")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  }
  else if (model == "Turns")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("turnTimes"));
  }
  else if (model == "Hybrid1")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel1"));
  }
  else if (model == "Hybrid2")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel2"));
  }
  else if (model == "Hybrid3")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel3"));
  }
  else if (model == "Hybrid4")
  {
    turnTimes = Rcpp::as<arma::mat>(ratdata.slot("hybridModel4"));
  }
  

  double alpha = Rcpp::as<double>(modelData.slot("alpha"));
  double beta = Rcpp::as<double>(modelData.slot("gamma1"));
  //double gamma2 = Rcpp::as<double>(modelData.slot("gamma2"));
  

  arma::mat mseMatrix;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec allpaths_pathNb = allpaths.col(5);
  
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
  
  double averageReward = 0;
  double rewardSum = 0;
  double durationSum = 0;
  int actionCounter = 0;
  
  Graph S0(testModel, 0);
  Graph S1(testModel, 1);


  for (unsigned int session = 0; session < uniqSessIdx.n_elem; session++)
  {
    
    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout <<"sessId="<<sessId<<std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec allpaths_pathNb_sess = allpaths_pathNb.elem(sessionIdx);
    
    if(sim != 1)
    {
      states_sess = states_sess - 1;
      actions_sess = actions_sess -1;
    }

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
    bool endOfCurrEpisode = false;
    
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    Node *prevNode=nullptr;
    Node *currNode=nullptr;
    Node * rootNode=nullptr; 
    Graph * graph=nullptr;
    //int prevTurnReward = 0;
    int currTurnReward = 0;
    int i = 0;
    while(i < (nrow))
    {
      initState = states_sess(i);
      
      S = states_sess(i);
      A = actions_sess(i);
      //int S_prime = states_sess(i + 1);
      
      if (S == 0)
      {
        graph = &S0;
        rootNode = graph->getNode("E"); 
      }
      else
      {
        graph = &S1;
        rootNode = graph->getNode("I"); 
      }
      int R = rewards_sess(i);
      if(R==1)
      {
        R = 1;
      }
      //Rcpp::Rcout <<"ses=" << sessId << ", i=" << i << ", S=" <<S <<  ", A=" << A << ", R="<< R << std::endl;
      
      
      Rcpp::StringVector turns;
      turns = graph->getTurnsFromPaths(A);
      int nbOfTurns = turns.length();
      
      //Rcpp::Rcout <<"S=" <<S << ", R="<< R << ", nbOfTurns=" << nbOfTurns << std::endl;
      
      
      
      double pathProb = 1;
      for (int j = 0; j < nbOfTurns; j++)
      {
        actionCounter = actionCounter+1;
        if (j == (nbOfTurns - 1))
        {
          currTurnReward = R;
        }
        else
        {
          currTurnReward = 0;
        }
        
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph->getNode(currTurn);
        double turntime = turn_times_session(session_turn_count);
        turntime = round(turntime/1000);
        //Rcpp::Rcout <<"currTurn="<< currTurn << ", currTurnReward=" <<currTurnReward<<std::endl;
        
        Edge edge;
        std::vector<Edge> *siblings = nullptr;
        if(j==0)
        {
          edge = graph->getEdge(rootNode->node, currNode->node);
          siblings = graph->getOutgoingEdges(rootNode->node);
        }
        else
        {
          edge = graph->getEdge(prevNode->node, currNode->node);
          siblings = graph->getOutgoingEdges(prevNode->node);
        }
        

        
        bool isCurrTurnGreedy = false;
        double maxProb = 0;
        Node* maxSibling = nullptr;
        for (auto it = siblings->begin(); it != siblings->end(); ++it)
        {
          //Rcpp::Rcout <<"edge.src=" << it->src->node << ", edge.dest=" << it->dest->node << ",credit=" << it->dest->credit <<std::endl;
          
          double prob = it->probability;
          if((prob - maxProb) >= 1e-9)
          {
            maxSibling = it->dest;
          }
          
        }
        if(maxSibling->node == currNode->node)
        {
          //Rcpp::Rcout <<"Max Sibling is curr node, action is greedy" << std::endl;
          isCurrTurnGreedy = true;
        }
        
        
        double qMax = 0;
        std::vector<Edge> *edges = graph->getOutgoingEdges(currTurn);
        
        if(edges->size() > 0) //If curr turn is an intermediate turn in the maze, determine qmax using edges
        {
          //Rcpp::Rcout <<"Number of edges greater than zero for current turn " << currTurn << std::endl;
          Node* selectedNode = nullptr;
          for (auto it = edges->begin(); it != edges->end(); ++it)
          {
            //Rcpp::Rcout <<"edge.src=" << it->src->node << ", edge.dest=" << it->dest->node << ",credit=" << it->dest->credit <<std::endl;
            
            double destNodeCredit = it->dest->credit;
            if((destNodeCredit - qMax) >= 1e-9)
            {
              qMax = destNodeCredit;
              selectedNode = it->dest;
            }
            
          }
          if(selectedNode == nullptr)
          {
            //Rcpp::Rcout <<"No edge is selected because all edges have 0 value " << std::endl;
            Edge selectedEdge =  edges->at(0);
            selectedNode = selectedEdge.dest;
          }
          qMax = selectedNode->credit;
          //Rcpp::Rcout <<"Edge with max value is: " << selectedNode->node << std::endl;
        }
        else if(j == (nbOfTurns - 1))  //If curr turn leads to next box, then select qmax using actions from next box
        {
          //Rcpp::Rcout <<"Final turn of the path" << std::endl;
          if(i != (nrow-1))
          {
            //Rcpp::Rcout <<"Not the final path of the session" << std::endl;
            int S_prime = states_sess(i + 1);
            Node * newRootNode;
            Graph * newGraph=nullptr;
            if (S_prime == 0)
            {
              //Rcpp::Rcout <<"Next state is box E" << std::endl;
              newGraph = &S0;
              newRootNode = newGraph->getNode("E"); 
            }
            else
            {
              //Rcpp::Rcout <<"Next state is box I" << std::endl;
              newGraph = &S1;
              newRootNode = newGraph->getNode("I"); 
            }
            std::vector<Edge> *edges = newGraph->getOutgoingEdges(newRootNode->node);
            
            Node* selectedNode=nullptr;
            for (auto it = edges->begin(); it != edges->end(); ++it)
            {
              double destNodeCredit = it->dest->credit;
              if((destNodeCredit - qMax) >= 1e-9)
              {
                qMax = destNodeCredit;
                selectedNode = it->dest;
              }
              
            }
            if(selectedNode == nullptr)
            {
              //Rcpp::Rcout <<"All actions is box "<< rootNode->node << " have value 0." << std::endl;
              // If all edges have same qval, select edge[0]
              Edge selectedEdge =  edges->at(0);
              selectedNode = selectedEdge.dest;
            }
            qMax = selectedNode->credit;
            //Rcpp::Rcout <<"Max value action in box "<< rootNode->node << " is: " << selectedNode->node << std::endl;
          }
        }
        
        
        double td_err = currTurnReward - (averageReward*turntime) + qMax - currNode->credit;
        //Rcpp::Rcout <<"currTurn="  << currTurn <<", currTurnReward=" << currTurnReward  << ", turntime=" <<turntime <<  ", averageReward=" <<averageReward << ", qMax=" <<  qMax << ", td_err=" <<td_err << std::endl;
        
        double alpha_prime = alpha/(double) std::pow(actionCounter,0);
        currNode->credit = currNode->credit + (alpha_prime * td_err);
        double beta_prime = beta/(double) std::pow(actionCounter,0);
        averageReward = averageReward + (beta_prime*td_err);
        //if(isCurrTurnGreedy)
        //{
        //  rewardSum = rewardSum + currTurnReward;
         // durationSum = durationSum + turntime;
         // averageReward = rewardSum/(double) durationSum;
        //}
        
        //Rcpp::Rcout <<"turntime=" <<turntime <<  " ,currTurnReward=" << currTurnReward <<  ", averageReward=" <<averageReward << ", qMax=" <<  qMax << ", td_err=" <<td_err << std::endl;
        
        
        prevNode = currNode;
        //prevTurnReward = currTurnReward;
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
        
        //S0.printCredits(true);
        //S1.printCredits(true);
        
        session_turn_count++;
      }
      
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
      i++;
    }
  }
  
  return (mseMatrix);
}

#endif
