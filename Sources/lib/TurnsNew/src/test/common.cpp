// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "aca3CreditUpdate.hpp"
#include "utils.hpp"
#include "tree.hpp"
#include "Debugger.hpp"


using namespace Rcpp;

//Function simulateTurnTimeFromR = Environment::global_env()["simulateTurnTime"];

int aca_getNextState(int curr_state, int action, int last_turn)
{
  //Rcpp::Rcout << "curr_state=" << curr_state << ", action=" << action << ", last_turn=" << last_turn << std::endl;
  int new_state = -1;
  if (action == 4 || action == 5)
  {
    new_state = curr_state;
  }
  else if (action == 6)
  {
    if (last_turn == 4 || last_turn == 7 || last_turn == 12 || last_turn == 15)
    {
      new_state = 1;
    }
    else if (last_turn == 5 || last_turn == 6 || last_turn == 13 || last_turn == 14)
    {
      new_state = 0;
    }
  }
  else if (curr_state == 0)
  {
    new_state = 1;
  }
  else if (curr_state == 1)
  {
    new_state = 0;
  }

  //Rcpp::Rcout << "new_state=" << new_state << std::endl;

  return (new_state);
}

Edge softmax_action_sel(Graph graph, std::vector<Edge> edges, bool debug=FALSE)
{
  
  Debugger logger;
  logger.setDebug(debug);
  std::ostringstream msg; 

  std::vector<double> probVec;
  for (auto edge = edges.begin(); edge != edges.end(); edge++)
  {
    probVec.push_back((*edge).probability);
  }
  arma::vec probVec_arma(probVec);
  //Rcpp::Rcout <<"creditVec_arma="<<creditVec_arma<<std::endl;

  IntegerVector actions = seq(0, (edges.size() - 1));
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, probVec_arma)[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (edges[action_selected]);
}

// [[Rcpp::export()]]
Rcpp::List simulateTurnsModels(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, arma::vec turnStages, bool debug = false)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  
  Debugger logger;
  logger.setDebug(debug);
  std::ostringstream msg; 
  arma::mat turnTimes;

  if(model == "Turns")
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
  Rcpp::List nodeGroups = Rcpp::as<Rcpp::List>(testModel.slot("nodeGroups"));

  double alpha = Rcpp::as<double>(modelData.slot("alpha"));
  double gamma1 = Rcpp::as<double>(modelData.slot("gamma1"));
  double gamma2 = Rcpp::as<double>(modelData.slot("gamma2"));
   
  
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
    //double avg_score = 0;
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
    for (int i = 0; i < nrow; i++)
    {
      actionNb++;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      msg.str("");
      msg << "i=" <<i <<", S=" <<S;
      //logger.Print(msg.str());  
      //Rcpp::Rcout <<"i=" <<i <<", S=" <<S  << std::endl;
      Node *currNode;
      std::vector<Edge> *edges;
      Rcpp::StringVector turnNames;
      Graph graph;
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

      Rcpp::IntegerVector turns_index;
      double pathDuration = 0;
      while (!edges->empty())
      {
        Edge edgeSelected = softmax_action_sel(graph, *edges,debug);
        std::string turnSelected = edgeSelected.dest->node;
        msg.str("");
        msg << "Turn=" << turnSelected;
        //logger.Print(msg.str()); 
        int turnNb = graph.getNodeIndex(turnSelected);
        msg.str("");
        msg << "turnNb=" << turnNb;
        //logger.Print(msg.str()); 
        currNode = edgeSelected.dest;
        msg.str("");
        msg << "currNode=" << currNode->node;
        //logger.Print(msg.str()); 
        currNode = edgeSelected.dest;
        arma::vec durationVec = simulateTurnDuration(turnTimes, allpaths, turnNb, (turnIdx+1), turnStages,nodeGroups,debug);
        msg.str("");
        //msg << "Turn=" << currNode->node << ", durationVec=";
        //logger.PrintArmaVec(msg.str(),durationVec); 
        double turnTime = durationVec(1);

        turnNames.push_back(turnSelected);
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turnTime);
        turns_index.push_back(turnIdx);
        pathDuration = pathDuration+ turnTime;

        generated_TurnsData_sess(turnIdx, 0) = turnNb;
        generated_TurnsData_sess(turnIdx, 1) = S;
        generated_TurnsData_sess(turnIdx, 2) = 0;
        generated_TurnsData_sess(turnIdx, 3) = turnTime;
        //Rcpp::Rcout << "Turn=" << currTurn <<", turn duration="<< generated_TurnsData_sess(turnIdx, 3)<<std::endl;
        generated_TurnsData_sess(turnIdx, 4) = sessId;
        generated_TurnsData_sess(turnIdx, 5) = actionNb;
        generated_TurnsData_sess(turnIdx, 6) = durationVec(0);
        turnIdx++;
        msg.str("");
        msg << "turnIdx=" << turnIdx;
        //logger.Print(msg.str());

        edges = graph.getOutgoingEdges(currNode->node);
        msg.str("");
        msg << "edges->size()=" << edges->size();
        //logger.Print(msg.str());
      }

      int A;
      if(S == 0)
      {
        A = S0.getPathFromTurns(turnNames);
      }
      else
      {
       A = S1.getPathFromTurns(turnNames); 
      }

      //arma::mat durationMat = simulatePathTime(turnTimes, allpaths, actionNb, A, pathStages,nodeGroups);
      msg.str("");
      msg << "A=" << A << ", S=" <<S ;
      //logger.Print(msg.str()); 
      //Rcpp::Rcout <<"A=" << A << ", S=" << S << ", sessId=" <<sessId<< std::endl;
      generated_PathData_sess(i, 0) = A;
      generated_PathData_sess(i, 1) = S;
      //Rcpp::Rcout <<"R(S, A)=" <<R(S, A)<< std::endl;
      generated_PathData_sess(i, 2) = R(S, A);
      generated_PathData_sess(i, 3) = pathDuration;
      generated_PathData_sess(i, 4) = sessId;
      generated_PathData_sess(i, 5) = actionNb;


      if (R(S, A) == 1)
      {
        //Rcpp::Rcout << "turnNb=" << generated_TurnsData_sess((turnIdx - 1), 0) << ", receives reward"<< std::endl;
        generated_TurnsData_sess((turnIdx - 1), 2) = 1;
        score_episode = score_episode + 1;
      }

      int last_turn = generated_TurnsData_sess((turnIdx - 1), 0);

      int S_prime = aca_getNextState(S, A, last_turn);
      
      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      if (returnToInitState)
      {
        //Rcpp::Rcout << "Inside end episode" << std::endl;
        msg.str("");
        msg << "Inside end episode" <<S ;
        //logger.Print(msg.str()); 

        msg.str("");
        msg << "i=" <<i << ", score_episode=" <<score_episode ;
        //logger.Print(msg.str()); 

        msg.str("");
        msg << "Actions=" ;
        //logger.PrintStdVec(msg.str(),episodeTurns); 

        changeState = false;
        returnToInitState = false;

        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, &S0, &S1);
        //Rcpp::Rcout << "S0 credits: ";
        //S0.printCredits(debug);
        //Rcpp::Rcout << std::endl;
        //Rcpp::Rcout << "S1 credits: ";
        //Rcpp::Rcout << std::endl;
        //S1.printCredits(debug);

        msg.str("");
        msg << "Here7" <<S ;
        //logger.Print(msg.str()); 
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }

      //Rcpp::Rcout << "Here1" << std::endl;
      msg.str("");
       msg << "Here1" <<S ;
      //logger.Print(msg.str()); 
      S0.decayCredits(gamma1);
      S1.decayCredits(gamma1);
      msg.str("");
      msg << "Here2" <<S ;
      //logger.Print(msg.str()); 

      S = S_prime;
    }

    msg.str("");
    msg << "Here3" <<S ;
    //logger.Print(msg.str()); 
    S0.decayCredits(gamma2);
    S1.decayCredits(gamma2);

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

// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  if(model == "Turns")
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
  double gamma1 = Rcpp::as<double>(modelData.slot("gamma1"));
  double gamma2 = Rcpp::as<double>(modelData.slot("gamma2"));


 //Rcpp::Rcout <<  "alpha="<<alpha <<std::endl;

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
  else
  {
    turnTime_method = turnTimes.col(5);
  }

  int episode = 1;

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

    arma::uvec turnTimes_idx = arma::find(turnTimes.col(4) == sessId);
    arma::vec turn_times_session = turnTime_method.elem(turnTimes_idx);
    arma::uword session_turn_count = 0;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode = 0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;

    for (int i = 0; i < (nrow - 1); i++)
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
        score_episode = score_episode + 1;
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
      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
      }

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      Rcpp::Rcout <<"i="<< i << ", S=" << S <<", A=" << A<<std::endl;

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
      Rcpp::Rcout <<"turns="<< turns << std::endl;

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
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph.getNode(currTurn);
        //currNode->credit = currNode->credit + 1; //Test
        //Rcpp::Rcout <<"currNode="<< currNode->node<<std::endl;
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turn_times_session(session_turn_count));

        Edge edge = graph.getEdge(prevNode->node, currNode->node);
        double prob_a = edge.probability;
                //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
        pathProb = pathProb* prob_a;      
        
        session_turn_count++;
        prevNode = currNode;
      }
      double logProb = log(pathProb);
      mseMatrix.push_back(logProb);

      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        avg_score = avg_score + (score_episode - avg_score) / episode;
        //Rcpp::Rcout <<  "score_episode=" << score_episode<<std::endl;
        // for (unsigned int index = 0; index < episodeTurns.size(); ++index)
        // {
        //   Rcpp::Rcout << "index=" <<index << ", episodeTurns[index]= " <<episodeTurns[index]->node <<std::endl;
        // }
        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, &S0, &S1);
        S0.updateEdgeProbs();
        //Rcpp::Rcout << "Update S1 probabilities"<<std::endl;
        S1.updateEdgeProbs();
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
      }
      S0.decayCredits(gamma1);
      S1.decayCredits(gamma1);
      S = S_prime;
      //trial=trial+1;
    }
    S0.decayCredits(gamma2);
    S1.decayCredits(gamma2);
  }

  return (mseMatrix);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug = false)
{
  arma::mat allpaths = Rcpp::as<arma::mat>(ratdata.slot("allpaths"));
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  arma::mat turnTimes;
  if(model == "Turns")
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
  double gamma1 = Rcpp::as<double>(modelData.slot("gamma1"));
  double gamma2 = Rcpp::as<double>(modelData.slot("gamma2"));
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::mat mseMatrix;
  //int mseRowIdx = 0;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec allpaths_pathNb = allpaths.col(5);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec pathIds;
  if (sim == 1)
  {
    pathIds = turnTimes.col(5);
  }
  else
  {
    pathIds = turnTimes.col(0);
  }

  arma::vec turnTime_method;
  if (sim == 1)
  {
    turnTime_method = turnTimes.col(3);
  }
  else
  {
    turnTime_method = turnTimes.col(5);
  }
  int episode = 1;

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

    arma::uvec turnTimes_idx = arma::find(turnTimes.col(4) == sessId);
    arma::vec turn_times_session = turnTime_method.elem(turnTimes_idx);
    
    arma::uword session_turn_count = 0;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode = 0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;
    //Rcpp::Rcout <<"nrow="<<nrow<<std::endl;
    for (int i = 0; i < (nrow - 1); i++)
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
        score_episode = score_episode + 1;
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
      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
      }

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      
      //Rcpp::Rcout <<"i="<< i << ", S=" << S <<", A=" << A<<std::endl;
       std::ostringstream msg; 
       msg << "i="<< i << ", S=" << S <<", A=" << A;
       logger.Print(msg.str()); 

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
      logger.PrintRcppVec(msg.str(),turns); 

      Node *currNode;
      Graph graph;
      if (S == 0)
      {
        graph = S0;
      }
      else
      {
        graph = S1;
      }

      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph.getNode(currTurn);
        //Edge edge = graph.getEdge(prevNode->node, currNode->node);
        int turnId = graph.getNodeIndex(currTurn);
        msg.str("");
        msg << "currTurn=" << currTurn << ", turnId=" << turnId <<", session_turn_count=" <<session_turn_count;
        logger.Print(msg.str()); 

        double turntime = turn_times_session(session_turn_count);
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turntime);

        msg  << ", turntime=" << turntime;
        logger.Print(msg.str()); 
        
        session_turn_count++;
      }
      //Rcpp::Rcout << "Here5" << std::endl;
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
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        msg.str("");
        msg <<"Inside end episode";
        logger.Print(msg.str()); 
        changeState = false;
        returnToInitState = false;

        avg_score = avg_score + (score_episode - avg_score) / episode;

        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, &S0, &S1);
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
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
      
      S0.decayCredits(gamma1);
      S1.decayCredits(gamma1);
      S = S_prime;
      
      // msg.str("");
      // msg <<"Here2";
      // logger.Print(msg.str());
      //trial=trial+1;
    }
    S0.decayCredits(gamma2);
    S1.decayCredits(gamma2);
    //Rcpp::Rcout <<  "Here3"<<std::endl;
  }

  return (mseMatrix);
}
