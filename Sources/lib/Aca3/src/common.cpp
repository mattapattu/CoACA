// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "acaCreditUpdate.hpp"
#include "utils.hpp"

using namespace Rcpp;

inline double softmax_cpp3(int A, int S, arma::mat H)
{
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m = arma::max(v);
  double exp_sum = std::exp(H(S, 0) - m) + std::exp(H(S, 1) - m) + std::exp(H(S, 2) - m) + std::exp(H(S, 3) - m) + std::exp(H(S, 4) - m) + std::exp(H(S, 5) - m);
  double pr_A = (std::exp(H(S, A) - m)) / exp_sum;

  //  float m=arma::max(v);
  //  v=exp(v-m);
  // // //Rcpp::Rcout << "m=" << m<< std::endl;
  //  double exp_sum  = arma::accu(v) ;
  //  v=v/exp_sum;
  //  double pr_A=v[A];

  if (pr_A < 0)
  {
    Rcpp::Rcout << "A=" << A << ", S=" << S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" << m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S, A) - m) << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    Rcpp::Rcout << v << std::endl;

    //stop("logProb is NAN");
  }
  else if (pr_A > 1)
  {
    Rcpp::Rcout << "pr_A=" << pr_A << " is > 1" << std::endl;
    Rcpp::Rcout << "A=" << A << ", S=" << S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" << m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S, A) - m) << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S, 0) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S, 1) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S, 2) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S, 3) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S, 4) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S, 5) - m) << std::endl;
  }
  if (R_IsNaN((pr_A)))
  {

    Rcpp::Rcout << "exp_sum=" << exp_sum << std::endl;
    Rcpp::Rcout << "numerator=" << (std::exp(H(S, A) - m)) << std::endl;
    Rcpp::Rcout << "pr_A=" << pr_A << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "A=" << A << ", S=" << S << ", m=" << m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S, A) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S, 0) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S, 1) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S, 2) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S, 3) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S, 4) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S, 5) - m) << std::endl;
    //stop("logProb is NAN");
  }

  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return (pr_A);
}


int aca_getNextState(int curr_state, int action)
{
  int new_state = -1;
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

int softmax_action_sel(arma::mat H, int S)
{

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  arma::rowvec v = H.row(S);
  double m = arma::max(v);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  v = exp(v - m);
  double exp_sum = arma::accu(v);
  v = v / exp_sum;
  IntegerVector actions = seq(0, 5);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, v.as_col())[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (action_selected);
}

// [[Rcpp::export()]]
Rcpp::List simulateTrials(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, arma::vec pathStages)
{

  arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  R(0, 3) = 1;
  R(1, 3) = 1;
  arma::mat generated_PathData;
  arma::mat generated_TurnData;
  
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;
  int score_episode = 0;
  int actionNb=0;

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);

    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int A = 0;
    int turnIdx = 0;
    //int A_prime=0;
    std::vector<int> episodeActions;
    std::vector<int> episodeStates;
    std::vector<double> episodePathTimes;


    int S = states_sess(0) - 1;
    arma::mat generated_PathData_sess(nrow, 6);
    generated_PathData_sess.fill(-1);
    arma::mat generated_TurnsData_sess((nrow * 3), 6);
    generated_TurnsData_sess.fill(-1);

    //All episodes in new session
    for (int act = 0; act < (nrow); act++)
    {
      actionNb++;
      // Rcpp::Rcout << "episode=" << i  << std::endl;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      A = softmax_action_sel(H,S);
      
      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;

      score_episode = score_episode + R(S, A);
      generated_PathData_sess(act, 0) = A;
      generated_PathData_sess(act, 1) = S;
      generated_PathData_sess(act, 2) = R(S, A);
      generated_PathData_sess(act, 3) = 0;
      arma::mat durationMat; 
      if (A == 6)
      {
        episodePathTimes.push_back(0);  
      }
      else
      {
          durationMat = simulatePathTime(turnTimes,allpaths,actionNb,A,pathStages);
          //Rcpp::Rcout << "durationMat=" << durationMat <<std::endl;
          double pathDuration = arma::accu(durationMat.col(1));
          generated_PathData_sess(act, 3) = pathDuration;
          episodePathTimes.push_back(pathDuration);  
      }
      generated_PathData_sess(act, 4) = sessId;
      generated_PathData_sess(act, 5) = actionNb;

      Rcpp::StringVector turns = getTurnsFromPaths(A, S);
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"Path="<< A << ", nbOfTurns=" << nbOfTurns<<std::endl;
      Rcpp::IntegerVector turns_index;
      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        int turnNb = getTurnIdx(currTurn, S);
        generated_TurnsData_sess(turnIdx, 0) = turnNb;
        generated_TurnsData_sess(turnIdx, 1) = S;
        generated_TurnsData_sess(turnIdx, 2) = 0;
        if (A != 6)
        {
          //arma::uvec ids = arma::find(durationMat.col(0) == turnNb);
          //Rcpp::Rcout << "durationMat(j,1)=" << durationMat(j,1) <<std::endl;
          generated_TurnsData_sess(turnIdx, 3) = durationMat(j,1);
        }
        else
        {
          generated_TurnsData_sess(turnIdx, 3) = 0;
        }
        generated_TurnsData_sess(turnIdx, 4) = sessId;
        generated_TurnsData_sess(turnIdx, 5) = actionNb;
        turns_index.push_back(turnIdx);
        turnIdx++;
      }

      if (R(S, A) == 1)
      {
        generated_TurnsData_sess((turnIdx - 1), 2) = 1;
      }

      episodeActions.push_back(A);
      episodeStates.push_back(S);
      
       
      int S_prime = aca_getNextState(S, A);

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      //Rcpp::Rcout << "logProb=" << logProb <<std::endl;
      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::vec actions = arma::conv_to<arma::vec>::from(episodeActions);
        arma::vec states = arma::conv_to<arma::vec>::from(episodeStates);
        arma::vec time_taken_for_trial = arma::conv_to<arma::vec>::from(episodePathTimes);

        episode = episode + 1;
        resetVector = true;

        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial, alpha, score_episode);

        score_episode = 0;
        episodeActions.clear();
        episodeStates.clear();
        episodePathTimes.clear();
      }

      H = gamma1 * H;
      S = S_prime;
      //trial=trial+1;
    }
    H = gamma2 * H;
    // if (turnIdx < (nrow * 2) - 1)
    // {
    //   generated_TurnsData_sess.shed_rows((turnIdx), ((nrow * 2) - 1));
    // }
    generated_TurnData = arma::join_cols(generated_TurnData, generated_TurnsData_sess.rows(0,(turnIdx-1)));
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    generated_PathData = arma::join_cols(generated_PathData, generated_PathData_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  
  return (Rcpp::List::create(Named("PathData") = generated_PathData, _["TurnData"] = generated_TurnData));
}



// [[Rcpp::export()]]
arma::vec getPathLikelihood(arma::mat allpaths, double alpha, double gamma1, double gamma2, int sim)
{
  arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::vec likelihoodVec;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;
  int score_episode = 0;
  float avg_score = 0;

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int A = 0;
    //int A_prime=0;
    

    int S = 0;
    if (sim == 1)
    {
      S = states_sess(0);
    }
    else
    {
      S = states_sess(0) - 1;
    }

    arma::vec likelihoodVec_sess(nrow);
    likelihoodVec_sess.fill(0);
    arma::vec episodeVec(nrow);
    //All episodes in new session
    for (int i = 0; i < nrow; i++)
    {

      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
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
      if (i < (nrow - 1))
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
      episodeVec(i) = episode;

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      double prob_a = 0;
      double logProb = 0;
      if(A != 6)
      {
        prob_a = softmax_cpp3(A, S, H);
        logProb = log(prob_a);
        likelihoodVec_sess(i) = logProb;
        
      }

      //Rcpp::Rcout << "logProb=" << logProb <<std::endl;
      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::uvec episodeIdx = arma::find(episodeVec == episode);
        arma::vec actions = actions_sess.elem(episodeIdx);
        arma::vec states = states_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial = time_taken_for_trial_sess.elem(episodeIdx);

        if (sim != 1)
        {
          actions = actions - 1;
          states = states - 1;
        }

        episode = episode + 1;
        resetVector = true;
        avg_score = (avg_score * (episode - 1) + (score_episode - avg_score)) / episode;

        // Rcpp::Rcout << "episodeIdx=" << episodeIdx<<std::endl;
        // Rcpp::Rcout << "actions=" << actions<<std::endl;
        // Rcpp::Rcout << "states=" << states<<std::endl;
        // Rcpp::Rcout << "time_taken_for_trial=" << time_taken_for_trial<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial, alpha, score_episode);
        //Rcpp::Rcout <<  "H after episode=" << H << std::endl;

        //H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score,model);
        score_episode = 0;
      }

      H = gamma1 * H;
      S = S_prime;
      //trial=trial+1;
    }

    H = gamma2 * H;
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    likelihoodVec = arma::join_cols(likelihoodVec, likelihoodVec_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  Rcpp::Rcout <<  "H =" << H<<std::endl;
  return (likelihoodVec);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, double alpha, double gamma1, double gamma2, int sim)
{
  arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::mat probMatrix_aca;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec pathIdx = allpaths.col(5);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;
  int score_episode = 0;
  float avg_score = 0;


  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);
    arma::vec pathIdx_sess = pathIdx.elem(sessionIdx);

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    //int A = 0;
    //int A_prime=0;

    int S = 0;
    if (sim == 1)
    {
      S = states_sess(0);
    }
    else
    {
      S = states_sess(0) - 1;
    }

    arma::mat probMatrix_sess((nrow - 1), 13);
    arma::vec episodeVec(nrow - 1);

    //All episodes in new session
    for (int i = 0; i < (nrow - 1); i++)
    {

      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
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

      int S_prime = 0;
      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
      }

      episodeVec(i) = episode;

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      probMatrix_sess(i,12) = pathIdx_sess(i);
      if (S == 0)
      {
        probMatrix_sess.submat(i, 6, i, 11) = arma::zeros(1, 6);
        for (int act = 0; act < 6; act++)
        {
          double x = 0;
         x = softmax_cpp3(act, 0, H);
          probMatrix_sess(i, act) = x;
          //Rcpp::Rcout <<"S=0" << ", i="<< i << ", x=" << x << std::endl;
        }
      }
      else if (S == 1)
      {
        //Rcpp::Rcout << "i=" <<i<< std::endl;
        probMatrix_sess.submat(i, 0, i, 5) = arma::zeros(1, 6);
        //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
        for (int act = 0; act < 6; act++)
        {
          double x = 0;
          x = softmax_cpp3(act, 1, H);
          probMatrix_sess(i, (6 + act)) = x;
          //Rcpp::Rcout << "S=1" << ", i="<< i << ", x=" << x << std::endl;
        }
      }

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::uvec episodeIdx = arma::find(episodeVec == episode);
        arma::vec actions = actions_sess.elem(episodeIdx);
        arma::vec states = states_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial = time_taken_for_trial_sess.elem(episodeIdx);

        if (sim != 1)
        {
          actions = actions - 1;
          states = states - 1;
        }

        episode = episode + 1;
        resetVector = true;
        avg_score = (avg_score * (episode - 1) + (score_episode - avg_score)) / episode;

        //Rcpp::Rcout << "i=" <<i<< ", episode="<<episode<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial, alpha, score_episode);
        //H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, model);
        score_episode = 0;
        H = gamma1 * H;
      }

      S = S_prime;
      //trial=trial+1;
    }

    H = gamma2 * H;
    probMatrix_aca = arma::join_cols(probMatrix_aca, probMatrix_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (probMatrix_aca);
}