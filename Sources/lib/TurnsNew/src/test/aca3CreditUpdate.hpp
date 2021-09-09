#ifndef __ACACREDITUPDATE__
#define __ACACREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"

//updateHMat(H,actions, states, trialTimes, alpha,N, score_episode, avg_score, model);
inline void Aca3CreditUpdate(std::vector<std::string> episodeTurns, std::vector<int> episodeTurnStates, std::vector<double> episodeTurnTimes, double alpha, float score_episode, Graph* S0, Graph* S1)
{

  arma::vec episodeTurnStates_arma = arma::conv_to<arma::vec>::from(episodeTurnStates);
  //Rcpp::Rcout <<  "episodeTurns.size=" << episodeTurns.size() <<std::endl;

  arma::vec episodeTurnTimes_arma(episodeTurnTimes);
  for (int state = 0; state < 2; state++)
  {
    //get turns in state 0/1
    std::set<std::string> turns;
    //std::vector<double> turnTimes;

    //identify unique turns corresponding to each state
    for (unsigned int index = 0; index < episodeTurnStates.size(); ++index)
    {
      if (episodeTurnStates[index] == state)
      {
        turns.insert(episodeTurns[index]);
        //turnTimes.push_back(episodeTurnTimes[index]);
      }
    }
    
    std::ostringstream stream;
    std::copy(turns.begin(), turns.end(), std::ostream_iterator<std::string>(stream, ","));
    std::string result = stream.str();
    //Rcpp::Rcout << "curr_state=" <<state << ", turns= " <<result <<std::endl;

    //turns - contains all unique turns in an episode corresponding to one state
    //Next: Loop through turns
    //Next: get the pointer of curr_turn from episodeTurns
    //Next: update credit of curr_turn in the tree
    //for each unique turn - get all instances of that in current episode
    
    for (auto curr_turn = std::begin(turns); curr_turn != std::end(turns); ++curr_turn)
    {
      Rcpp::IntegerVector turnIdx;
      //Rcpp::Rcout << "curr_turn=" <<*curr_turn << " in state=" << state<<std::endl;
       // to store the indices of all instances of curr_turn
      unsigned int turnIndex = 0;
      Node* currNode;
      

      for (auto node = std::begin(episodeTurns); node != std::end(episodeTurns); ++node)
      {
        //Rcpp::Rcout << "turnIndex=" <<turnIndex <<", state =" <<episodeTurnStates[turnIndex] <<  ", node->turn="<< (*node)->turn  <<std::endl;
        if (episodeTurnStates[turnIndex] == state)
        {
          //Get all instance idx of curr_turn in this episode
          if (*node == *curr_turn)
          {
            //Rcpp::Rcout <<  "Turn="<< *curr_turn <<", state=" <<state << " is found in episodeTurns at index=" <<turnIndex <<std::endl;
            turnIdx.push_back(turnIndex);
            //currNode = episodeTurns[turnIndex];
            if(state == 0)
            {
              currNode = S0->getNode(*curr_turn);
            }
            else
            {
              currNode = S1->getNode(*curr_turn);
            }
          }
        }
        turnIndex++;
      }

      if (!currNode)
      {
        Rcpp::Rcout <<"state=" <<state <<  ", turn="<< *curr_turn  << " not found in episodeTurns" <<std::endl;
      }
      arma::uvec ids = Rcpp::as<arma::uvec>(turnIdx);
      //Rcpp::Rcout <<  "turnIdx="<< arma::conv_to<arma::rowvec>::from(ids) <<std::endl;
      double turnTime = arma::accu(episodeTurnTimes_arma.elem(ids));
      double episodeDuration = arma::accu(episodeTurnTimes_arma);
      //Rcpp::Rcout <<  "turnTime="<< turnTime << ", episodeDuration=" << episodeDuration<<std::endl;
      double activity = 0;
      if(episodeDuration != 0)
      {
        activity = turnTime / arma::accu(episodeTurnTimes_arma);
      }
      double nodeCredit = currNode->credit;
      nodeCredit = nodeCredit + (alpha * score_episode * activity);
      //Rcpp::Rcout <<  "Turn="<< currNode->node  <<", state=" <<state  << ", activity=" <<activity << ", credit=" << nodeCredit <<std::endl;
      if(state == 0)
      {
        S0->updateNodeCredit(currNode->node,nodeCredit);
      }
      else
      {
        S1->updateNodeCredit(currNode->node,nodeCredit);
      }
      //double partialCredit = score_episode * activity;
      Rcpp::Rcout <<  "Turn="<< currNode->node  <<", state=" <<state  << ", credit=" << currNode->credit <<std::endl;
      
    }
  }
}
#endif
