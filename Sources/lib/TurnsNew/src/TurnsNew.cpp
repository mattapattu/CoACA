#include "aca3.hpp"
#include "sarsa.hpp"
#include "aca2.hpp"
#include "aca4.hpp"
#include "avgRewardQLearning.hpp"


// [[Rcpp::export()]]
Rcpp::List simulateTurnsModels(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, Rcpp::S4 turnModel, arma::vec turnStages, bool debug = false)
{
    std::string creditAssignment = Rcpp::as<std::string>(modelData.slot("creditAssignment"));
    Rcpp::List ret;
    if(creditAssignment == "aca3")
    {
      ret =   simulateAca3TurnsModels(ratdata, modelData, testModel, turnStages, debug);
    }
    else if(creditAssignment == "aca2")
    {
        ret =   simulateAca2TurnsModels(ratdata, modelData, testModel, turnModel,turnStages, debug);
    }
    else if(creditAssignment == "sarsa")
    {

    }
    else if(creditAssignment == "qlearningAvgRwd")
    {
      ret =   simulateQLearn(ratdata, modelData, testModel, turnModel,turnStages, debug);
    }
    return(ret);
}

// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug = false)
{
    std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
    std::string creditAssignment = Rcpp::as<std::string>(modelData.slot("creditAssignment"));
    std::vector<double> ret;
    if(creditAssignment == "aca3")
    {
      ret =   getAca3Likelihood(ratdata, modelData, testModel, sim);
    }
    else if(creditAssignment == "aca2")
    {
        ret =   getAca2Likelihood(ratdata, modelData, testModel, sim);
    }
    else if(creditAssignment == "aca4")
    {
      ret =   getAca4Likelihood(ratdata, modelData, testModel, sim, debug);
    }
    else if(creditAssignment == "sarsa")
    {
        ret =   getSarsaLik(ratdata, modelData, testModel, sim);
    }
    else if(creditAssignment == "qlearningAvgRwd")
    {
      ret =   getQLearningLik(ratdata, modelData, testModel, sim, debug);
    }
    return(ret);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug = false)
{
    std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
    std::string creditAssignment = Rcpp::as<std::string>(modelData.slot("creditAssignment"));
    arma::mat ret;
    if(creditAssignment == "aca3")
    {
      ret =   getAca3ProbMatrix(ratdata, modelData, testModel, sim, debug);
    }
    else if(creditAssignment == "aca2")
    {
        ret =   getAca2ProbMatrix(ratdata, modelData, testModel, sim, debug);
    }
    else if(creditAssignment == "aca4")
    {
      ret =   getAca4ProbMatrix(ratdata, modelData, testModel, sim, debug);
    }
    else if(creditAssignment == "sarsa")
    {
        ret =   getSarsaProbMat(ratdata, modelData, testModel, sim);
    }
    else if(creditAssignment == "qlearningAvgRwd")
    {
      ret =   getQLearningProbMat(ratdata, modelData, testModel, sim, debug);
    }
    return(ret);
}


// [[Rcpp::export()]]
arma::mat getProbMatrix2(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug = false)
{
  std::string model = Rcpp::as<std::string>(modelData.slot("Model"));
  std::string creditAssignment = Rcpp::as<std::string>(modelData.slot("creditAssignment"));
  arma::mat ret;
  if(creditAssignment == "aca2")
  {
    ret =   getAca2ProbMatrix2(ratdata, modelData, testModel, sim, debug);
  }
  else if(creditAssignment == "qlearningAvgRwd")
  {
    ret =   getQLearningProbMat2(ratdata, modelData, testModel, sim, debug);
  }
  
  return(ret);
}
