#ifndef __TURNSNEW__
#define __TURNSNEW__


#include "utils.hpp"


using namespace Rcpp;


Rcpp::List simulateTurnsModels(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, arma::vec turnStages, bool debug = false);
std::vector<double> getTurnsLikelihood(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim);
arma::mat getProbMatrix(Rcpp::S4 ratdata, Rcpp::S4 modelData, Rcpp::S4 testModel, int sim, bool debug = false);

#endif