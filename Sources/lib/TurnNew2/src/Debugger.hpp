#ifndef __DEBUGGER__
#define __DEBUGGER__


#include <iostream>
#include <vector>
#include <RcppArmadillo.h>

class Debugger
{
    private:
        bool m_debug;

    public:
        Debugger();
        void setDebug(bool debug);
        void Print(std::string message);
        void PrintArmaVec(std::string message, arma::vec vector);
        void PrintArmaMat(std::string message, arma::mat matrix);
        void PrintRcppVec(std::string message, Rcpp::StringVector vector);
        void PrintStdVec(std::string message, std::vector<std::string> vector);
        
};

Debugger::Debugger()
{
    m_debug = false;
}

void Debugger::setDebug(bool debug)
{
    m_debug = debug;
}

void Debugger::Print(std::string message)
{
    if(m_debug)
    {
        Rcpp::Rcout << message  << std::endl;
    }
}


void Debugger::PrintArmaVec(std::string message, arma::vec vector)
{
    if(m_debug)
    {
        Rcpp::Rcout << message  << vector << std::endl;
    }
}

void Debugger::PrintArmaMat(std::string message, arma::mat matrix)
{
    if(m_debug)
    {
        Rcpp::Rcout << message  << matrix << std::endl;
    }
}

void Debugger::PrintRcppVec(std::string message, Rcpp::StringVector vector)
{
    if(m_debug)
    {
        Rcpp::Rcout << message  << vector << std::endl;
    }
}


void Debugger::PrintStdVec(std::string message, std::vector<std::string> vector)
{
    if(m_debug)
    {
        for(unsigned int i=0; i<vector.size(); ++i)
            std::cout << vector[i] << ' ';
        std::cout << std::endl;
    }
}

#endif
