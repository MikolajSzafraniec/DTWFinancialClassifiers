#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

//Klasa zawierająca metody przekształcające sekwencję w jej shapeDescriptor

class ShapeDescriptors{
public:
  
  static NumericVector RawSubsequence(NumericVector subsequence){
    return subsequence;
  } 
    
  static NumericVector PAADescriptor(NumericVector subsequence, int PAAWindow){
    int subLength = subsequence.length();
    
    if(subLength < PAAWindow)
      stop("Szerokosc okna nie moze byc wieksza od dlugosci podsekwencji");
    
    int lastIndex = subLength - PAAWindow;
    NumericVector res(lastIndex + 1);
    
    for(int i = 0; i <= lastIndex; i++){
      Rcpp::Range currentRange = seq(i, i+PAAWindow-1);
      res[i] = mean(subsequence[currentRange]);
    }
    
    return res;
  }
  
  static NumericVector derivativeDescriptor(NumericVector subsequence){
    int subLenght = subsequence.length();
    NumericVector res(subLenght);
    int lastInd = subLenght - 1;
    
    for(int i = 1; i < lastInd; i++){
      res[i] = discreteDerivative(subsequence, i);
    }
    
    res[0] = res[1];
    res[subLenght -1] = res[subLenght - 2];
    
    return res;
  }
  
  static NumericVector slopeDescriptor(NumericVector subsequence, int slopeWindow){
    int subLength = subsequence.length();
    
    if(subLength < slopeWindow)
      stop("Szerokosc okna nie moze byc wieksza od dlugosci podsekwencji");
    
    int lastIndex = subLength - slopeWindow;
    NumericVector res(lastIndex + 1);
    
    NumericVector regresors(slopeWindow);
    for(int i = 0; i < slopeWindow; i++)
      regresors[i] = i+1;
    
    for(int i = 0; i <= lastIndex; i++){
      Rcpp::Range currentRange = seq(i, i+slopeWindow-1);
      res[i] = rcppSlope(regresors, subsequence[currentRange]);
    }
    
    return res;
  }
  
private:
  static double discreteDerivative(NumericVector subsequence, int i){
    double res = ((subsequence[i] - subsequence[i-1]) + ((subsequence[i+1] - subsequence[i-1])/2))/2;
    return res;
  }
  
  static double rcppSlope(NumericVector x, NumericVector y){
    int x_len = x.length();
    int y_len = y.length();
    
    if(x_len != y_len)
      stop("Blad: wektory o roznej dlugosci");
    
    double x_mean = mean(x);
    double y_mean = mean(y);
    
    double covariance = 0;
    double x_variance = 0;
    
    for(int i = 0; i < x_len; i++){
      covariance += (x[i] - x_mean)*(y[i] - y_mean);
      x_variance += pow((x[i] - x_mean), 2);
    }
    
    double res = covariance / x_variance;
    return res;
  }
  
  ShapeDescriptors() {}
};