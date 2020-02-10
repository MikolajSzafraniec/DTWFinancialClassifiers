#include <math.h>
using namespace Rcpp;

/*
 * This class contains functions to calculate discrete trigonometric
 * transforms of time series: sine, cosine and Hilbert transform. Formulas
 * for calculating this transforms originate from the paper below:
 * http://www.math.uni.wroc.pl/~wisla2012/prezentacje/gorecki.pdf
 * 
 * There is possibility to use functions from package dtt as well,
 * but my tests suggest, that implementation below is quite faster.
 */

namespace TTR
{
  class TrigonometricTransforms{
    
  public:
    static NumericVector DCT(NumericVector timeSeries){
      double pi = M_PI;
      int n = timeSeries.size();
      NumericVector res(n);
      double partialSum;
      
      for(int k = 1; k <= n; k++){
        partialSum = 0;
        for(int i = 1; i <= n; i++){
          partialSum += timeSeries[i-1]*cos((pi/n)*(i-0.5)*(k-1)); 
        }
        
        res[k-1] = partialSum;
      }
      
      return res;
    }
    
    static NumericVector DST(NumericVector timeSeries){
      double pi = M_PI;
      int n = timeSeries.size();
      NumericVector res(n);
      double partialSum;
      
      for(int k = 1; k <= n; k++){
        partialSum = 0;
        for(int i = 1; i <= n; i++){
          partialSum += timeSeries[i-1]*sin((pi/n)*(i-0.5)*k); 
        }
        
        res[k-1] = partialSum;
      }
      
      return res;
    }
    
    static NumericVector DHT(NumericVector timeSeries){
      int n = timeSeries.size();
      NumericVector res(n);
      double partialSum;
      
      for(int k = 1; k <= n; k++){
        partialSum = 0;
        for(int i = 1; i <= n; i++){
          if(i != k){
            partialSum += timeSeries[i-1] / (k-i);
          }
        }
        
        res[k-1] = partialSum;
      }
      
      return res;
    }
    
  private:
    TrigonometricTransforms() {}
  };
}