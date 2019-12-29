#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

/* Klasa zawierająca funkcje pozwalające na obliczenie dyskretnych
 * transformat trygonometrycznych szeregów czasowych: kosinusowej, sinusowej
 * oraz Hilberta. Wzory zaczerpnięto z publikacji: 
 * http://www.math.uni.wroc.pl/~wisla2012/prezentacje/gorecki.pdf
 * 
 * W zastępstwie można by wykorzystać pakiet dtt autorstwa Łukasza Komsty,
 * ale moje testy wykazały, że poniższa implementacja jest nieco szybsza
*/ 

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