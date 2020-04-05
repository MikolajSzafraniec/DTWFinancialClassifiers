#include <Rcpp.h>
#include "RcppDistances.h"
#ifndef MyEnums
#define MyEnums
#endif

using namespace Rcpp;
using namespace RcppDist;

namespace CppDTW{

  struct SimpleDTWResults{
    double Distance;
    IntegerVector WarpingPathP;
    IntegerVector WarpingPathQ;
  };

  NumericMatrix AccumulatedCostMatrix(NumericMatrix distMatrix){
    
    int n_col = distMatrix.ncol();
    int n_row = distMatrix.nrow();
    
    NumericMatrix res(n_row, n_col);
    
    res(0, 0) = distMatrix(0, 0);
    
    for(int i = 1; i < n_row; i++)
      res(i, 0) = distMatrix(i, 0) + res(i-1, 0);
    
    for(int j = 1; j < n_col; j++)
      res(0, j) = distMatrix(0, j) + res(0, j-1);
    
    for(int i = 1; i < n_row; i++){
      for(int j = 1; j < n_col; j++){
        res(i, j) = distMatrix(i, j) + std::min({
          res(i - 1, j - 1),
          res(i, j - 1),
          res(i - 1, j)
        });
      }
    }
    
    return res;
  }
  
  SimpleDTWResults DTWRcpp(NumericMatrix distMatrix){
    
    NumericMatrix accCostMatrix = AccumulatedCostMatrix(distMatrix);
    std::vector<IntegerVector> warpingPoints;
    
    int n_row = distMatrix.nrow();
    int n_col = distMatrix.ncol();
    
    int i = 0;
    int j = 0;
    
    warpingPoints.push_back({i+1, j+1});
    
    while(i < (n_row-1) && j < (n_col-1)){
      
      if(i == (n_row-1)){
        j++;
      }else if(j == (n_col-1)){
        i++;
      }else{
        
        double min_val = std::min({
          distMatrix(i + 1, j + 1),
          distMatrix(i + 1, j),
          distMatrix(j + 1, i)
        });
        
        if(distMatrix(i, j+1) == min_val){
          j++;
        }else if(distMatrix(i+1, j) == min_val){
          i++;
        }else{
          i++;
          j++;
        }
      }
      
      warpingPoints.push_back({i+1, j+1});
    }
    
    int warpingLength = warpingPoints.size();
    IntegerVector warpingPathP(warpingLength);
    IntegerVector warpingPathQ(warpingLength);
    
    for(int i = 0; i < warpingLength; i++){
      warpingPathP(i) = warpingPoints[i](0);
      warpingPathQ(i) = warpingPoints[i](1);
    }
    
    SimpleDTWResults res;
    
    res.Distance = accCostMatrix(n_row-1, n_col-1);
    res.WarpingPathP = warpingPathP;
    res.WarpingPathQ = warpingPathQ;
    
    return res;
  }
  
  
  SimpleDTWResults DTWRcppV2(NumericMatrix distMatrix){
    
    NumericMatrix accCostMatrix = AccumulatedCostMatrix(distMatrix);
    std::vector<IntegerVector> warpingPoints;
    
    int n_row = distMatrix.nrow();
    int n_col = distMatrix.ncol();
    
    int i = n_row - 1;
    int j = n_col - 1;
    
    warpingPoints.push_back({i+1, j+1});
    
    while((i > 0) | (j > 0)){
      
      if(i == 0){
        j--;
      }else if(j == 0){
        i--;
      }else{
        
        double min_val = std::min({
          accCostMatrix(i - 1, j - 1),
          accCostMatrix(i - 1, j),
          accCostMatrix(i, j - 1)
        });
        
        if(accCostMatrix(i-1, j-1) == min_val){
          i--;
          j--;
        }else if(accCostMatrix(i, j-1) == min_val){
          j--;
        }else{
          i--;
        }
      }
      
      warpingPoints.push_back({i+1, j+1});
    }
    
    int warpingLength = warpingPoints.size();
    IntegerVector warpingPathP(warpingLength);
    IntegerVector warpingPathQ(warpingLength);
    
    for(int i = 0; i < warpingLength; i++){
      warpingPathP(i) = warpingPoints[warpingLength - i - 1](0);
      warpingPathQ(i) = warpingPoints[warpingLength - i - 1](1);
    }
    
    SimpleDTWResults res;
    
    res.Distance = accCostMatrix(n_row-1, n_col-1);
    res.WarpingPathP = warpingPathP;
    res.WarpingPathQ = warpingPathQ;
    
    return res;
  }
}