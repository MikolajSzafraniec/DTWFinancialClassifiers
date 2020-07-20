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

  struct DTWResults{
    double RawSeriesDistance;
    double ShapeDescriptorsDistance;
    std::vector<IntegerVector> WarpingPathsP;
    std::vector<IntegerVector> WarpingPathsQ;
  };
  
  void CopyDTWResults(DTWResults *r1, DTWResults *r2){
    
    int warpPathLength = r2->WarpingPathsP.size();
    
    r1->RawSeriesDistance = r2->RawSeriesDistance;
    r1->ShapeDescriptorsDistance = r2->ShapeDescriptorsDistance;
    r1->WarpingPathsP = std::vector<IntegerVector>();
    r1->WarpingPathsQ = std::vector<IntegerVector>();
    
    for(int i = 0; i < warpPathLength; i++){
      r1->WarpingPathsP.push_back(r2->WarpingPathsP[i]);
      r1->WarpingPathsQ.push_back(r2->WarpingPathsQ[i]);
    }
  }

<<<<<<< HEAD
  NumericMatrix AccumulatedCostMatrix(NumericMatrix distMatrix){
=======
  NumericMatrix AccumulatedCostMatrix(NumericMatrix distMatrix, 
                                      Rcpp::Nullable<int> sakoeChibaWindow = R_NilValue){
>>>>>>> sakoe_chiba_window
    
    int n_col = distMatrix.ncol();
    int n_row = distMatrix.nrow();
    
    NumericMatrix res(n_row, n_col);
    
<<<<<<< HEAD
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
=======
    if(sakoeChibaWindow == R_NilValue){
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
    }else{
      
      for(int i = 0; i < n_row; i++){
        for(int j = 0; j < n_col; j++){
          res(i, j) = R_PosInf;
        }
      }
      
      int sakoeChibaWindowInt = Rcpp::as<int>(sakoeChibaWindow);
      //sakoeChibaWindowInt = std::max(sakoeChibaWindowInt, std::abs(n_row - n_col));
      if(sakoeChibaWindowInt < 0)
        stop("Sakoe-chiba window must be positive value or zero");
        
      res(0, 0) = distMatrix(0, 0);
      
      for(int i = 1; i < std::min(n_row, sakoeChibaWindowInt+1); i++)
        res(i, 0) = distMatrix(i, 0) + res(i-1, 0);
      
      for(int j = 1; j < std::min(n_col, sakoeChibaWindowInt+1); j++)
        res(0, j) = distMatrix(0, j) + res(0, j-1);
      
      for(int i = 1; i < n_row; i++){
        for(int j = std::max(1, i-sakoeChibaWindowInt); j < std::min(n_col, i+sakoeChibaWindowInt+1); j++){
          res(i, j) = 0;
        }
      }
      
      for(int i = 1; i < n_row; i++){
        for(int j = std::max(1, i-sakoeChibaWindowInt); j < std::min(n_col, i+sakoeChibaWindowInt+1); j++){
          res(i, j) = distMatrix(i, j) + std::min({
            res(i - 1, j - 1),
            res(i, j - 1),
            res(i - 1, j)
          });
        }
>>>>>>> sakoe_chiba_window
      }
    }
    
    return res;
  }
  
  
<<<<<<< HEAD
  SimpleDTWResults DTWRcpp(NumericMatrix distMatrix){
    
    NumericMatrix accCostMatrix = AccumulatedCostMatrix(distMatrix);
=======
  SimpleDTWResults DTWRcpp(NumericMatrix distMatrix,
                           Rcpp::Nullable<int> sakoeChibaWindow = R_NilValue){
    
    NumericMatrix accCostMatrix = AccumulatedCostMatrix(distMatrix, sakoeChibaWindow);
>>>>>>> sakoe_chiba_window
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
  
  double CalcDistanceFromWarpingPaths(NumericMatrix distMatrix,
                                      IntegerVector warpingPathP,
                                      IntegerVector warpingPathQ){
    
    int wpPLen = warpingPathP.size();
    int wpQLen = warpingPathQ.size();
    
    if(wpPLen != wpQLen)
      stop("Lengths of warping paths must match");
    
    NumericVector distances(wpPLen);
    
    for(int i = 0; i < wpPLen; i++){
      distances(i) = distMatrix(warpingPathP(i)-1, warpingPathQ(i)-1);
    }
    
    double res = Rcpp::sum(distances);
    
    return res;
  }
  
<<<<<<< HEAD
  DTWResults ComplexDTWRcpp(DistMatrices inputDistances){
=======
  DTWResults ComplexDTWRcpp(DistMatrices inputDistances,
                            Rcpp::Nullable<int> sakoeChibaWindow = R_NilValue){
>>>>>>> sakoe_chiba_window
    
    int rawSeries_len = inputDistances.RawSeriesDistMatrices.size();
    int shapeDesc_len = inputDistances.ShapeDesciptorDistMatrices.size();
    
    if(rawSeries_len != shapeDesc_len)
      stop("There has to be the same number of distance matrices for both raw and shapeDescriptor series");
    
    std::vector<SimpleDTWResults> shapeDescDTWResults;
    
    for(int i = 0; i < shapeDesc_len; i++){
      shapeDescDTWResults.push_back(
<<<<<<< HEAD
        DTWRcpp(inputDistances.ShapeDesciptorDistMatrices[i])
=======
        DTWRcpp(inputDistances.ShapeDesciptorDistMatrices[i], sakoeChibaWindow)
>>>>>>> sakoe_chiba_window
      );
    }
    
    double rawSeriesDistance = 0;
    double shapeDescDistance = 0;
    std::vector<IntegerVector> warpingPathsP;
    std::vector<IntegerVector> warpingPathsQ;
    
    if(inputDistances.DistanceType == DEPENDENT){
      
      shapeDescDistance = shapeDescDTWResults[0].Distance;
      rawSeriesDistance = CalcDistanceFromWarpingPaths(
        inputDistances.RawSeriesDistMatrices[0],
        shapeDescDTWResults[0].WarpingPathP,
        shapeDescDTWResults[0].WarpingPathQ
      );
      
      warpingPathsP.push_back(shapeDescDTWResults[0].WarpingPathP);
      warpingPathsQ.push_back(shapeDescDTWResults[0].WarpingPathQ);
      
    }else{
      
      for(int i = 0; i < shapeDesc_len; i++){
        shapeDescDistance += shapeDescDTWResults[i].Distance;
        rawSeriesDistance += CalcDistanceFromWarpingPaths(
          inputDistances.RawSeriesDistMatrices[i],
          shapeDescDTWResults[i].WarpingPathP,
          shapeDescDTWResults[i].WarpingPathQ
        );
        
        warpingPathsP.push_back(shapeDescDTWResults[i].WarpingPathP);
        warpingPathsQ.push_back(shapeDescDTWResults[i].WarpingPathQ);
      }
    }
    
    DTWResults res;
    
    res.RawSeriesDistance = rawSeriesDistance;
    res.ShapeDescriptorsDistance = shapeDescDistance;
    res.WarpingPathsP = warpingPathsP;
    res.WarpingPathsQ = warpingPathsQ;
    
    return res;
  }
}