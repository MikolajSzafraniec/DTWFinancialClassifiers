#include <Rcpp.h>
#include "TimeSeriesTransformation.h"
#ifndef MyEnums
#define MyEnums
#endif
using namespace Rcpp;
using namespace TSTransformation;

namespace RcppDist{
  
  // Structure returned by main function from this scope
  struct DistMatrices{
    std::vector<NumericMatrix> RawSeriesDistMatrices;
    std::vector<NumericMatrix> ShapeDesciptorDistMatrices;
    MultidimensionalDTWTypes DistanceType;
  };
  
  // Distance between two points in euclidean space
  double DistanceCpp(double x, double y){
    double xy_pow = pow(x - y, 2);
    double res = pow(xy_pow, 0.5);
    return res;
  }

  // Distance between two vectors in euclidean space
  double DistanceCpp(NumericVector x, NumericVector y){
    int len_x = x.size();
    int len_y = y.size();
    
    if(len_x != len_y)
      stop("Size of both input vectors must match.");
    
    double xy_pow = 0;
    
    for(int i = 0; i < len_x; i++){
      xy_pow += pow(x[i] - y[i], 2);
    }
    
    double res = pow(xy_pow, 0.5);
    return res;
  }
  
  
  // Distance matrix between two vectors (raw dimensions of time series)
  NumericMatrix DistanceMatrixCpp(NumericVector x, NumericVector y){
    
    int len_x = x.size();
    int len_y = y.size();
    
    if(len_x != len_y)
      stop("Size of both input vectors must match.");
    
    NumericMatrix res(len_x, len_y);
    
    for(int i = 0; i < len_x; i++){
      for(int j = 0; j < len_y; j++){
        res(i, j) = DistanceCpp(x[i], y[j]);
      }
    }
    
    return res;
  }

  /*
   * Distance matrix between two matrices - it denotes two series transformed to
   * them shape descriptors series
   */
  NumericMatrix DistanceMatrixCpp(NumericMatrix x, NumericMatrix y){
    
    int x_nrow = x.nrow();
    int y_nrow = y.nrow();
    
    if(x_nrow != y_nrow)
      stop("Numbers of rows in both matrices must match");
    
    NumericMatrix res(x_nrow, y_nrow);
    
    for(int i = 0; i < x_nrow; i++){
      for(int j = 0; j < y_nrow; j++){
        res(i, j) = DistanceCpp(x(i, _), y(j, _));
      }
    }
    
    return res;
  }

  /*
   * Distance matrix between two raw series calculating in the way
   * designed for the dependent version of multidimensional DTW 
   */
  
  NumericMatrix DistanceMatrixCppDependent(NumericMatrix x, NumericMatrix y){
    
    int x_nrow = x.nrow();
    int y_nrow = y.nrow();
    
    if(x_nrow != y_nrow)
      stop("Numbers of rows of both matrices must match.");
    
    int x_dim = x.ncol();
    int y_dim = y.ncol();
    
    if(x_dim != y_dim)
      stop("Dimensions (number of column) of both matrices must match.");
    
    NumericMatrix res(x_nrow, y_nrow);
    
    double temp_dist;
    
    for(int i = 0; i < x_nrow; i ++){
      for(int j = 0; j < y_nrow; j++){
        temp_dist = 0;
        for(int k = 0; k < x_dim; k++)
          temp_dist += DistanceCpp(x(i, k), y(j, k));
        res(i, j) = temp_dist;
      }
    }
    
    return res;
  }
  
  /*
   * Distance matrix between two shape descriptors of the series calculating in the way
   * designed for the dependent version of multidimensional DTW 
   */
  
  NumericMatrix DistanceMatrixCppDependent(std::vector<NumericMatrix> x,
                                           std::vector<NumericMatrix> y){
    
    int x_dim = x.size();
    int y_dim = y.size();
    
    if(x_dim != y_dim)
      stop("Dimension of both series must match.");
    
    int x_size = x[0].nrow();
    int y_size = y[0].nrow();
    
    if(x_size != y_size)
      stop("Lenghts of both series must match.");
    
    double temp_dist;
    NumericMatrix res(x_size, y_size);
    
    for(int i = 0; i < x_size; i++){
      for(int j = 0; j < y_size; j++){
        temp_dist = 0;
        for(int k = 0; k < x_dim; k++)
          temp_dist += DistanceCpp(x[k](i, _), y[k](j, _));
        res(i, j) = temp_dist;
      }
    }
    
    return res;
  }

  /*
   * Calculation of distance matrices for both raw and transformed to shaped
   * descriptors time series. Calculation can be conducted in the way characteristic
   * to both dependent and independent multidimensional DTW
   */
  DistMatrices CalculateDistMatrices(TransformedTS transformedTSReference,
                                     TransformedTS transformedTSTest,
                                     MultidimensionalDTWTypes distanceType){
    
    std::vector<NumericMatrix> RawSeriesDistMatrices;
    std::vector<NumericMatrix> ShapeDescriptorDistMatrices;
    DistMatrices res;
    
    if(distanceType == DEPENDENT){
      RawSeriesDistMatrices.push_back(
        DistanceMatrixCppDependent(transformedTSReference.normalizedSeries,
                                   transformedTSTest.normalizedSeries));
      ShapeDescriptorDistMatrices.push_back(
        DistanceMatrixCppDependent(transformedTSReference.shapeDescriptorsSeries,
                                   transformedTSTest.shapeDescriptorsSeries)
      );
      
    }else{
      
      int rawSeriesDimRef = transformedTSReference.normalizedSeries.ncol();
      int rawSeriesDimTest = transformedTSTest.normalizedSeries.ncol();
      
      if(rawSeriesDimRef != rawSeriesDimTest)
        stop("Dimension of both series must match");
      
      for(int i = 0; i < rawSeriesDimRef; i++){
        
        RawSeriesDistMatrices.push_back(
          DistanceMatrixCpp(transformedTSReference.normalizedSeries(_, i),
                            transformedTSTest.normalizedSeries(_, i))
        );
        
        ShapeDescriptorDistMatrices.push_back(
          DistanceMatrixCpp(transformedTSReference.shapeDescriptorsSeries[i],
                            transformedTSTest.shapeDescriptorsSeries[i])
        );
      }
    }
    
    res.RawSeriesDistMatrices = RawSeriesDistMatrices;
    res.ShapeDesciptorDistMatrices = ShapeDescriptorDistMatrices;
    res.DistanceType = distanceType;
    
    return res;
  }
}