#include "TrigonometricTransforms.h"
#include "ShapeDescriptorsComputation.h"
#include "SubsequenceFiller.h"
#ifndef MyEnums
#define MyEnums
#endif
using namespace TTR;
using namespace SDComputation;
using namespace SFiller;
using namespace Rcpp;

namespace TSTransformation{

  struct TransformedTS{
    NumericMatrix normalizedSeries;
    std::vector<NumericMatrix> shapeDescriptorsSeries;
  };

  NumericVector zScoreComputation(NumericVector input){
    
    double input_avg = Rcpp::mean(input);
    double input_sd = Rcpp::sd(input);
    int input_len = input.size();
    
    NumericVector output(input_len);
    
    for(int i = 0; i < input_len; i++)
      output[i] = (input[i] - input_avg) / input_sd;
    
    return output;
  }
  
  NumericVector unitarizationComputation(NumericVector input){
    
    double input_min = Rcpp::min(input);
    double input_max = Rcpp::max(input);
    double input_range = input_max - input_min;
    int input_len = input.size();
    
    
    NumericVector output(input_len);
    
    for(int i = 0; i < input_len; i++)
      output[i] = (input[i] - input_min) / input_range;
    
    return output;
  }
  
  // Function pointer definition
  typedef NumericVector (*TSNorm) (NumericVector);
  
  NumericVector TSNormalization(NumericVector input, std::string normalizationType){
    TSNorm fun;
    
    TSNormalizationMethods switchCondition =
      TSNormalizationMethodMap()[normalizationType];
    
    switch(switchCondition){
    case(ZSCORE):
      fun = zScoreComputation;
      break;
    case(UNITARIZATION):
      fun = unitarizationComputation;
      break;
    default:
      fun = unitarizationComputation;
    }
    
    return fun(input);
  }
  
  TransformedTS TsTransformation(NumericMatrix timeSeries, S4 shapeDescriptorParams,
                                 int subsequenceWidth, std::string normalizationType,
                                 Rcpp::Nullable<S4> trigonometricTransformParams = R_NilValue){
    
    IntegerVector dimsToTrigonometricTransform;
    
    // Adding trigonometric transforms of chosen dimensions if required
    if(trigonometricTransformParams.isNotNull()){
      
      S4 ttP(trigonometricTransformParams);
      
      dimsToTrigonometricTransform = ttP.slot("DimToApplyTransform");
      std::string transformType = ttP.slot("TransformType");
      
      for(int i = 0; i < dimsToTrigonometricTransform.size(); i++){
        int currentDim = dimsToTrigonometricTransform[i]-1;
        timeSeries = cbind(timeSeries,
                           trigonometicTransform(timeSeries(_, currentDim), transformType));
      }
    }
    
    int seriesDim = timeSeries.ncol();
    NumericMatrix timeSeriesCopy = Rcpp::clone(timeSeries);
    
    // Normalization of each dimension apparently
    for(int i = 0; i < seriesDim; i++){
      NumericMatrix::Column currentCol = timeSeriesCopy(_, i);
      currentCol = TSNormalization(currentCol, normalizationType);
    }
    
    // Transforming each dimension to subsequence matrix and then to shape descriptors series
    std::vector<NumericMatrix> shapeDescriptorSeries;
    NumericMatrix tempSubsequences;
    NumericMatrix tempShapeDescriptors;
    
    for(int i = 0; i < seriesDim; i++){
      tempSubsequences = subsequencesMatrix(timeSeriesCopy(_, i), subsequenceWidth);
      tempShapeDescriptors = asShapeDescriptor(tempSubsequences, shapeDescriptorParams);
      shapeDescriptorSeries.push_back(tempShapeDescriptors);
    }
    
    TransformedTS res = {
      timeSeriesCopy,
      shapeDescriptorSeries
    };
    
    return res;
  }
}