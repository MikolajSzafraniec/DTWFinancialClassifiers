#include <Rcpp.h>
#include <math.h>
#include "Iterators.h"
#include "RcppDistances.h"
#ifndef TimeSeriesTransformation
#define TimeSeriesTransformation
#endif
#ifndef MyEnums
#define MyEnums
#endif
#ifndef ShapeDescriptors
#define ShapeDescriptors
#endif
#ifndef TrigonometricTransforms
#define TrigonometricTransforms
#endif
#ifndef ShapeDescriptorsComputation
#define ShapeDescriptorsComputation
#endif
#ifndef SubsequenceFiller
#define SubsequenceFiller
#endif
using namespace Rcpp;
using namespace SD;
using namespace SDComputation;
using namespace TTR;
using namespace SFiller;
using namespace Iterators;
using namespace TSTransformation;
using namespace RcppDist;

//[[Rcpp::plugins("cpp11")]]

// Function auxiliary to asSubsequence function
//[[Rcpp::export]]
NumericMatrix subsequencesMatrixCpp(NumericVector values, int subsequenceWidth){
  return subsequencesMatrix(values, subsequenceWidth);
}

// Funkcja przekształcająca macierz podsekwencji w macierz deskryptorów kształtu
//[[Rcpp::export]]
NumericMatrix asShapeDescriptorCpp(NumericMatrix subsequenceSeries, S4 shapeDescriptorParams){
  return asShapeDescriptor(subsequenceSeries, shapeDescriptorParams);
}

/* Funkcja przekształcająca szereg czasowy w jego wybraną transformatę trygonometryczną.
 * WYbrana może zostać transformata kosinusowa, sinusowa oraz Hilberta.
 */

//[[Rcpp::export]]
NumericVector trigonometicTransformCpp(NumericVector input, std::string transformType){
  return trigonometicTransform(input, transformType);
}

//[[Rcpp::export]]
NumericVector TSNormalizationCpp(NumericVector input, std::string normType){
  return TSNormalization(input, normType);
}

//[[Rcpp::export]]
List tsTransformationCpp(NumericMatrix timeSeries, S4 shapeDescriptorParams,
                         int subsequenceWidth, std::string normalizationType,
                         Rcpp::Nullable<S4> trigonometricTransformParams = R_NilValue){
  
  TransformedTS tempRes = TsTransformation(timeSeries, shapeDescriptorParams,
                                           subsequenceWidth, normalizationType,
                                           trigonometricTransformParams);
  
  List res = List::create(
    tempRes.normalizedSeries,
    tempRes.shapeDescriptorsSeries
  );
  
  return res;
}

//[[Rcpp::export]]
List RcppDistancesTest(NumericMatrix timeSeriesRef, NumericMatrix timeSeriesTest,
                       S4 shapeDescriptorParams, int subsequenceWidth, std::string normalizationType,
                       Rcpp::Nullable<S4> trigonometricTransformParams = R_NilValue,
                       std::string distanceType = "Dependent"){
  
  TransformedTS tempResRefSeries = TsTransformation(timeSeriesRef, shapeDescriptorParams,
                                                    subsequenceWidth, normalizationType,
                                                    trigonometricTransformParams);
  
  MultidimensionalDTWTypes distT = 
    MultidimensionalDTWTypeMap()[distanceType];
  
  TransformedTS tempResTestSeries = TsTransformation(timeSeriesTest, shapeDescriptorParams,
                                                    subsequenceWidth, normalizationType,
                                                    trigonometricTransformParams);
  
  
  
  DistMatrices distMatrices = CalculateDistMatrices(tempResRefSeries,
                                                    tempResTestSeries,
                                                    distT);
  
  List res = List::create(
    distMatrices.RawSeriesDistMatrices,
    distMatrices.ShapeDesciptorDistMatrices
  );
  
  return res;
}