#include <Rcpp.h>
#include <math.h>
#include "Iterators.h"
#include "CppDTWFunctions.h"
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
#ifndef RcppDistances
#define RcppDistances
#endif
using namespace Rcpp;
using namespace SD;
using namespace SDComputation;
using namespace TTR;
using namespace SFiller;
using namespace Iterators;
using namespace TSTransformation;
using namespace RcppDist;
using namespace CppDTW;

//[[Rcpp::plugins("cpp11")]]

// Function auxiliary to asSubsequence function
//[[Rcpp::export]]
NumericMatrix RcppsubsequencesMatrix(NumericVector values, int subsequenceWidth){
  return subsequencesMatrix(values, subsequenceWidth);
}

// Funkcja przekształcająca macierz podsekwencji w macierz deskryptorów kształtu
//[[Rcpp::export]]
NumericMatrix RcppasShapeDescriptor(NumericMatrix subsequenceSeries, S4 shapeDescriptorParams){
  return asShapeDescriptor(subsequenceSeries, shapeDescriptorParams);
}

/* Funkcja przekształcająca szereg czasowy w jego wybraną transformatę trygonometryczną.
* WYbrana może zostać transformata kosinusowa, sinusowa oraz Hilberta.
*/

//[[Rcpp::export]]
NumericVector RcpptrigonometicTransform(NumericVector input, std::string transformType){
  return trigonometicTransform(input, transformType);
}

/*
* Function to normalize time series with usage of Unitarization method
* or Z-score normalization (x-mean(x)) / sd(x)
*/

//[[Rcpp::export]]
NumericVector RcppTSNormalization(NumericVector input, std::string normType){
  return TSNormalization(input, normType);
}

/*
* This function conducts normalization of multidimensional time series and transforms
* each dimension to the matrix of its shape descriptors. There is possiblity to add
* trigonometric transform of chosen dimensions before transformation. It will be
* transformed to the shape descriptors as well.
*/

//[[Rcpp::export]]
List RcpptsTransformation(NumericMatrix timeSeries, S4 shapeDescriptorParams,
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

/*
* This function generate distance matrices between raw series and series transformed
* to their shape descriptors.
*/

//[[Rcpp::export]]
List RcppDistanceMatrices(NumericMatrix timeSeriesRef, NumericMatrix timeSeriesTest,
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

/*
* This function transforms distance matrix to the accumulated cost matrix
* used by the DTW algorithm.
*/

//[[Rcpp::export]]
NumericMatrix RcppAccumulatedCostMatrix(NumericMatrix x){
  return AccumulatedCostMatrix(x);
}

/*
* This function returns DTW results (distance and warping paths)
* calculated based on the singular distance matrix
*/

//[[Rcpp::export]]
List RcppSimpleDTW(NumericMatrix x){
  SimpleDTWResults dtwRes = DTWRcpp(x);
  
  List res = List::create(
    dtwRes.Distance,
    dtwRes.WarpingPathP,
    dtwRes.WarpingPathQ
  );
  
  res.names() = CharacterVector({"Dist", "wpP", "wpQ"});
  
  return res;
}

/*
* This function calculate distance betweem time series based on the given distance
* matrix and warping paths calculated with DTW algorithm. It is useful to calculate
* distance between raw time series based on the warping paths determinated by shape
* descriptors of these series. 
*/

//[[Rcpp::export]]
double RcppdistanceFromWarpingPaths(NumericMatrix distMatrix,
                                    IntegerVector path1,
                                    IntegerVector path2){
  return CalcDistanceFromWarpingPaths(distMatrix, path1, path2);
}

/*
* This function return results of the complex DTW algorithm applied to the
* raw series and their shape descriptors.
*/

//[[Rcpp::export]]
List RcppComplexDTWResults(ListOf<NumericMatrix> RawSeriesDistMat,
                           ListOf<NumericMatrix> ShapeDescriptorMatrix,
                           std::string DistanceType = "Dependent"){
  
  std::vector<NumericMatrix> rawSeries;
  std::vector<NumericMatrix> shapeSeries;
  
  int n_series = RawSeriesDistMat.size();
  
  for(int i = 0; i < n_series; i++){
    rawSeries.push_back(RawSeriesDistMat[i]);
    shapeSeries.push_back(ShapeDescriptorMatrix[i]);
  }
  
  MultidimensionalDTWTypes distType = 
    MultidimensionalDTWTypeMap()[DistanceType];
  
  DistMatrices input;
  input.RawSeriesDistMatrices = rawSeries;
  input.ShapeDesciptorDistMatrices = shapeSeries;
  input.DistanceType = distType;
  
  DTWResults resCpp = ComplexDTWRcpp(input);
  
  List rRes;
  
  rRes.push_back(resCpp.RawSeriesDistance, "RawSeriesDistance");
  rRes.push_back(resCpp.ShapeDescriptorsDistance, "ShapeDescriptorsDistance");
  int warpPathSize = resCpp.WarpingPathsP.size();
  
  for(int i = 0; i < warpPathSize; i++){
    std::string tempName = "WarpingPaths_" + std::to_string(i);
    rRes.push_back(Rcpp::cbind(
        resCpp.WarpingPathsP[i],
                            resCpp.WarpingPathsQ[i]
    ), tempName);
  }
  
  return rRes;
}

/*
* General workflow for the shape DTW kNN algorithm implemented in the Rcpp.
* It takes two matrices as the input which represents two multidimensional
* time series - reference series and test series - and find nearest neighbour
* for the reference series among the several subsequences of the test series.
*/

//[[Rcpp::export]]
List kNNShapeDTWCpp(NumericMatrix referenceSeries,
                    NumericMatrix testSeries,
                    int forecastHorizon,
                    int subsequenceWidth,
                    int subsequenceBreaks,
                    S4 shapeDescriptorParams,
                    std::string normalizationType = "Unitarization",
                    std::string distanceType = "Dependent",
                    Rcpp::Nullable<S4> ttParams = R_NilValue){
  
  int refSeriesLength = referenceSeries.nrow();
  int testSeriesLengt = testSeries.nrow();
  int refSeriesDim = referenceSeries.ncol();
  int testSeriesDim = testSeries.ncol();
  
  MultidimensionalDTWTypes distanceTypeEnum =
    MultidimensionalDTWTypeMap()[distanceType];
  
  if(refSeriesDim != testSeriesDim)
    stop("Number of dimensions (columns) in both series must match.");
  
  IntegerVector iteratorsSet = Iterators::CreateTSIterator(refSeriesLength = refSeriesLength, 
                                                           testSeriesLengt = testSeriesLengt, 
                                                           forecastHorizon = forecastHorizon,
                                                           subsequenceBreaks = subsequenceBreaks);
  
  int itSetLen = iteratorsSet.length();
  NumericMatrix tempTestSeriesSubset(refSeriesLength, testSeriesDim);
  
  TransformedTS transformedTsRef = TSTransformation::TsTransformation(
    referenceSeries,
    shapeDescriptorParams = shapeDescriptorParams,
    subsequenceWidth = subsequenceWidth,
    normalizationType = normalizationType,
    ttParams
  );
  
  DTWResults finalResRawDist;
  DTWResults finalResShapeDescDist;
  DTWResults tempRes;
  
  int rawIdxBest = 0;
  int shapeIdxBest = 0;
  
  for(int i = 0; i < itSetLen; i++){
    int currentStart = iteratorsSet[i];
    int currentEnd = currentStart + refSeriesLength - 1;
    
    tempTestSeriesSubset = testSeries(Range(currentStart, currentEnd),
                                      Range(0, testSeriesDim-1));
    
    TransformedTS transformedTsTest = TSTransformation::TsTransformation(
      tempTestSeriesSubset,
      shapeDescriptorParams = shapeDescriptorParams,
      subsequenceWidth = subsequenceWidth,
      normalizationType = normalizationType,
      ttParams
    );
    
    DistMatrices tempDistMat = CalculateDistMatrices(
      transformedTsRef,
      transformedTsTest,
      distanceTypeEnum
    );
    
    tempRes = ComplexDTWRcpp(tempDistMat);
    
    if(i == 0){
      //finalResRawDist = Rcpp::clone(tempRes);
      //finalResShapeDescDist = Rcpp::clone(tempRes);
      CopyDTWResults(&finalResRawDist, &tempRes);
      CopyDTWResults(&finalResShapeDescDist, &tempRes);
    }else{
      
      if(tempRes.RawSeriesDistance < finalResRawDist.RawSeriesDistance){
        CopyDTWResults(&finalResRawDist, &tempRes);
        rawIdxBest = currentStart;
        //finalResRawDist = Rcpp::clone(tempRes);
      }
      
      if(tempRes.ShapeDescriptorsDistance < finalResShapeDescDist.ShapeDescriptorsDistance){
        CopyDTWResults(&finalResShapeDescDist, &tempRes);
        shapeIdxBest = currentStart;
        //finalResShapeDescDist = Rcpp::clone(tempRes);
      }
    }
  }
  
  List rawSeriesWarpingPaths;
  int rawWarpPathSize = finalResRawDist.WarpingPathsP.size();
  
  for(int i = 0; i < rawWarpPathSize; i++){
    std::string tempName = "WarpingPaths_" + std::to_string(i);
    rawSeriesWarpingPaths.push_back(Rcpp::cbind(
        finalResRawDist.WarpingPathsP[i],
                                     finalResRawDist.WarpingPathsQ[i]
    ), tempName);
  }
  
  List shapeDescSeriesWarpingPaths;
  int shapeWarPathSize = finalResShapeDescDist.WarpingPathsP.size();
  
  for(int i = 0; i < shapeWarPathSize; i++){
    std::string tempName = "WarpingPaths_" + std::to_string(i);
    shapeDescSeriesWarpingPaths.push_back(Rcpp::cbind(
        finalResShapeDescDist.WarpingPathsP[i],
                                           finalResShapeDescDist.WarpingPathsQ[i]
    ), tempName);
  }
  
  List res = List::create(
    _["RawSeriesDistanceResults"] = List::create(
      _["RawDistance"] = finalResRawDist.RawSeriesDistance,
      _["ShapeDescriptorsDistance"] = finalResRawDist.ShapeDescriptorsDistance,
      _["bestSubsequenceIdx"] = rawIdxBest+1,
      _["WarpingPaths"] = Rcpp::clone(rawSeriesWarpingPaths)
    ),
    _["ShapeDescriptorsDistanceResults"] = List::create(
      _["RawDistance"] = finalResShapeDescDist.RawSeriesDistance,
      _["ShapeDescriptorsDistance"] = finalResShapeDescDist.ShapeDescriptorsDistance,
      _["bestSubsequenceIdx"] = shapeIdxBest+1,
      _["WarpingPaths"] = Rcpp::clone(shapeDescSeriesWarpingPaths)
    )
  );
  
  return res;
}
