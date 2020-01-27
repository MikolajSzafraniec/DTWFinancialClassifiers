#include <Rcpp.h>
#include "SubsequenceFiller.h"
#include "ShapeDescriptorsComputation.h"
#include "TrigonometricTransforms.h"
#ifndef Enums
#define Enums
#endif
#ifndef ShapeDescriptors
#define ShapeDescriptors
#endif
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

//Funkcja pomocnicza do asSubsequence
//[[Rcpp::export]]
NumericMatrix subsequencesMatrix(NumericVector values, int subsequenceWidth){
  if(subsequenceWidth < 0)
    stop("Szerokosc okna musi byc liczba nieujemna");
  
  int subseqenceLength = (2*subsequenceWidth) + 1;
  int tsLength = values.length();
  NumericMatrix res(tsLength, subseqenceLength);
  
  auto SubFill = new SubsequenceFiller(&res, &values, tsLength, 
                                       subsequenceWidth, subseqenceLength);
  
  for(int i = 0; i < tsLength; i++){
    
    if(((i - subsequenceWidth) < 0) && ((i + subsequenceWidth) >= tsLength)){
    
      (*SubFill).ShortLeftShortRightFiller(i);
      
    }else if((i - subsequenceWidth) < 0){
      
      (*SubFill).ShortLeftFiller(i);
    
    }else if((i + subsequenceWidth) >= tsLength){
    
      (*SubFill).ShortRightFiller(i);
     
    }else{
      (*SubFill).OpenEndedFiller(i);
    }  
  }
  
  delete SubFill;
  return res;
  
}

// Funkcja przekształcająca macierz podsekwencji w macierz deskryptorów kształtu
//[[Rcpp::export]]
NumericMatrix asShapeDescriptorCpp(NumericMatrix subsequenceSeries, S4 shapeDescriptorParams){

  std::string shapeDescriptorType = shapeDescriptorParams.slot("Type");
  
  if(shapeDescriptorType.compare("simple") == 0){
    NumericMatrix res = ShapeDescriptorsComputation::ComputeShapeDescriptors(subsequenceSeries, shapeDescriptorParams,
                                                                             shapeDescriptorParams.slot("Descriptors"));
    return res;
  }
  
  int inputNcol = subsequenceSeries.ncol();
  int inputNrow = subsequenceSeries.nrow();
  
  std::vector<std::string> Descriptors = shapeDescriptorParams.slot("Descriptors");
  int nDesc = Descriptors.size();
  IntegerVector partialLengths(nDesc);
  
  for(int i = 0; i < nDesc; i++){
    partialLengths[i] = ShapeDescriptorsComputation::ComputeShapeDescriptorLength(inputNcol, shapeDescriptorParams,
                                                                                  Descriptors[i]);
  }
  
  std::vector<int> partialCumsum(nDesc);
  std::partial_sum(partialLengths.begin(), partialLengths.end(), partialCumsum.begin());
  std::vector<int> colBegins {0};
  colBegins.insert(colBegins.end(), partialCumsum.begin(), partialCumsum.end() - 1);
  
  int outputNcol = Rcpp::sum(partialLengths);
  List AddParams = shapeDescriptorParams.slot("Additional_params");
  NumericVector Weights = AddParams["Weights"];
  NumericMatrix res(inputNrow, outputNcol);
  int currentRowBegin = 0;
  int currentColBegin;
  double currentWeight;
  
  NumericMatrix *partialRes;
  
  for(int i = 0; i < nDesc; i++){
    currentWeight = Weights[i];
    currentColBegin = colBegins[i];
    
    partialRes = new NumericMatrix;
    *partialRes = ShapeDescriptorsComputation::ComputeShapeDescriptors(subsequenceSeries, 
                                                                       shapeDescriptorParams, 
                                                                       Descriptors[i]);
    
    ShapeDescriptorsComputation::MatrixPartialCopy(partialRes, &res, currentWeight, currentRowBegin,
                                                   currentColBegin);
    
    delete partialRes;
  }
   
  return res;
}

/* Funkcja przekształcająca szereg czasowy w jego wybraną transformatę trygonometryczną.
 * WYbrana może zostać transformata kosinusowa, sinusowa oraz Hilberta.
 */

// Definicja wskaźnika na funkcję
typedef NumericVector (*trTransform) (NumericVector);

//[[Rcpp::export]]
NumericVector trigonometicTransformCpp(NumericVector input, std::string transformType){
  
  trTransform fun;
  TrigonometricTransformTypes switchCondition =
    TrigonometricTransformTypeMap()[transformType];
  
  switch(switchCondition){
  case(COSINUS):
    fun = TrigonometricTransforms::DCT;
    break;
  case(SINUS):
    fun = TrigonometricTransforms::DST;
    break;
  case(HILBERT):
    fun = TrigonometricTransforms::DHT;
    break;
  default:
    fun = TrigonometricTransforms::DCT;
  }
  
  NumericVector res = fun(input);
  return res;
}

List MatricesVectorToList(std::vector<NumericMatrix> input){
  int len = input.size();
  List res (len);
  
  for(int i = 0; i < len; i++){
    res[i] = input[i];
  }
  
  return res;
}

//[[Rcpp::export]]
List MatrixToShapeDescriptors_V1(NumericMatrix ts, int subWidth, S4 shapeDescriptorParams){
  int tsNVariables = ts.ncol();
  std::vector<NumericMatrix> *SubsequenceVector =
    new std::vector<NumericMatrix>();
  
  std::vector<NumericMatrix> *ShapeDescVector =
    new std::vector<NumericMatrix>();
  
  for(int i = 0; i < tsNVariables; i++){
    (*SubsequenceVector).push_back(subsequencesMatrix(ts(_,i), subWidth));
  }
  
  for(int i = 0; i < tsNVariables; i++){
    (*ShapeDescVector).push_back(asShapeDescriptorCpp((*SubsequenceVector).back(),
                                 shapeDescriptorParams));
    (*SubsequenceVector).pop_back();
  }
  
  List res = MatricesVectorToList((*ShapeDescVector));
  delete ShapeDescVector;
  delete SubsequenceVector;
  return(res);
}


//[[Rcpp::export]]
List MatrixToShapeDescriptors_V2(NumericMatrix ts, int subWidth, S4 shapeDescriptorParams){
  
  int tsNVariables = ts.ncol();
  List subsequenceList (tsNVariables);
  List res (tsNVariables);
  
  for(int i = 0; i < tsNVariables; i++)
    subsequenceList[i] = subsequencesMatrix(ts(_,i), subWidth);
  
  for(int i = 0; i < tsNVariables; i++)
    res[i] = asShapeDescriptorCpp(subsequenceList[i], shapeDescriptorParams);

  return(res);
}