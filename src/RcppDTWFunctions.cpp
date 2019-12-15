#include <Rcpp.h>
#include "SubsequenceFiller.h"
#include "ShapeDescriptorsComputation.h"
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
  
  SubsequenceFiller SubFill = SubsequenceFiller(&res, &values, tsLength,
                                                subsequenceWidth, subseqenceLength);
  
  for(int i = 0; i < tsLength; i++){
    
    if(((i - subsequenceWidth) < 0) && ((i + subsequenceWidth) >= tsLength)){
    
      SubFill.ShortLeftShortRightFiller(i);
      
    }else if((i - subsequenceWidth) < 0){
      
      SubFill.ShortLeftFiller(i);
    
    }else if((i + subsequenceWidth) >= tsLength){
    
      SubFill.ShortRightFiller(i);
     
    }else{
      SubFill.OpenEndedFiller(i);
    }  
  }
  
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
  
  for(int i = 0; i < nDesc; i++){
    currentWeight = Weights[i];
    currentColBegin = colBegins[i];
    
    NumericMatrix partialRes = ShapeDescriptorsComputation::ComputeShapeDescriptors(subsequenceSeries,
                                                                                    shapeDescriptorParams,
                                                                                    Descriptors[i]);
    ShapeDescriptorsComputation::MatrixPartialCopy(&partialRes, &res, currentWeight, currentRowBegin,
                                                   currentColBegin);
  }
  
  return res;
   
}

