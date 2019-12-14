#include <Rcpp.h>
#include "SubsequenceFiller.h"
#include "ShapeDescriptors.h"
#include "ShapeDescriptorsComputation.h"
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
NumericMatrix asShapeDescriptor(NumericMatrix subsequenceSeries, S4 shapeDescriptorParams){
  int inputNcol = subsequenceSeries.ncol();
  int inputNrow = subsequenceSeries.nrow();
  std::string shapeDescriptorType = shapeDescriptorParams.slot("Type");
  
  if(shapeDescriptorType.compare("simple")){
    NumericMatrix res = ShapeDescriptorsComputation::ComputeShapeDescriptors(subsequenceSeries, shapeDescriptorParams,
                                                                             shapeDescriptorParams.slot("Descriptors"));
    return res;
  }
  
  std::vector<std::string> Descriptors = shapeDescriptorParams.slot("Descriptors");
  int nDesc = Descriptors.size();
  IntegerVector partialLengths(nDesc);
  
  for(int i = 0; i < nDesc; i++){
    partialLengths[i] = ShapeDescriptorsComputation::ComputeShapeDescriptorLength(inputNcol, shapeDescriptorParams,
                                                                                  Descriptors[i]);
  }
  
}
