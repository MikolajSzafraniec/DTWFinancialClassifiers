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
  
  std::vector<int> partialCumsum(nDesc);
  std::partial_sum(partialLengths.begin(), partialLengths.end(), partialCumsum.begin());
  std::vector<int> colBegins {0};
  colBegins.insert(colBegins.end(), partialCumsum.begin() + 1, partialCumsum.begin() - 1);
  std::vector<int> colEnds(nDesc);
  std::copy(partialCumsum.begin(), partialCumsum.end(), colEnds.begin());
  std::for_each(colEnds.begin(), colEnds.end(), [](double& d){d-= 1;});
  
  int outputNcol = Rcpp::sum(partialLengths);
  List AddParams = shapeDescriptorParams.slot("Additional_params");
  NumericVector Weights = AddParams["Weights"];
  //NumericMatrix res(inputNrow, outputNcol);
  NumericMatrix *res;
  res = new NumericMatrix(inputNrow, outputNcol);
  
  for(int i = 0; i < nDesc; i++){
    double currentWeight = Weights[i];
    
    (*res)(1, _) = 
      ShapeDescriptorsComputation::ComputeShapeDescriptors(subsequenceSeries, shapeDescriptorParams,
                                                           Descriptors[i]);
    
  }
}



//[[Rcpp::export]]
std::vector<int> testCumsum(std::vector<int> input){
  std::vector<int> res(input.size());
  std::partial_sum(input.begin(), input.end(), res.begin());
  return res;
}
