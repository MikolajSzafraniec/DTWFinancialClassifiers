#include <Rcpp.h>
#include "ShapeDescriptors.h"
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

// Klasa zawierająca metody obliczające macierz deskryptorów na podstawie zestawu przekazanych
// parametrów
class ShapeDescriptorsComputation{

public:
  static int ComputeShapeDescriptorLength(int subsequenceLength, S4 shapeDescriptorParams,
                                          std::string descriptorType){
    int res;
    
    switch(descriptorType[0]){
    case 'R':
      res = Rcpp::clone(subsequenceLength);
      break;
    case 'P':
      res = subsequenceLength - shapeDescriptorParams.slot("Additional_params")["PAAWindow"] + 1;
      break;
    case 'd':
      res = Rcpp::clone(subsequenceLength);
      break;
    case 's':
      res = subsequenceLength - shapeDescriptorParams.slot("Additional_params")["slopeWindow"] + 1;
      break;
    }
    
    return res;
  }
  
  static NumericMatrix ComputeShapeDescriptors(NumericMatrix subsequences, S4 shapeDescriptorParams,
                                               std::string descriptorType){
    int inputNcol = subsequences.ncol();
    int inputNrow = subsequences.nrow();
    int outputNrow = Rcpp::clone(inputNrow);
    int outputNcol = ComputeShapeDescriptorLength(inputNcol, shapeDescriptorParams, descriptorType);
    
    NumericMatrix res(outputNrow, outputNcol);
    
    for(int i = 0; i < outputNrow; i++){
      res(i, _) = clone(PickDescriptor(subsequences(i, _), shapeDescriptorParams, descriptorType));
    }
    
    return res;
  }
  
private:
  
  static NumericVector PickDescriptor(NumericVector subsequence, S4 shapeDescriptorParams,
                                              std::string descriptorType){
    NumericVector result;
    
    switch(descriptorType[0]){
    case 'R':
      result = ShapeDescriptors::RawSubsequence(subsequence);
      break;
    case 'P': {
        int PAAWindow = shapeDescriptorParams.slot("Additional_params")["PAAWindow"];
        result = ShapeDescriptors::PAADescriptor(subsequence, PAAWindow); 
      }
      break;
    case 'd':
      result = ShapeDescriptors::derivativeDescriptor(subsequence);
      break;
    case 's': {
        int slopeWindow = shapeDescriptorParams.slot("Additional_params")["slopeWindow"];
        result = ShapeDescriptors::slopeDescriptor(subsequence, slopeWindow);
      }
      break;
    }
    return result;
  }
  
  ShapeDescriptorsComputation() {}
};