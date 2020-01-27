#include <Rcpp.h>
#include "Enums.h"
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
    ShapeDescriptorTypes switchCondition =
      ShapeDescriptorTypeMap()[descriptorType];
    
    switch(switchCondition){
    case RAWSUBSEQUENCE:
      res = subsequenceLength;
      break;
    case PAADESCRIPTOR:{
      List AddParams = shapeDescriptorParams.slot("Additional_params");
      int PAAWindow = AddParams["PAAWindow"];
      res = subsequenceLength - PAAWindow + 1;
        }
      break;
    case DERIVATIVEDESCRIPTOR:
      res = subsequenceLength;
      break;
    case SLOPEDESCRIPTOR:{
      List AddParams = shapeDescriptorParams.slot("Additional_params");
      int slopeWindow = AddParams["slopeWindow"];
      res = subsequenceLength - slopeWindow + 1;
        }
      break;
    default:
      res = subsequenceLength;
      break;
    }
    
    return res;
  }
  
  static NumericMatrix ComputeShapeDescriptors(NumericMatrix subsequences, S4 shapeDescriptorParams,
                                               std::string descriptorType){
    int inputNcol = subsequences.ncol();
    int outputNrow = subsequences.nrow();
    int outputNcol = ComputeShapeDescriptorLength(inputNcol, shapeDescriptorParams, descriptorType);
    
    NumericMatrix res(outputNrow, outputNcol);
    
    for(int i = 0; i < outputNrow; i++){
      res(i, _) = clone(PickDescriptor(subsequences(i, _), shapeDescriptorParams, descriptorType));
    }
    
    return res;
  }
  
  static void MatrixPartialCopy(NumericMatrix *input, NumericMatrix *output, double Weight,
                                int RowBegin, int ColBegin){
    
    int inputNrow = (*input).nrow();
    int inputNcol = (*input).ncol();
    int currentOutputRow, currentOutputCol;
    
    for(int i = 0; i < inputNrow; i++){
      currentOutputRow = i + RowBegin;
      for(int j = 0; j < inputNcol; j++){
        currentOutputCol = j + ColBegin;
        (*output)(currentOutputRow, currentOutputCol) = (*input)(i, j) * Weight;
      }
    }
  }
  
private:
  
  typedef NumericVector (*descriptor) (NumericVector, List);
  
  static NumericVector PickDescriptor(NumericVector subsequence, S4 shapeDescriptorParams,
                                              std::string descriptorType){
    NumericVector result;
    descriptor desc;
    
    ShapeDescriptorTypes switchCondition = 
      ShapeDescriptorTypeMap()[descriptorType];
    
    List AddParams = shapeDescriptorParams.slot("Additional_params");
    
    switch(switchCondition){
    case RAWSUBSEQUENCE:
      desc = ShapeDescriptors::RawSubsequence;
      break;
    case PAADESCRIPTOR: 
      desc = ShapeDescriptors::PAADescriptor; 
      break;
    case DERIVATIVEDESCRIPTOR:
      desc = ShapeDescriptors::derivativeDescriptor;
      break;
    case SLOPEDESCRIPTOR: 
      desc = ShapeDescriptors::slopeDescriptor;
      break;
    default:
      desc = ShapeDescriptors::RawSubsequence;
    }
    
    result = desc(subsequence, AddParams);
    return result;
  }
  
  ShapeDescriptorsComputation() {}
};