#include <Rcpp.h>
#include "ShapeDescriptors.h"
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

// Klasa zawierająca metody obliczające macierz deskryptorów na podstawie zestawu przekazanych
// parametrów
class ShapeDescriptorsComputation{

public:
  static int ComputeShapeDescriptorLength(int subsequenceLength, List shapeDescriptorParams,
                                          std::string descriptorType){
    int res;
    
    switch(descriptorType[0]){
    case 'R':
      res = subsequenceLength;
      break;
    case 'P':{
      List AddParams = shapeDescriptorParams["Additional_params"];
      int PAAWindow = AddParams["PAAWindow"];
      res = subsequenceLength - PAAWindow + 1;
        }
      break;
    case 'd':
      res = subsequenceLength;
      break;
    case 's':{
      List AddParams = shapeDescriptorParams["Additional_params"];
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
  
  static NumericMatrix ComputeShapeDescriptors(NumericMatrix subsequences, List shapeDescriptorParams,
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
  
  static void MatrixPartialCopy(NumericMatrix *input, NumericMatrix *output, int Weight,
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
  
  static NumericVector PickDescriptor(NumericVector subsequence, List shapeDescriptorParams,
                                              std::string descriptorType){
    NumericVector result;
    
    switch(descriptorType[0]){
    case 'R':
      result = ShapeDescriptors::RawSubsequence(subsequence);
      break;
    case 'P': {
      List AddParams = shapeDescriptorParams["Additional_params"];
      int PAAWindow = AddParams["PAAWindow"];  
      result = ShapeDescriptors::PAADescriptor(subsequence, PAAWindow); 
      }
      break;
    case 'd':
      result = ShapeDescriptors::derivativeDescriptor(subsequence);
      break;
    case 's': {
      List AddParams = shapeDescriptorParams["Additional_params"];
      int slopeWindow = AddParams["slopeWindow"];
      result = ShapeDescriptors::slopeDescriptor(subsequence, slopeWindow);
      }
      break;
    }
    return result;
  }
  
  ShapeDescriptorsComputation() {}
};