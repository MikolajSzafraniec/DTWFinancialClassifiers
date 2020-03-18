#include "ShapeDescriptors.h"
#ifndef MyEnums
#define MyEnums
#endif
using namespace Rcpp;
using namespace SD;

// Klasa zawierająca metody obliczające macierz deskryptorów na podstawie zestawu przekazanych
// parametrów

/*
 * This class contains methods which allow us to compute matrix of shape descriptors
 * with respect to passed params
 */

namespace SDComputation
{
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

  NumericMatrix asShapeDescriptor(NumericMatrix subsequenceSeries, S4 shapeDescriptorParams){
    
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
}