/*
 * This file contains all enum classes and functions which allow us to convert
 * string values to those enums
 */
using namespace Rcpp;

namespace Enums{

  /*
   * Types of trigonometric transforms
   */

  enum TrigonometricTransformTypes
  {
    SINUS = 0,
    COSINUS = 1,
    HILBERT = 2
  };
  
  struct TrigonometricTransformTypeMap : public std::map<std::string, TrigonometricTransformTypes>
  {
    TrigonometricTransformTypeMap()
    {
      this->operator[]("sinus") = SINUS;
      this->operator[]("cosinus") = COSINUS;
      this->operator[]("hilbert") = HILBERT;
    };
    
    ~TrigonometricTransformTypeMap() {}
  };

  /*
   * Types of shape descriptors
   */

  enum ShapeDescriptorTypes
  {
    RAWSUBSEQUENCE = 0, 
    PAADESCRIPTOR = 1, 
    DERIVATIVEDESCRIPTOR = 2,
    SLOPEDESCRIPTOR = 3
  };

  struct ShapeDescriptorTypeMap : public std::map<std::string, ShapeDescriptorTypes>
  {
    ShapeDescriptorTypeMap()
    {
      this->operator[]("RawSubsequence") = RAWSUBSEQUENCE;
      this->operator[]("PAADescriptor") = PAADESCRIPTOR;
      this->operator[]("derivativeDescriptor") = DERIVATIVEDESCRIPTOR;
      this->operator[]("slopeDescriptor") = SLOPEDESCRIPTOR;
    };
    
    ~ShapeDescriptorTypeMap() {}
  };
  
  /*
   * Types of multidimensional DTW (dependent / independent)
   */
  
  enum MultidimensionalDTWTypes
  {
    DEPENDENT = 0,
    INDEPENDENT = 1
  };
  
  struct MultidimensionalDTWTypeMap : public std::map<std::string, MultidimensionalDTWTypes>
  {
    MultidimensionalDTWTypeMap()
    {
      this->operator[]("Dependent") = DEPENDENT;
      this->operator[]("Independent") = INDEPENDENT;
    }
  };
}