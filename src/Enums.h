/*
 * Skrypt zawierający struktury typu enum wykorzystywane w kodzie Rcpp
 */

#include <map>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

/*
 * Typy transformat trygonometrycznych
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
 * Typy deskryptorów kształtu
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