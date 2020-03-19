# Załadowanie niezbędnych pakietów
necPack <- function() {
  c("timeSeries", "lubridate", "stringr", "dplyr", "Rcpp", "dtw", "inline")
}

source("R/PackageLoading.R")
# Załadowanie funkcji pakietu Rcpp
Rcpp::sourceCpp("src/RcppDTWFunctions.cpp")
subsequencesMatrix(1:10)
subsequencesMatrix(1:10, 2)

subsequencesMatrixCpp(1:2, 0)

testMatrix <- matrix(1:20, ncol = 2)
SDParams = new("ShapeDescriptorParams")
normType = "Unitarization"
tTrParams = new("TrigonometricTransformParams")

tsTransformationCpp(timeSeries = testMatrix, shapeDescriptorParams = SDParams, 
                    subsequenceWidth = 2, normalizationType = normType, tTrParams)
