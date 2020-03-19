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

testMatrix <- matrix(1:100, ncol = 2)
SDParams = new("ShapeDescriptorParams", "Type" = "compound",
               "Descriptors" = c("slopeDescriptor", "RawSubsequence"),
               "Additional_params" = list(slopeWindow = 2L, Weights = c(1, 1)))
normType = "Unitarization"
tTrParams = new("TrigonometricTransformParams")

tsTransformationCpp(timeSeries = testMatrix, shapeDescriptorParams = SDParams, 
                    subsequenceWidth = 3, normalizationType = normType, tTrParams)

microbenchmark::microbenchmark(tsTransformationCpp(timeSeries = testMatrix, shapeDescriptorParams = SDParams, 
                                                   subsequenceWidth = 3, normalizationType = normType, tTrParams))
