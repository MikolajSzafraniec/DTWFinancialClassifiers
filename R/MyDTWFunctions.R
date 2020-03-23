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
testMatrix[,1] <- cos(seq(from = 0, to = pi, length.out = 50)*25)

SDParams = new("ShapeDescriptorParams", "Type" = "simple",
               "Descriptors" = "RawSubsequence")
normType = "Unitarization"
tTrParams = new("TrigonometricTransformParams",
                DimToApplyTransform = c(1L, 2L))

trRes <- tsTransformationCpp(timeSeries = testMatrix, shapeDescriptorParams = SDParams, 
                    subsequenceWidth = 0, normalizationType = normType, tTrParams)

plot(trRes[[1]][,3], type = "l")
