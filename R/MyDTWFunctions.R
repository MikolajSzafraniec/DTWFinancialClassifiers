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

tsRef <- matrix(1:10, ncol = 2)
tsTest <- matrix(2:11, ncol = 2)
tsTest[,2] <- rnorm(5)

distTestRes <- RcppDistancesTest(timeSeriesRef = tsRef, timeSeriesTest = tsTest, 
                                 shapeDescriptorParams = SDParams, subsequenceWidth = 1, 
                                 normalizationType = normType, distanceType = "Independent")

z_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}

proxy::dist(z_norm(tsRef[,1]), z_norm(tsTest[,1]))
microbenchmark::microbenchmark(proxy::dist(z_norm(tsRef[,1]), z_norm(tsTest[,1])),
                               RcppDistancesTest(timeSeriesRef = tsRef, timeSeriesTest = tsTest, 
                                                 shapeDescriptorParams = SDParams, subsequenceWidth = 1, 
                                                 normalizationType = normType, distanceType = "Independent"))
