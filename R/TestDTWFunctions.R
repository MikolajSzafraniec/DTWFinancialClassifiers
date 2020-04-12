# Załadowanie niezbędnych pakietów
necPack <- function() {
  c("timeSeries", "lubridate", "stringr", "dplyr", "Rcpp", "dtw", "inline")
}

require(furrr)
require(purrr)
require(future)
require(RcppShapeDTW)

source("R/PackageLoading.R")
# Załadowanie funkcji pakietu Rcpp
Rcpp::sourceCpp("src/RcppDTWFunctions.cpp")
testClone()

series1 <- matrix(rnorm(10), ncol = 1)
series2 <- matrix(rnorm(100), ncol = 1)


knnRes <- kNNShapeDTWCpp(referenceSeries = series1,
                         testSeries = series2, 
                         forecastHorizon = 5, 
                         subsequenceWidth = 5, 
                         subsequenceBreaks = 2, 
                         shapeDescriptorParams = SDP)
knnRes
z_normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

dtw::dtw(z_normalize(series1[,1]), z_normalize(series2[,1][36:45]), step.pattern = dtw::symmetric1)$distance
dtw::dtw(z_normalize(series1[,1]), z_normalize(series2[,1][36:45]), step.pattern = dtw::symmetric1)$index2


