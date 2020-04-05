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


tst_matrix <- matrix(rnorm(1:100), nrow = 10, ncol = 10)
AccumulatedCostMatrixCppTest(tst_matrix)
dtw_tsts <- dtw::dtw(tst_matrix, keep.internals = T, step.pattern = dtw::symmetric1)
dtw_tsts$costMatrix

dtw::dtw
?dtw:::globalCostMatrix
dtw:::globalCostMatrix(tst_matrix)$costMatrix
microbenchmark::microbenchmark(dtw:::globalCostMatrix(tst_matrix)$costMatrix,
                               AccumulatedCostMatrixCppTest(tst_matrix))

unclass(dtw::symmetric1)
unclass(dtw::asymmetricP05)

a <- sample(1:10, 10)
b <- sample(1:10, 10)
distMat <- proxy::dist(a, b)

dtw_R <- dtw::dtw(a,b, keep.internals = T, step.pattern = dtw::symmetric1)
my_dtw <- SimpleDTWTest(distMat)
my_dtw
dtw_R$index1
dtw_R$index2
all(my_dtw$wpP == dtw_R$index1) & all(my_dtw$wpQ == dtw_R$index2)


AccumulatedCostMatrixCppTest(distMat)

microbenchmark::microbenchmark(dtw::dtw(distMat, keep.internals = F, step.pattern = dtw::symmetric1),
                               SimpleDTWTest(distMat))
