# Załadowanie niezbędnych pakietów
necPack <- function() {
  c("timeSeries", "lubridate", "stringr", "dplyr", "Rcpp", "dtw", "inline")
}

source("R/PackageLoading.R")
# Załadowanie funkcji pakietu Rcpp
Rcpp::sourceCpp("src/RcppDTWFunctions.cpp")
Rcpp::sourceCpp("src/RcppPreprocessingFunctions.cpp")

RMatrixToShapeDesc <- function(x, subWidth, shapeDescParams){
  nvariables = ncol(x)
  subsequenceList <- vector(mode = "list", length = nvariables)
  resultList <- vector(mode = "list", length = nvariables)
  
  for(i in 1:nvariables)
    subsequenceList[[i]] <- subsequencesMatrix(x[,i], subWidth)
  
  for(i in 1:nvariables)
    resultList[[i]] <- asShapeDescriptorCpp(subsequenceList[[i]], shapeDescParams)
  
  return(resultList)
}


SDP <- new("ShapeDescriptorParams", Type = "compound",
           Descriptors = c("RawSubsequence", "slopeDescriptor", "PAADescriptor"),
           Additional_params = list(PAAWindow = 2L, slopeWindow = 3L,
                                    Weights = c(0.5,0.5,0.5)))
subWidth <- 3
MyTS <- matrix(rnorm(3000, 0, 1), ncol = 30)

microbenchmark::microbenchmark(MatrixToShapeDescriptors_V1(MyTS, subWidth, SDP),
                               MatrixToShapeDescriptors_V2(MyTS, subWidth, SDP),
                               RMatrixToShapeDesc(MyTS, subWidth, SDP))

