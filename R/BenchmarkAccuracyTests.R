require(RcppShapeDTW)
require(farff)
require(Rcpp)
require(lubridate)
require(dplyr)
require(tidyr)

source("R/BenchmarkSeriesFunctions.R")
source("R/ClassDefinitions.R")

SDP_standard <- new("ShapeDescriptorParams")
SDP_shape <- new("ShapeDescriptorParams",
                 Type = "compound",
                 Descriptors = c("slopeDescriptor", "PAADescriptor"),
                 Additional_params = list(Weights = c(1, 1), PAAWindow = 3L, slopeWindow = 3L))

files_paths_train <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/PenDigits/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/PenDigits/",
                                     pattern = "[0-9]{,1}_TRAIN"))
files_paths_test <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/PenDigits/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/PenDigits/",
                                     pattern = "[0-9]{,1}_TEST"))

train_series_full <- load_benchmark_series_MD_dataset(filesPaths = files_paths_train)
test_series_full <- load_benchmark_series_MD_dataset(filesPaths = files_paths_test)

first_tst <- benchSeriesSelfClassParallel_general(benchmarkTS = test_series_full, 
                                                  shapeDTWParams = SDP_shape,
                                                  testSet = train_series_full,
                                                  excludeCol = "tsNum",
                                                  normalizationType = "Z",
                                                  distanceType = "D",
                                                  targetDistance = "r")

sum(as.integer(first_tst$ClassInd) == first_tst$classificationResults) / nrow(first_tst)
