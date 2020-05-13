require(RcppShapeDTW)
require(farff)
require(Rcpp)
require(lubridate)
require(dplyr)
require(tidyr)
require(rje)
require(tibble)

source("R/BenchmarkSeriesFunctions.R")
source("R/ClassDefinitions.R")

SDP_standard <- new("ShapeDescriptorParams")
SDP_shape <- new("ShapeDescriptorParams",
                 Type = "compound",
                 Descriptors = c("slopeDescriptor", "PAADescriptor"),
                 Additional_params = list(Weights = c(1, 1), PAAWindow = 3L, slopeWindow = 3L))

files_paths_train <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/",
                                     pattern = "[0-9]{,1}_TRAIN"))
files_paths_test <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/",
                                     pattern = "[0-9]{,1}_TEST"))


train_series_full <- load_benchmark_series_MD_dataset(filesPaths = files_paths_train)
test_series_full <- load_benchmark_series_MD_dataset(filesPaths = files_paths_test)

first_tst <- benchSeriesSelfClassParallel_general(benchmarkTS = test_series_full, 
                                                  shapeDTWParams = SDP_standard,
                                                  testSet = train_series_full,
                                                  excludeCol = "tsNum",
                                                  normalizationType = "Z",
                                                  distanceType = "D",
                                                  targetDistance = "r")

test_whole_set <- buildParametersSetBenchmarkSeries(benchmarkTS = test_series_full, 
                                                    testSet = train_series_full, 
                                                    shapeDTWParams = c(SDP_standard, SDP_shape))

ft <- do.call(what = benchSeriesSelfClassParallel_general, 
              args = test_whole_set[[11]])
sum(ft$ClassInd == ft$classificationResults) / nrow(ft)


