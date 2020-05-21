require(RcppShapeDTW)
require(farff)
require(Rcpp)
require(lubridate)
require(dplyr)
require(tidyr)
require(rje)
require(tibble)
require(stringr)

source("R/BenchmarkSeriesFunctions.R")
source("R/ClassDefinitions.R")

SDP_standard <- new("ShapeDescriptorParams")
SDP_shape <- new("ShapeDescriptorParams",
                 Type = "compound",
                 Descriptors = c("slopeDescriptor", "PAADescriptor"),
                 Additional_params = list(Weights = c(1, 1), PAAWindow = 3L, slopeWindow = 3L))

### Testing LIBRAS data set ###

files_paths_train_libras <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/",
                                     pattern = "[0-9]{,1}_TRAIN"))
files_paths_test_libras <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/Libras/",
                                     pattern = "[0-9]{,1}_TEST"))


train_series_libras <- load_benchmark_series_MD_dataset(filesPaths = files_paths_train_libras)
test_series_libras <- load_benchmark_series_MD_dataset(filesPaths = files_paths_test_libras)

test_whole_set_libras <- buildParametersSetBenchmarkSeries(benchmarkTS = test_series_libras, 
                                                    testSet = train_series_libras, 
                                                    shapeDTWParams = c(SDP_standard, SDP_shape))

libras_classification_results <- purrr::map(test_whole_set_libras, .f = function(args_set){
  do.call(what = benchSeriesSelfClassParallel_general, 
          args = c(args_set, switchToSequential = F))
})

future::plan(future::sequential)

libras_accuracy_results <- purrr::map(.x = libras_classification_results, .f = function(cres){
  sum(cres$ClassInd == cres$classificationResults) / nrow(cres)
})

libras_accuracy_table <- parseAccuracyResToTableBenchmark(libras_accuracy_results)
write.csv2(libras_accuracy_table, file = "Data/Results/libras_accuracy_res_v2.csv")

### Testing ERing data set ###

files_paths_train_ering <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ERing/", 
                                   list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ERing/",
                                              pattern = "[0-9]{,1}_TRAIN"))
files_paths_test_ering <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ERing/", 
                                  list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ERing/",
                                             pattern = "[0-9]{,1}_TEST"))


train_series_ering <- load_benchmark_series_MD_dataset(filesPaths = files_paths_train_ering)
test_series_ering <- load_benchmark_series_MD_dataset(filesPaths = files_paths_test_ering)

test_whole_set_ering <- buildParametersSetBenchmarkSeries(benchmarkTS = test_series_ering, 
                                                           testSet = train_series_ering, 
                                                           shapeDTWParams = c(SDP_standard, SDP_shape))


ering_classification_results <- purrr::map(test_whole_set_ering, .f = function(args_set){
  do.call(what = benchSeriesSelfClassParallel_general, 
          args = c(args_set, switchToSequential = F))
})

future::plan(future::sequential)

ering_accuracy_results <- purrr::map(.x = ering_classification_results, .f = function(cres){
  sum(cres$ClassInd == cres$classificationResults) / nrow(cres)
})

ering_accuracy_table <- parseAccuracyResToTableBenchmark(ering_accuracy_results)
write.csv2(ering_accuracy_table, file = "Data/Results/ering_accuracy_res_v2.csv")
