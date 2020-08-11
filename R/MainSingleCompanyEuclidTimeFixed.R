# Instalacja / Załadowanie niezbędnych pakietów
require(RcppShapeDTW)
require(farff)
require(Rcpp)
require(lubridate)
require(dplyr)
require(tidyr)
require(rje)
require(tibble)
require(stringr)
require(R.utils)
require(purrr)
require(furrr)
require(ggplot2)
require(gridExtra)
require(TTR)
require(timeSeries)

# Wywołanie pliku zawierającego funkcje ładujące szeregi czasowe różnego rodzaju
# (dzienne GPW, dzienne Forex, tickowe GPW, tickowe Forex)
source("R/FunctionsDataReadingAndPreprocessing.R")
source("R/ClassDefinitions.R")
source("R/RknnEuclideanAndDTWAdditions.R")

# Zdefiniowanie deskryptorów kształtu i transformaty trygonometrycznej
SDP_traditional <- new("ShapeDescriptorParams")
SDP_compound <- new("ShapeDescriptorParams",
                    Type = "compound",
                    Descriptors = c("PAADescriptor",
                                    "slopeDescriptor"),
                    Additional_params = list(
                      Weights = c(1, 10),
                      PAAWindow = 3L,
                      slopeWindow = 3L
                    ))
TTR <- new("TrigonometricTransformParams",
           DimToApplyTransform = c(1L, 2L),
           TransformType = "cosinus")

# Zbiór parametrów
params_set <- buildParamsSetEuclidPreprocessing(shape_DTW_params = c(SDP_traditional,
                                                                     SDP_compound), 
                                                trigonometric_transform_params = TTR, 
                                                subsequenceWidth = 4)


### Loading and processing FX tick data ###
FX_tick_data_2019_04 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_04/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_05 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_05/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_06 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_06/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_07 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_07/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_08 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_08/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_09 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_09/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_10 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_10/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_11 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_11/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2019_12 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2019_12/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2020_01 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2020_01/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_data_2020_02 <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX tick/2020_02/", 
                                            data_type = "FOREX_tick", include_all = T)

FX_tick_whole_set <- purrr::pmap(list(FX2019_04 = FX_tick_data_2019_04,
                                      FX2019_05 = FX_tick_data_2019_05,
                                      FX2019_06 = FX_tick_data_2019_06,
                                      FX2019_07 = FX_tick_data_2019_07,
                                      FX2019_08 = FX_tick_data_2019_08,
                                      FX2019_09 = FX_tick_data_2019_09,
                                      FX2019_10 = FX_tick_data_2019_10,
                                      FX2019_11 = FX_tick_data_2019_11,
                                      FX2019_12 = FX_tick_data_2019_12,
                                      FX2020_01 = FX_tick_data_2020_01,
                                      FX2020_02 = FX_tick_data_2020_02),
                                 function(FX2019_04, FX2019_05, FX2019_06, FX2019_07,
                                          FX2019_08, FX2019_09, FX2019_10, FX2019_11,
                                          FX2019_12, FX2020_01, FX2020_02){
                                   res <- rbind(FX2019_04, FX2019_05, FX2019_06, FX2019_07,
                                                FX2019_08, FX2019_09, FX2019_10, FX2019_11,
                                                FX2019_12, FX2020_01, FX2020_02)
                                   colnames(res) <- c("Instrument", "Date", "Open", "Close")
                                   return(res)
                                 })

rm(FX_tick_data_2019_04)
rm(FX_tick_data_2019_05)
rm(FX_tick_data_2019_06)
rm(FX_tick_data_2019_07)
rm(FX_tick_data_2019_08)
rm(FX_tick_data_2019_09)
rm(FX_tick_data_2019_10)
rm(FX_tick_data_2019_11)
rm(FX_tick_data_2019_12)
rm(FX_tick_data_2020_01)
rm(FX_tick_data_2020_02)

saveRDS(FX_tick_whole_set, file = "Data/EuclidPreproSingleStock/FXTickSetsProcessed/FX_tick_whole_set.rds")

FX_tick_d1min <- purrr::map(.x = FX_tick_whole_set, 
                            .f = FXTickAggregateAndFillNA,
                            delta = lubridate::dminutes(1))

saveRDS(FX_tick_d1min, file = "Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd1min.RDS")

FX_tick_d5min <- purrr::map(.x = FX_tick_whole_set, 
                            .f = FXTickAggregateAndFillNA,
                            delta = lubridate::dminutes(5))

saveRDS(FX_tick_d5min, file = "Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd5min.RDS")

FX_tick_d10min <- purrr::map(.x = FX_tick_whole_set, 
                             .f = FXTickAggregateAndFillNA,
                             delta = lubridate::dminutes(10))

saveRDS(FX_tick_d10min, file = "Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd10min.RDS")

FX_tick_d30min <- purrr::map(.x = FX_tick_whole_set, 
                             .f = FXTickAggregateAndFillNA,
                             delta = lubridate::dminutes(30))

saveRDS(FX_tick_d30min, file = "Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd30min.RDS")

FX_daily_set <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/FX day/", 
                                    data_type = "FOREX_d", include_all = T)

FX_daily_parsed <- purrr::map(FX_daily_set, FXDailyParse)
saveRDS(FX_daily_parsed, "Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXDailyParsed.rds")

# Loading and parsing GPW data
GPW_tick_set <- load_financial_data("Data/EuclidPreproSingleStock/GPW Tick/",
                                    data_type = "GPW_t", include_all = T)

GPW_pattern_d1min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_pattern_d1min.rds")
GPW_pattern_d5min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_pattern_d5min.rds")
GPW_pattern_d10min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_pattern_d10min.rds")
GPW_pattern_d30min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_pattern_d30min.rds")

GPW_tick_d1min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                             patternDatesToAgg = GPW_pattern_d1min)

saveRDS(GPW_tick_d1min, "Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d1min.rds")

GPW_tick_d5min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                             patternDatesToAgg = GPW_pattern_d5min)

saveRDS(GPW_tick_d5min, "Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d5min.rds")

GPW_tick_d10min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                              patternDatesToAgg = GPW_pattern_d10min)

saveRDS(GPW_tick_d10min, "Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d10min.rds")

GPW_tick_d30min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                              patternDatesToAgg = GPW_pattern_d30min)

saveRDS(GPW_tick_d30min, "Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d30min.rds")

rm(GPW_pattern_d1min)
rm(GPW_pattern_d5min)
rm(GPW_pattern_d10min)
rm(GPW_pattern_d30min)
rm(GPW_tick_set)

GPW_daily_data <- load_financial_data(folder_path = "Data/EuclidPreproSingleStock/GPW day/",
                                      data_type = "GPW_d", include_all = T)

GPW_daily_parsed <- purrr::map(GPW_daily_data, GPWDailyParse)
rm(GPW_daily_data)

saveRDS(GPW_daily_parsed, "Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_daily_parsed.rds")

# Data reading

FX_tick_d1min <- readRDS("Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd1min.RDS")
FX_tick_d5min <- readRDS("Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd5min.RDS")
FX_tick_d10min <- readRDS("Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd10min.RDS")
FX_tick_d30min <- readRDS("Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXTickd30min.RDS")
FX_daily_parsed <- readRDS("Data/EuclidPreproSingleStock/FXTickSetsProcessed/FXDailyParsed.rds")

GPW_tick_d1min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d1min.rds")
GPW_tick_d5min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d5min.rds")
GPW_tick_d10min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d10min.rds")
GPW_tick_d30min <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_tick_d30min.rds")
GPW_daily_parsed <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_daily_parsed.rds")


########### Forex data filtering ###########
FX_tick_d1min_filtered <- purrr::map(FX_tick_d1min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2019-07-25"), end = as.Date("2019-08-04")),
    window(full_set, start = as.Date("2019-08-05"), end = as.Date("2019-08-07"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})

FX_tick_d5min_filtered <- purrr::map(FX_tick_d5min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2019-06-18"), end = as.Date("2019-08-04")),
    window(full_set, start = as.Date("2019-08-05"), end = as.Date("2019-08-14"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})


FX_tick_d10min_filtered <- purrr::map(FX_tick_d10min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2019-03-04"), end = as.Date("2019-06-30")),
    window(full_set, start = as.Date("2019-07-01"), end = as.Date("2019-07-21"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})

FX_tick_d30min_filtered <- FX_tick_d30min

FX_daily_filtered <- purrr::map(FX_daily_parsed, function(x){
  window(x, start = as.Date("2006-09-19"), end = as.Date("2020-07-01"))
})

########### GPW data filtering ###########

GPW_tick_d1min_filtered <- purrr::map(GPW_tick_d1min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2019-05-06"), end = as.Date("2019-06-02")),
    window(full_set, start = as.Date("2019-06-03"), end = as.Date("2019-06-08"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})

GPW_tick_d5min_filtered <- purrr::map(GPW_tick_d5min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2019-04-15"), end = as.Date("2019-09-01")),
    window(full_set, start = as.Date("2019-09-02"), end = as.Date("2019-09-27"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})

GPW_tick_d10min_filtered <- purrr::map(GPW_tick_d10min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2018-11-26"), end = as.Date("2019-09-01")),
    window(full_set, start = as.Date("2019-09-02"), end = as.Date("2019-10-19"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})


GPW_tick_d30min_filtered <- purrr::map(GPW_tick_d30min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2017-04-03"), end = as.Date("2019-06-02")),
    window(full_set, start = as.Date("2019-06-03"), end = as.Date("2019-10-26"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})

GPW_daily_filtered <- purrr::map(GPW_daily_parsed, function(full_set){
  res <- window(full_set, start = as.Date("2006-08-17"), end = Inf)
  res
})

############ Processing ##############

# Forex tick delta 1 min

FX_tick_d1min_results_ref_25 <- purrr::map(FX_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9901, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d1min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d1min_results_ref_25.rds")

FX_tick_d1min_results_ref_50 <- purrr::map(FX_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9901, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d1min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d1min_results_ref_50.rds")


FX_tick_d1min_results_ref_100 <- purrr::map(FX_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9901, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d1min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d1min_results_ref_100.rds")

FX_tick_d1min_results_ref_200 <- purrr::map(FX_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9901, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d1min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d1min_results_ref_200.rds")

FX_tick_d1min_results_ref_400 <- purrr::map(FX_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9901, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d1min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d1min_results_ref_400.rds")


# GPW tick delta 1 min

GPW_tick_d1min_results_ref_25 <- purrr::map(GPW_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9641, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d1min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d1min_results_ref_25.rds")


GPW_tick_d1min_results_ref_50 <- purrr::map(GPW_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9641, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d1min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d1min_results_ref_50.rds")

GPW_tick_d1min_results_ref_100 <- purrr::map(GPW_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9641, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d1min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d1min_results_ref_100.rds")

GPW_tick_d1min_results_ref_200 <- purrr::map(GPW_tick_d1min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9641, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d1min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d1min_results_ref_200.rds")


# Forex tick delta 5 min

FX_tick_d5min_results_ref_25 <- purrr::map(FX_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9757, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d5min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d5min_results_ref_25.rds")


FX_tick_d5min_results_ref_50 <- purrr::map(FX_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9757, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d5min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d5min_results_ref_50.rds")

FX_tick_d5min_results_ref_100 <- purrr::map(FX_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9757, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d5min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d5min_results_ref_100.rds")

FX_tick_d5min_results_ref_200 <- purrr::map(FX_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9757, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d5min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d5min_results_ref_200.rds")

FX_tick_d5min_results_ref_400 <- purrr::map(FX_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9757, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d5min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d5min_results_ref_400.rds")


# GPW tick delta 5 min
GPW_tick_d5min_results_ref_25 <- purrr::map(GPW_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9119, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d5min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d5min_results_ref_25.rds")


GPW_tick_d5min_results_ref_50 <- purrr::map(GPW_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9119, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d5min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d5min_results_ref_50.rds")

GPW_tick_d5min_results_ref_100 <- purrr::map(GPW_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9119, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d5min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d5min_results_ref_100.rds")

GPW_tick_d5min_results_ref_200 <- purrr::map(GPW_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9119, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d5min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d5min_results_ref_200.rds")

GPW_tick_d5min_results_ref_400 <- purrr::map(GPW_tick_d5min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9119, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d5min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d5min_results_ref_400.rds")


# Forex tick delta 10 min

FX_tick_d10min_results_ref_25 <- purrr::map(FX_tick_d10min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2019-07-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d10min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d10min_results_ref_25.rds")

FX_tick_d10min_results_ref_50 <- purrr::map(FX_tick_d10min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2019-07-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d10min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d10min_results_ref_50.rds")


FX_tick_d10min_results_ref_100 <- purrr::map(FX_tick_d10min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2019-07-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d10min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d10min_results_ref_100.rds")

FX_tick_d10min_results_ref_200 <- purrr::map(FX_tick_d10min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2019-07-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d10min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d10min_results_ref_200.rds")


FX_tick_d10min_results_ref_400 <- purrr::map(FX_tick_d10min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2019-07-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d10min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d10min_results_ref_400.rds")


# GPW tick delta 10 min

GPW_tick_d10min_results_ref_25 <- purrr::map(GPW_tick_d10min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9262, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d10min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d10min_results_ref_25.rds")


GPW_tick_d10min_results_ref_50 <- purrr::map(GPW_tick_d10min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9262, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d10min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d10min_results_ref_50.rds")


GPW_tick_d10min_results_ref_100 <- purrr::map(GPW_tick_d10min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9262, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d10min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d10min_results_ref_100.rds")

GPW_tick_d10min_results_ref_200 <- purrr::map(GPW_tick_d10min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9262, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d10min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d10min_results_ref_200.rds")

GPW_tick_d10min_results_ref_400 <- purrr::map(GPW_tick_d10min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9262, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d10min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d10min_results_ref_400.rds")


# Forex tick delta 30 min


FX_tick_d30min_results_ref_25 <- purrr::map(FX_tick_d30min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2020-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  
  return(res)
})

saveRDS(FX_tick_d30min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d30min_results_ref_25.rds")


FX_tick_d30min_results_ref_50 <- purrr::map(FX_tick_d30min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2020-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d30min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d30min_results_ref_50.rds")

FX_tick_d30min_results_ref_100 <- purrr::map(FX_tick_d30min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2020-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d30min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d30min_results_ref_100.rds")

FX_tick_d30min_results_ref_200 <- purrr::map(FX_tick_d30min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2020-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d30min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d30min_results_ref_200.rds")


FX_tick_d30min_results_ref_400 <- purrr::map(FX_tick_d30min_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2020-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(FX_tick_d30min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_tick_d30min_results_ref_400.rds")


# GPW tick delta 30 min

GPW_tick_d30min_results_ref_25 <- purrr::map(GPW_tick_d30min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9062, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d30min_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d30min_results_ref_25.rds")


GPW_tick_d30min_results_ref_50 <- purrr::map(GPW_tick_d30min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9062, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d30min_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d30min_results_ref_50.rds")


GPW_tick_d30min_results_ref_100 <- purrr::map(GPW_tick_d30min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9062, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d30min_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d30min_results_ref_100.rds")

GPW_tick_d30min_results_ref_200 <- purrr::map(GPW_tick_d30min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9062, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d30min_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d30min_results_ref_200.rds")

GPW_tick_d30min_results_ref_400 <- purrr::map(GPW_tick_d30min_filtered, function(data_set){
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = 9062, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_tick_d30min_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_tick_d30min_results_ref_400.rds")


# Forex daily
purrr::map(FX_daily_filtered, nrow)
purrr::map(FX_daily_filtered, function(x){
  max(seq(from = sum(time(x) < timeDate(as.Date("2015-01-01")))+1, by = 10, length.out = 100)+600)
})

FX_daily_results_ref_25 <- purrr::map(FX_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2015-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  
  return(res)
})

saveRDS(FX_daily_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_daily_results_ref_25.rds")

FX_daily_results_ref_50 <- purrr::map(FX_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2015-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  
  return(res)
})

saveRDS(FX_daily_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_daily_results_ref_50.rds")



FX_daily_results_ref_100 <- purrr::map(FX_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2015-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  
  return(res)
})

saveRDS(FX_daily_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_daily_results_ref_100.rds")

FX_daily_results_ref_200 <- purrr::map(FX_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2015-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  
  return(res)
})

saveRDS(FX_daily_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_daily_results_ref_200.rds")


FX_daily_results_ref_400 <- purrr::map(FX_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2015-01-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  
  return(res)
})

saveRDS(FX_daily_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/FX_daily_results_ref_400.rds")


# GPW daily results

GPW_daily_results_ref_25 <- purrr::map(GPW_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2013-06-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 25, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_daily_results_ref_25, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_daily_results_ref_25.rds")

GPW_daily_results_ref_50 <- purrr::map(GPW_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2013-06-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 50, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_daily_results_ref_50, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_daily_results_ref_50.rds")

GPW_daily_results_ref_100 <- purrr::map(GPW_daily_filtered, function(data_set){

  starting_point <- sum(time(data_set) < timeDate(as.Date("2013-06-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 100, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_daily_results_ref_100, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_daily_results_ref_100.rds")

GPW_daily_results_ref_200 <- purrr::map(GPW_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2013-06-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 200, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_daily_results_ref_200, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_daily_results_ref_200.rds")

GPW_daily_results_ref_400 <- purrr::map(GPW_daily_filtered, function(data_set){
  
  starting_point <- sum(time(data_set) < timeDate(as.Date("2013-06-01"))) + 1
  cat("Starting point: ", starting_point, "\n")
  
  res <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
    refSeries = data_set, 
    learnSeriesList = list(data_set), 
    refSeriesStartIndices = seq(from = starting_point, by = 10, length.out = 100), 
    input_params = params_set, 
    targetDistance = "r", 
    normalizationType = "Z", 
    knn = 25, 
    refSeriesLength = 400, 
    forecastHorizons = c(5, 10, 25, 50, 75, 100, 150, 200), 
    sd_borders = c(0.5, 0.6, 1, 1.4, 1.7, 2, 2.4, 2.8),
    includeRefSeries = F
  )
  
  return(res)
})

saveRDS(GPW_daily_results_ref_400, file = "Data/EuclidPreproSingleStock/ResultsEuclidTimeFixed/GPW_daily_results_ref_400.rds")
