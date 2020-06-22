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

### Loading and processing FX tick data ###
FX_tick_data_Oct_2019 <- load_financial_data(folder_path = "Data/EuclidPreprocSets/FX tick/Oct2019/", 
                                             data_type = "FOREX_tick", include_all = T)

FX_tick_data_Nov_2019 <- load_financial_data(folder_path = "Data/EuclidPreprocSets/FX tick/Nov2019/", 
                                             data_type = "FOREX_tick", include_all = T)

FX_tick_data_Dec_2019 <- load_financial_data(folder_path = "Data/EuclidPreprocSets/FX tick/Dec2019/", 
                                             data_type = "FOREX_tick", include_all = T)

FX_tick_data_Jan_2020 <- load_financial_data(folder_path = "Data/EuclidPreprocSets/FX tick/Jan2020/", 
                                             data_type = "FOREX_tick", include_all = T)

FX_tick_data_Feb_2020 <- load_financial_data(folder_path = "Data/EuclidPreprocSets/FX tick/Feb2020/", 
                                             data_type = "FOREX_tick", include_all = T)

FX_tick_whole_set <- purrr::pmap(list(Oct2019 = FX_tick_data_Oct_2019,
                                      Nov2019 = FX_tick_data_Nov_2019,
                                      Dec2019 = FX_tick_data_Dec_2019,
                                      Jan2020 = FX_tick_data_Jan_2020,
                                      Feb2020 = FX_tick_data_Feb_2020),
                                 function(Oct2019, Nov2019, Dec2019, Jan2020, Feb2020){
                                   res <- rbind(Oct2019, Nov2019, Dec2019, Jan2020, Feb2020)
                                   colnames(res) <- c("Instrument", "Date", "Open", "Close")
                                   return(res)
                                 })

rm(FX_tick_data_Oct_2019)
rm(FX_tick_data_Nov_2019)
rm(FX_tick_data_Dec_2019)
rm(FX_tick_data_Jan_2020)
rm(FX_tick_data_Feb_2020)

FX_tick_d1min <- purrr::map(.x = FX_tick_whole_set, 
                            .f = FXTickAggregateAndFillNA,
                            delta = lubridate::dminutes(1))

saveRDS(FX_tick_d1min, file = "Data/EuclidPreprocSets/FXTickSetsProcessed/FXTickd1min.RDS")

FX_tick_d5min <- purrr::map(.x = FX_tick_whole_set, 
                            .f = FXTickAggregateAndFillNA,
                            delta = lubridate::dminutes(5))

saveRDS(FX_tick_d5min, file = "Data/EuclidPreprocSets/FXTickSetsProcessed/FXTickd5min.RDS")

FX_tick_d10min <- purrr::map(.x = FX_tick_whole_set, 
                            .f = FXTickAggregateAndFillNA,
                            delta = lubridate::dminutes(10))

saveRDS(FX_tick_d10min, file = "Data/EuclidPreprocSets/FXTickSetsProcessed/FXTickd10min.RDS")

FX_tick_d30min <- purrr::map(.x = FX_tick_whole_set, 
                             .f = FXTickAggregateAndFillNA,
                             delta = lubridate::dminutes(30))

saveRDS(FX_tick_d30min, file = "Data/EuclidPreprocSets/FXTickSetsProcessed/FXTickd30min.RDS")

FX_daily_set <- load_financial_data(folder_path = "Data/EuclidPreprocSets/FX day/", 
                                    data_type = "FOREX_d", include_all = T)

FX_daily_parsed <- purrr::map(FX_daily_set, FXDailyParse)

# Preparing set of parameters 
params_set <- buildParamsSetEuclidPreprocessing(shape_DTW_params = c(SDP_traditional,
                                                                     SDP_compound), 
                                                trigonometric_transform_params = TTR, 
                                                subsequenceWidth = 4)

# FX tick delta 1 min #
FX_tick_d1min_filtered <- purrr::map(FX_tick_d1min, function(x){
  res <- x[50000:55000,]
})[-5]

FX_tick_d1min_results_AUDUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d1min_filtered$`AUDUSD-2019-10`, 
  learnSeriesList = FX_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(FX_tick_d1min_results_AUDUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d1min_results_AUDUSD.rds")

FX_tick_d1min_results_EURCHF <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d1min_filtered$`EURCHF-2019-10`, 
  learnSeriesList = FX_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(FX_tick_d1min_results_EURCHF, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d1min_results_EURCHF.rds")

FX_tick_d1min_results_EURPLN <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d1min_filtered$`EURPLN-2019-10`, 
  learnSeriesList = FX_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(FX_tick_d1min_results_EURPLN, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d1min_results_EURPLN.rds")

FX_tick_d1min_results_GBPUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d1min_filtered$`GBPUSD-2019-10`, 
  learnSeriesList = FX_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(FX_tick_d1min_results_GBPUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d1min_results_GBPUSD.rds")

# FX tick delta 5min #
FX_tick_d5min_filtered <- purrr::map(FX_tick_d5min[-5], function(x){
  res <- x[20000:25000,]
})


FX_tick_d5min_results_AUDUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d5min_filtered$`AUDUSD-2019-10`, 
  learnSeriesList = FX_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d5min_results_AUDUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d5min_results_AUDUSD.rds")

FX_tick_d5min_results_EURCHF <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d5min_filtered$`EURCHF-2019-10`, 
  learnSeriesList = FX_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d5min_results_EURCHF, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d5min_results_EURCHF.rds")

FX_tick_d5min_results_EURPLN <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d5min_filtered$`EURPLN-2019-10`, 
  learnSeriesList = FX_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d5min_results_EURPLN, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d5min_results_EURPLN.rds")

FX_tick_d5min_results_GBPUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d5min_filtered$`GBPUSD-2019-10`, 
  learnSeriesList = FX_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d5min_results_GBPUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d5min_results_GBPUSD.rds")

# FX tick delta 10min #
FX_tick_d10min_filtered <- purrr::map(FX_tick_d10min[-5], function(x){
  res <- x[5000:10000,]
})


FX_tick_d10min_results_AUDUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d10min_filtered$`AUDUSD-2019-10`, 
  learnSeriesList = FX_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d10min_results_AUDUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d10min_results_AUDUSD.rds")

FX_tick_d10min_results_EURCHF <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d10min_filtered$`EURCHF-2019-10`, 
  learnSeriesList = FX_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d10min_results_EURCHF, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d10min_results_EURCHF.rds")

FX_tick_d10min_results_EURPLN <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d10min_filtered$`EURPLN-2019-10`, 
  learnSeriesList = FX_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d10min_results_EURPLN, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d10min_results_EURPLN.rds")

FX_tick_d10min_results_GBPUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d10min_filtered$`GBPUSD-2019-10`, 
  learnSeriesList = FX_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d10min_results_GBPUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d10min_results_GBPUSD.rds")

# FX tick delta 30min #
FX_tick_d30min_filtered <- purrr::map(FX_tick_d30min[-5], function(x){
  res <- x[1:5000,]
})

FX_tick_d30min_results_AUDUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d30min_filtered$`AUDUSD-2019-10`, 
  learnSeriesList = FX_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d30min_results_AUDUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d30min_results_AUDUSD.rds")

FX_tick_d30min_results_EURCHF <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d30min_filtered$`EURCHF-2019-10`, 
  learnSeriesList = FX_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d30min_results_EURCHF, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d30min_results_EURCHF.rds")

FX_tick_d30min_results_EURPLN <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d30min_filtered$`EURPLN-2019-10`, 
  learnSeriesList = FX_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d30min_results_EURPLN, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d30min_results_EURPLN.rds")

FX_tick_d30min_results_GBPUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_tick_d30min_filtered$`GBPUSD-2019-10`, 
  learnSeriesList = FX_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_tick_d30min_results_GBPUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_tick_d30min_results_GBPUSD.rds")

# FX daily #

FX_daily_filtered <- purrr::map(FX_daily_parsed[-5], function(x){
  window(x, start = as.Date("2007-01-01"), end = Inf)
})

FX_daily_results_AUDUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_daily_filtered$AUDUSD, 
  learnSeriesList = FX_daily_filtered, 
  refSeriesStartIndices = seq(from = 1900, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_daily_results_AUDUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_daily_results_AUDUSD.rds")

FX_daily_results_EURCHF <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_daily_filtered$EURCHF, 
  learnSeriesList = FX_daily_filtered, 
  refSeriesStartIndices = seq(from = 1900, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_daily_results_EURCHF, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_daily_results_EURCHF.rds")

FX_daily_results_EURPLN <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_daily_filtered$EURPLN, 
  learnSeriesList = FX_daily_filtered, 
  refSeriesStartIndices = seq(from = 1900, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_daily_results_EURPLN, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_daily_results_EURPLN.rds")

FX_daily_results_GBPUSD <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = FX_daily_filtered$GBPUSD, 
  learnSeriesList = FX_daily_filtered, 
  refSeriesStartIndices = seq(from = 1900, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


saveRDS(FX_daily_results_GBPUSD, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/FX_daily_results_GBPUSD.rds")


###########################################################################
###                            GPW Data                                 ###
###########################################################################

# Loading and processing data

GPW_tick_set <- load_financial_data("Data/EuclidPreprocSets/GPW Tick/",
                                    data_type = "GPW_t", include_all = T)
GPW_pattern_series <- load_financial_data("Data/EuclidPreprocSets/PatternSeries/",
                                          data_type = "GPW_t", include_all = T)
GPW_pattern_d1min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = lubridate::dminutes(1),
                                             playOffTime = lubridate::dminutes(15),
                                             roundingUnit = "minute")
saveRDS(GPW_pattern_d1min, file = "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_pattern_d1min.rds")

GPW_pattern_d5min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = lubridate::dminutes(5),
                                             playOffTime = lubridate::dminutes(15),
                                             roundingUnit = "minute")
saveRDS(GPW_pattern_d5min, file = "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_pattern_d5min.rds")

GPW_pattern_d10min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = lubridate::dminutes(10),
                                             playOffTime = lubridate::dminutes(15),
                                             roundingUnit = "minute")
saveRDS(GPW_pattern_d10min, file = "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_pattern_d10min.rds")
GPW_pattern_d30min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = lubridate::dminutes(30),
                                              playOffTime = lubridate::dminutes(15),
                                              roundingUnit = "minute")
saveRDS(GPW_pattern_d30min, file = "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_pattern_d30min.rds")

# Aggregating sets

GPW_tick_d1min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                             patternDatesToAgg = GPW_pattern_d1min)

saveRDS(GPW_tick_d1min, "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_d1min.rds")

GPW_tick_d5min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                             patternDatesToAgg = GPW_pattern_d5min)

saveRDS(GPW_tick_d5min, "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_d5min.rds")

GPW_tick_d10min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                             patternDatesToAgg = GPW_pattern_d10min)

saveRDS(GPW_tick_d10min, "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_d10min.rds")

GPW_tick_d30min <- purrr::map(GPW_tick_set, GPWTickAggregateAndFillNA,
                              patternDatesToAgg = GPW_pattern_d30min)

saveRDS(GPW_tick_d30min, "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_d30min.rds")

rm(GPW_pattern_d1min)
rm(GPW_pattern_d5min)
rm(GPW_pattern_d10min)
rm(GPW_pattern_d30min)
rm(GPW_tick_set)

GPW_daily_data <- load_financial_data(folder_path = "Data/EuclidPreprocSets/GPW day/",
                                      data_type = "GPW_d", include_all = T)
GPW_daily_parsed <- purrr::map(GPW_daily_data, GPWDailyParse)
rm(GPW_daily_data)
saveRDS(GPW_daily_parsed, "Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_daily_parsed.rds")


### GPW delta 1 min ###
GPW_tick_d1min_filtered <- purrr::map(GPW_tick_d1min, function(x){
  window(x, start = as.Date("2015-07-01"), end = as.Date("2015-07-15"))
})

GPW_tick_d1min_results_BORYSZEW <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d1min_filtered$BORYSZEW, 
  learnSeriesList = GPW_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d1min_results_BORYSZEW, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d1min_results_BORYSZEW.rds")

GPW_tick_d1min_results_CCC <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d1min_filtered$CCC, 
  learnSeriesList = GPW_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d1min_results_CCC, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d1min_results_CCC.rds")

GPW_tick_d1min_results_LENTEX <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d1min_filtered$LENTEX, 
  learnSeriesList = GPW_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d1min_results_LENTEX, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d1min_results_LENTEX.rds")

GPW_tick_d1min_results_PGNIG <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d1min_filtered$PGNIG, 
  learnSeriesList = GPW_tick_d1min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d1min_results_PGNIG, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d1min_results_PGNIG.rds")



### GPW delta 5 min ###
GPW_tick_d5min_filtered <- purrr::map(GPW_tick_d5min, function(x){
  window(x, start = as.Date("2017-07-01"), end = as.Date("2017-09-15"))
})

GPW_tick_d5min_results_BORYSZEW <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d5min_filtered$BORYSZEW, 
  learnSeriesList = GPW_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d5min_results_BORYSZEW, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d5min_results_BORYSZEW.rds")

GPW_tick_d5min_results_CCC <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d5min_filtered$CCC, 
  learnSeriesList = GPW_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d5min_results_CCC, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d5min_results_CCC.rds")

GPW_tick_d5min_results_LENTEX <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d5min_filtered$LENTEX, 
  learnSeriesList = GPW_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d5min_results_LENTEX, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d5min_results_LENTEX.rds")

GPW_tick_d5min_results_PGNIG <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d5min_filtered$PGNIG, 
  learnSeriesList = GPW_tick_d5min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d5min_results_PGNIG, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d5min_results_PGNIG.rds")


### GPW delta 10 min ###
GPW_tick_d10min_filtered <- purrr::map(GPW_tick_d10min, function(x){
  window(x, start = as.Date("2017-01-01"), end = as.Date("2017-06-15"))
})

GPW_tick_d10min_results_BORYSZEW <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d10min_filtered$BORYSZEW, 
  learnSeriesList = GPW_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d10min_results_BORYSZEW, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d10min_results_BORYSZEW.rds")

GPW_tick_d10min_results_CCC <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d10min_filtered$CCC, 
  learnSeriesList = GPW_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d10min_results_CCC, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d10min_results_CCC.rds")

GPW_tick_d10min_results_LENTEX <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d10min_filtered$LENTEX, 
  learnSeriesList = GPW_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d10min_results_LENTEX, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d10min_results_LENTEX.rds")

GPW_tick_d10min_results_PGNIG <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d10min_filtered$PGNIG, 
  learnSeriesList = GPW_tick_d10min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d10min_results_PGNIG, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d10min_results_PGNIG.rds")


GPW_tick_d30min <- readRDS("Data/EuclidPreprocSets/GPWTickSetsProcessed/GPW_tick_d30min.rds")

### GPW delta 30 min ###
GPW_tick_d30min_filtered <- purrr::map(GPW_tick_d30min, function(x){
  window(x, start = as.Date("2018-01-01"), end = as.Date("2019-06-15"))
})

GPW_tick_d30min_results_BORYSZEW <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d30min_filtered$BORYSZEW, 
  learnSeriesList = GPW_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d30min_results_BORYSZEW, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d30min_results_BORYSZEW.rds")

GPW_tick_d30min_results_CCC <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d30min_filtered$CCC, 
  learnSeriesList = GPW_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d30min_results_CCC, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d30min_results_CCC.rds")

GPW_tick_d30min_results_LENTEX <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d30min_filtered$LENTEX, 
  learnSeriesList = GPW_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d30min_results_LENTEX, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d30min_results_LENTEX.rds")

GPW_tick_d30min_results_PGNIG <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_tick_d30min_filtered$PGNIG, 
  learnSeriesList = GPW_tick_d30min_filtered, 
  refSeriesStartIndices = seq(from = 2500, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_tick_d30min_results_PGNIG, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_tick_d30min_results_PGNIG.rds")


### GPW daily ###
GPW_daily_filtered <- purrr::map(GPW_daily_parsed, function(x){
  window(x, start = as.Date("2005-10-01"), end = Inf)
})

GPW_daily_results_BORYSZEW <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_daily_filtered$BORYSZEW, 
  learnSeriesList = GPW_daily_filtered, 
  refSeriesStartIndices = seq(from = 2200, by = 5, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_daily_results_BORYSZEW, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_daily_results_BORYSZEW.rds")

GPW_daily_results_CCC <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_daily_filtered$CCC, 
  learnSeriesList = GPW_daily_filtered, 
  refSeriesStartIndices = seq(from = 2200, by = 5, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_daily_results_CCC, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_daily_results_CCC.rds")

GPW_daily_results_LENTEX <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_daily_filtered$LENTEX, 
  learnSeriesList = GPW_daily_filtered, 
  refSeriesStartIndices = seq(from = 2200, by = 5, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_daily_results_LENTEX, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_daily_results_LENTEX.rds")

GPW_daily_results_PGNIG <- runShapeDTWForDefinedParamsTableWithEuclidPreprocessing(
  refSeries = GPW_daily_filtered$PGNIG, 
  learnSeriesList = GPW_daily_filtered, 
  refSeriesStartIndices = seq(from = 2200, by = 5, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)

saveRDS(GPW_daily_results_PGNIG, file = "Data/EuclidPreprocSets/ResultsEuclidPreproc/GPW_daily_results_PGNIG.rds")
