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

# Wywołanie pliku zawierającego funkcje ładujące szeregi czasowe różnego rodzaju
# (dzienne GPW, dzienne Forex, tickowe GPW, tickowe Forex)
source("R/FunctionsDataReadingAndPreprocessing.R")
source("R/ClassDefinitions.R")
source("R/RDTWCalculations.R")

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



# Data loading
FX_tick_Jan2020 <- load_financial_data("Data/FX tick/Jan2020/",
                               data_type = "FOREX_tick", include_all = T)

future::plan(future::multiprocess)
FXtickAgg_Jan2020_d1min <- furrr::future_map(.x = FX_tick_Jan2020, 
                                             .f = FXTickAggregateAndFillNA,
                                             delta = lubridate::dminutes(1))
future::plan(future::sequential)

input_params_Jan2020_d1min_100_50 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d1min, 
                                                            time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                            shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                            trigonometric_transform_params = TTR, 
                                                            subsequenceWidth = 4, 
                                                            learn_part_length = 100, 
                                                            forecast_part_length = 50, 
                                                            learn_set_n = 500, 
                                                            test_set_n = 100)

classResults_Jan2020_d1min_100_50 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d1min_100_50, 
                                                                      targetDistance = "raw", 
                                                                      normalizationType = "Z", 
                                                                      sd_border = 2)
saveRDS(classResults_Jan2020_d1min_100_50, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d1min_100_50.rds")

input_params_Jan2020_d1min_100_25 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d1min, 
                                                                   time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                   shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                   trigonometric_transform_params = TTR, 
                                                                   subsequenceWidth = 4, 
                                                                   learn_part_length = 100, 
                                                                   forecast_part_length = 25, 
                                                                   learn_set_n = 500, 
                                                                   test_set_n = 100)

classResults_Jan2020_d1min_100_25 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d1min_100_25, 
                                                                      targetDistance = "raw", 
                                                                      normalizationType = "Z", 
                                                                      sd_border = 2)

saveRDS(classResults_Jan2020_d1min_100_25, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d1min_100_25.rds")

# 100 / 100 version FX tick

input_params_Jan2020_d1min_100_100 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d1min, 
                                                                   time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                   shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                   trigonometric_transform_params = TTR, 
                                                                   subsequenceWidth = 4, 
                                                                   learn_part_length = 100, 
                                                                   forecast_part_length = 100, 
                                                                   learn_set_n = 500, 
                                                                   test_set_n = 100)

classResults_Jan2020_d1min_100_100 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d1min_100_100, 
                                                                      targetDistance = "raw", 
                                                                      normalizationType = "Z", 
                                                                      sd_border = 2)

saveRDS(classResults_Jan2020_d1min_100_100, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d1min_100_100.rds")


######## FX tick 5 minutes aggregation set #########
future::plan(future::multiprocess)
FXtickAgg_Jan2020_d5min <- furrr::future_map(.x = FX_tick_Jan2020, 
                                             .f = FXTickAggregateAndFillNA,
                                             delta = lubridate::dminutes(5))
future::plan(future::sequential)

# 100 / 25 version

input_params_Jan2020_d5min_100_25 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d5min, 
                                                                    time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                    shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                    trigonometric_transform_params = TTR, 
                                                                    subsequenceWidth = 4, 
                                                                    learn_part_length = 100, 
                                                                    forecast_part_length = 25, 
                                                                    learn_set_n = 500, 
                                                                    test_set_n = 100)

classResults_Jan2020_d5min_100_25 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d5min_100_25, 
                                                                       targetDistance = "raw", 
                                                                       normalizationType = "Z", 
                                                                       sd_border = 2)

saveRDS(classResults_Jan2020_d5min_100_25, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d5min_100_25.rds")

# 100 / 50 version

input_params_Jan2020_d5min_100_50 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d5min, 
                                                                   time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                   shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                   trigonometric_transform_params = TTR, 
                                                                   subsequenceWidth = 4, 
                                                                   learn_part_length = 100, 
                                                                   forecast_part_length = 50, 
                                                                   learn_set_n = 500, 
                                                                   test_set_n = 100)

classResults_Jan2020_d5min_100_50 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d5min_100_50, 
                                                                      targetDistance = "raw", 
                                                                      normalizationType = "Z", 
                                                                      sd_border = 2)

saveRDS(classResults_Jan2020_d5min_100_50, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d5min_100_50.rds")

# 100 / 100 version

input_params_Jan2020_d5min_100_100 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d5min, 
                                                                   time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                   shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                   trigonometric_transform_params = TTR, 
                                                                   subsequenceWidth = 4, 
                                                                   learn_part_length = 100, 
                                                                   forecast_part_length = 100, 
                                                                   learn_set_n = 500, 
                                                                   test_set_n = 100)

classResults_Jan2020_d5min_100_100 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d5min_100_100, 
                                                                      targetDistance = "raw", 
                                                                      normalizationType = "Z", 
                                                                      sd_border = 2)

saveRDS(classResults_Jan2020_d5min_100_100, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d5min_100_100.rds")

######## FX tick 10 minutes aggregation set #########
future::plan(future::multiprocess)
FXtickAgg_Jan2020_d10min <- furrr::future_map(.x = FX_tick_Jan2020, 
                                             .f = FXTickAggregateAndFillNA,
                                             delta = lubridate::dminutes(10))
future::plan(future::sequential)

# 100 / 25 version

input_params_Jan2020_d10min_100_25 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d10min, 
                                                                   time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                   shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                   trigonometric_transform_params = TTR, 
                                                                   subsequenceWidth = 4, 
                                                                   learn_part_length = 100, 
                                                                   forecast_part_length = 25, 
                                                                   learn_set_n = 500, 
                                                                   test_set_n = 100)

classResults_Jan2020_d10min_100_25 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d10min_100_25, 
                                                                      targetDistance = "raw", 
                                                                      normalizationType = "Z", 
                                                                      sd_border = 2)

saveRDS(classResults_Jan2020_d10min_100_25, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d10min_100_25.rds")

# 100 / 50 version

input_params_Jan2020_d10min_100_50 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d10min, 
                                                                    time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                    shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                    trigonometric_transform_params = TTR, 
                                                                    subsequenceWidth = 4, 
                                                                    learn_part_length = 100, 
                                                                    forecast_part_length = 50, 
                                                                    learn_set_n = 500, 
                                                                    test_set_n = 100)

classResults_Jan2020_d10min_100_50 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d10min_100_50, 
                                                                       targetDistance = "raw", 
                                                                       normalizationType = "Z", 
                                                                       sd_border = 2)

saveRDS(classResults_Jan2020_d10min_100_50, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d10min_100_50.rds")

# 100 / 100 version

input_params_Jan2020_d10min_100_100 <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d10min, 
                                                                    time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                    shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                    trigonometric_transform_params = TTR, 
                                                                    subsequenceWidth = 4, 
                                                                    learn_part_length = 100, 
                                                                    forecast_part_length = 100, 
                                                                    learn_set_n = 500, 
                                                                    test_set_n = 100)

classResults_Jan2020_d10min_100_100 <- runShapeDTWForDefinedParamsTable(input_params = input_params_Jan2020_d10min_100_100, 
                                                                       targetDistance = "raw", 
                                                                       normalizationType = "Z", 
                                                                       sd_border = 2)

saveRDS(classResults_Jan2020_d10min_100_100, file = "Data/Results/RDSFiles/tick_fx_classResults_Jan2020_d10min_100_100.rds")

rm(FX_tick_Jan2020)
######### Dane giełdowe akcje ############

GPW_tick <- load_financial_data("Data/GPW Tick/",
                                data_type = "GPW_t", include_all = T)
GPW_pattern_series <- load_financial_data("Data/PatternSeries/",
                                          data_type = "GPW_t", include_all = T)
GPWT_time_pattern_1min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = dminutes(1),
                                                  playOffTime = dminutes(15), roundingUnit = "minute")

GPWT_time_pattern_5min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = dminutes(5),
                                                  playOffTime = dminutes(15), roundingUnit = "minute")

GPWT_time_pattern_10min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = dminutes(10),
                                                  playOffTime = dminutes(15), roundingUnit = "minute")


future::plan(future::multiprocess)
GPW_tick_1min <- furrr::future_map(.x = GPW_tick, 
                                   .f = GPWTickAggregateAndFillNA, 
                                   patternDatesToAgg = GPWT_time_pattern_1min)
future::plan(future::sequential)

future::plan(future::multiprocess)
GPW_tick_5min <- furrr::future_map(.x = GPW_tick, 
                                   .f = GPWTickAggregateAndFillNA, 
                                   patternDatesToAgg = GPWT_time_pattern_5min)
future::plan(future::sequential)

future::plan(future::multiprocess)
GPW_tick_10min <- furrr::future_map(.x = GPW_tick, 
                                   .f = GPWTickAggregateAndFillNA, 
                                   patternDatesToAgg = GPWT_time_pattern_10min)
future::plan(future::sequential)

# Series filtration
GPW_tick_1min <- purrr::map(GPW_tick_1min,
                            function(x){
                              window(x, start = as.Date("2010-01-01"), end = Inf)
                            })

GPW_tick_5min <- purrr::map(GPW_tick_5min,
                            function(x){
                              window(x, start = as.Date("2010-01-01"), end = Inf)
                            })

GPW_tick_10min <- purrr::map(GPW_tick_10min,
                            function(x){
                              window(x, start = as.Date("2010-01-01"), end = Inf)
                            })


### 1 min wersja 100 / 25 ###

input_params_GPW_d1min_100_25 <- buildParamsSetFinancialSeries(ts_list = GPW_tick_1min, 
                                                               time_border = timeDate("2015-01-01 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                               shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                               trigonometric_transform_params = TTR, 
                                                               subsequenceWidth = 4, 
                                                               learn_part_length = 100, 
                                                               forecast_part_length = 25, 
                                                               learn_set_n = 500, 
                                                               test_set_n = 100)



classResults_GPW_d1min_100_25 <- runShapeDTWForDefinedParamsTable(input_params = input_params_GPW_d1min_100_25, 
                                                                        targetDistance = "raw", 
                                                                        normalizationType = "Z", 
                                                                        sd_border = 2)

saveRDS(classResults_GPW_d1min_100_25, file = "Data/Results/RDSFiles/classResults_GPW_d1min_100_25.rds")


### 1 min wersja 100 / 50 ###

input_params_GPW_d1min_100_50 <- buildParamsSetFinancialSeries(ts_list = GPW_tick_1min, 
                                                               time_border = timeDate("2015-01-01 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                               shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                               trigonometric_transform_params = TTR, 
                                                               subsequenceWidth = 4, 
                                                               learn_part_length = 100, 
                                                               forecast_part_length = 50, 
                                                               learn_set_n = 500, 
                                                               test_set_n = 100)



classResults_GPW_d1min_100_50 <- runShapeDTWForDefinedParamsTable(input_params = input_params_GPW_d1min_100_50, 
                                                                  targetDistance = "raw", 
                                                                  normalizationType = "Z", 
                                                                  sd_border = 1.5)

saveRDS(classResults_GPW_d1min_100_50, file = "Data/Results/RDSFiles/classResults_GPW_d1min_100_50.rds")

### 1 min wersja 100 / 100 ###

input_params_GPW_d1min_100_100 <- buildParamsSetFinancialSeries(ts_list = GPW_tick_1min, 
                                                               time_border = timeDate("2015-01-01 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                               shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                               trigonometric_transform_params = TTR, 
                                                               subsequenceWidth = 4, 
                                                               learn_part_length = 100, 
                                                               forecast_part_length = 100, 
                                                               learn_set_n = 500, 
                                                               test_set_n = 100)



classResults_GPW_d1min_100_100 <- runShapeDTWForDefinedParamsTable(input_params = input_params_GPW_d1min_100_100, 
                                                                  targetDistance = "raw", 
                                                                  normalizationType = "Z", 
                                                                  sd_border = 1.5)

saveRDS(classResults_GPW_d1min_100_100, file = "Data/Results/RDSFiles/classResults_GPW_d1min_100_100.rds")

### 5 min GPW wersja 100 / 25 ###

input_params_GPW_d5min_100_25 <- buildParamsSetFinancialSeries(ts_list = GPW_tick_5min, 
                                                                time_border = timeDate("2015-01-01 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                trigonometric_transform_params = TTR, 
                                                                subsequenceWidth = 4, 
                                                                learn_part_length = 100, 
                                                                forecast_part_length = 25, 
                                                                learn_set_n = 500, 
                                                                test_set_n = 100)



classResults_GPW_d5min_100_25 <- runShapeDTWForDefinedParamsTable(input_params = input_params_GPW_d5min_100_25, 
                                                                   targetDistance = "raw", 
                                                                   normalizationType = "Z", 
                                                                   sd_border = 1.5)

saveRDS(classResults_GPW_d5min_100_25, file = "Data/Results/RDSFiles/classResults_GPW_d5min_100_25.rds")
