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
  refSeriesStartIndices = seq(from = 2800, by = 10, length.out = 200), 
  input_params = params_set, 
  targetDistance = "r", 
  normalizationType = "Z", 
  knn = 150, 
  refSeriesLength = 100
)


nrow(FX_tick_d1min_filtered$`AUDUSD-2019-10`)
classResultsToAccuracyMeasureEuclidPreprocessing(FX_tick_d1min_results_AUDUSD, measure = "cor")
FX_tick_d1min_results_AUDUSD$dtw_type_Dependent.shape_desc_type_simple.dims1$target_series_150_returns

indices <- seq(from = 2800, by = 10, length.out = 200)

(FX_tick_d1min_filtered$`AUDUSD-2019-10`[indices[197]+99,1]@.Data - 
  FX_tick_d1min_filtered$`AUDUSD-2019-10`[indices[197]+99+150,1]@.Data) /
  FX_tick_d1min_filtered$`AUDUSD-2019-10`[indices[197]+99,1]@.Data
calcReturn(target_series = FX_tick_d1min_filtered$`AUDUSD-2019-10`,
           idx_begin = indices[196], 
           target_series_length =  100, 
           forecast_horizon = 150)
calcSD(target_series = FX_tick_d1min_filtered$`AUDUSD-2019-10`, 
       idx_begin = indices[196], 
       target_series_lenght = 100)
