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

# Wywołanie pliku zawierającego funkcje ładujące szeregi czasowe różnego rodzaju
# (dzienne GPW, dzienne Forex, tickowe GPW, tickowe Forex)
source("R/FunctionsDataReadingAndPreprocessing.R")
source("R/ClassDefinitions.R")
source("R/RDTWCalculations.R")

# Data loading
FX_tick_Jan2020 <- load_financial_data("Data/FX tick/Jan2020/",
                               data_type = "FOREX_tick", include_all = T)

future::plan(future::multiprocess)
FXtickAgg_Jan2020_d1min <- furrr::future_map(.x = FX_tick_Jan2020, 
                                             .f = FXTickAggregateAndFillNA,
                                             delta = lubridate::dminutes(1))
future::plan(future::sequential)


learn_series <- list(a = FXtickAgg_Jan2020_d1min$`AUDJPY-2020-01`[1:1500,],
                     b = FXtickAgg_Jan2020_d1min$`AUDNZD-2020-01`[1:1500,],
                     c = FXtickAgg_Jan2020_d1min$`AUDUSD-2020-01`[1:1500,],
                     d = FXtickAgg_Jan2020_d1min$`CADJPY-2020-01`[1:1500,])
test_series <- FXtickAgg_Jan2020_d1min$`CHFJPY-2020-01`[2001:3150,]

RunMultipleShapeDTWkNN(refSeries = test_series, 
                       learnSeries = learn_series, 
                       indicesVector = 1:10, 
                       shapeDTWParams = SDP_shape, 
                       targetDistance = "raw", 
                       distanceType = "Dependent", 
                       normalizationType = "Zscore", 
                       refSeriesLength = 100, 
                       subsequenceBreaks = 5, 
                       forecastHorizon = 50, 
                       includeRefSeries = F)
