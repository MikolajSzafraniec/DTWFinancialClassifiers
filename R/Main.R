# Instalacja / Załadowanie niezbędnych pakietów

necPack <- function() {
  c("timeSeries", "lubridate", "stringr", "dplyr", "Rcpp", "dtw", "R.utils")
}
source("R/PackageLoading.R")

# Wywołanie pliku zawierającego funkcje ładujące szeregi czasowe różnego rodzaju
# (dzienne GPW, dzienne Forex, tickowe GPW, tickowe Forex)
source("R/FunctionsDataReadingAndPreprocessing.R")

# Test ładowania danych
FX_tick <- load_financial_data("Data/FX tick",
                               data_type = "FOREX_tick", include_all = T)

FX_day <- load_financial_data("Data/FX day",
                              data_type = "FOREX_da", include_all = T)

GPW_tick <- load_financial_data("Data/GPW tick",
                              data_type = "GPW_t", include_all = T)

GPW_day <- load_financial_data("Data/GPW day",
                              data_type = "GPW_d", include_all = T)

# FX tick przykładowy agregat
FXtickAgg <- FXTickAggregateAndFillNA(FXTickData = FX_tick$`AUDJPY-2019-01`, delta = dminutes(0.5))

# FX day przykładowy agregat
FXDayAgg <- FXDailyParse(FX_day$USDRUB)

# GPW Tick przykładowy agregat
GPWPattern <- load_financial_data("Data/PatternSeries/",
                                  data_type = "GPW_t", include_all = T)

GPWTickAgg <- GPWTickAggregateAndFillNA(GPW_tick$CCC, GPWPattern$WIG$Date)

# GPW day przykładowy agregat
GPWDayAgg <- GPWDailyParse(GPW_day$TATRY)

