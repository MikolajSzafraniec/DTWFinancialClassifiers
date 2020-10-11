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

# Wywołanie skryptów zawierających niezbedne funkcje
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

# Wczytanie i filtrowanie szeregów GPW dla poziomu agregacji d = 30 minut
GPW_tick_d30min <- readRDS(file = "Data/TSExamples/GPW_tick_d30min.rds")

# Filtrowanie szeregu
GPW_tick_d30min_filtered <- purrr::map(GPW_tick_d30min, function(full_set){
  res <- rbind(
    window(full_set, start = as.Date("2017-04-03"), end = as.Date("2019-06-02")),
    window(full_set, start = as.Date("2019-06-03"), end = as.Date("2019-10-26"))
  )
  
  colnames(res) <- colnames(full_set)
  res
})

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

