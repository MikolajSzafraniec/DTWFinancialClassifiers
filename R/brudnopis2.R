df <- data.frame(val = cumsum(rnorm(100)), x = 1:100)
df2 <- data.frame(val = cumsum(rnorm(100)), x = 1:100)
require(ggplot2)

par(mfrow = c(2, 1))
pl1 <- ggplot(data = df, aes(y = val, x = x)) +
  geom_line()

pl2 <- ggplot(data = df2, aes(y = val, x = x)) +
  geom_line()


grid.arrange(pl1, pl2, nrow = 2)
?grid.arrange


require(timeSeries)
require(ggplot2)
require(gridExtra)
require(dplyr)
require(tidyr)
require(RcppShapeDTW)
require(purrr)
require(furrr)
require(log4r)

refSeries <- FXtickAgg[100000:110000,]
testSeries <- FXtickAgg[1:50000,]

SDP <- new("ShapeDescriptorParams", Descriptors = "RawSubsequence")

SDP <- new("ShapeDescriptorParams", Type = "compound", 
                Descriptors = c("PAADescriptor", "slopeDescriptor"),
                "Additional_params" = list("slopeWindow" = 3L, "PAAWindow" = 3L,
                                           Weights = c(1, 1)))

refSeries1DIM <- FXtickAgg[100000:110000,1]
testSeries1DIM <- FXtickAgg[1:50000,1]

TTR <- new("TrigonometricTransformParams")


test_res <- RknnShapeDTWParallel(refSeries = refSeries,
                                 testSeries = list(testSeries = testSeries), 
                                 refSeriesStart = 7500, shapeDTWParams = SDP_comp, includeRefSeries = F,
                                 targetDistance = "s", subsequenceWidth = 5, forecastHorizon = 50, 
                                 sd_border = 2, refSeriesLength = 200)
test_res
plot(test_res,includeFrcstPart = F, add_wp = T, lift = 1)
plot(test_res,includeFrcstPart = T, add_wp = F, lift = 0)




require(R.utils)
require(stringr)
require(lubridate)

source("R/ClassDefinitions.R")
source("R/RDTWCalculations.R")
source("R/FunctionsDataReadingAndPreprocessing.R")

FX_tick <- load_financial_data("Data/FX tick",
                               data_type = "FOREX_tick", include_all = T)

future::plan(future::multiprocess)
FX_tick_agg <- furrr::future_map(.x = FX_tick, .f = FXTickAggregateAndFillNA,
                                 delta = lubridate::dseconds(1))
future::plan(future::sequential)


SDP_full <- new("ShapeDescriptorParams", 
                Type = "compound",
                Descriptors = c("PAADescriptor",
                                "slopeDescriptor"),
                Additional_params = list(
                  Weights = c(1, 1),
                  slopeWindow = 3L,
                  PAAWindow = 3L
                ))

test_res <- RknnShapeDTWParallel(refSeries = FX_tick_agg$`EURUSD-2020-02`[90000:95000,],
                                 testSeries = list("abc" = FX_tick_agg[[1]][50000:95000,]), 
                                 refSeriesStart = 50, shapeDTWParams = SDP_full, includeRefSeries = F,
                                 targetDistance = "r", subsequenceWidth = 10, forecastHorizon = 50, 
                                 sd_border = 2, refSeriesLength = 100)
test_res
plot(test_res,includeFrcstPart = F, add_wp = T, lift = 1)
plot(test_res,includeFrcstPart = T, add_wp = F, lift = 0)

test_res_multiple_10_min <- RunMultipleShapeDTWkNN(refSeries = FX_tick_agg$`EURUSD-2020-02`,
                                            testSeries = FX_tick_agg[-11], 
                                            indicesVector = seq(from = 1, by = 10, length.out = 250), 
                                            shapeDTWParams = SDP_full, 
                                            includeRefSeries = F, 
                                            targetDistance = "s", subsequenceWidth = 5, forecastHorizon = 25,
                                            refSeriesLength = 100, sd_border = 3)

plot(test_res_multiple_10_min$testSeriesReturn,
     test_res_multiple_10_min$refSeriesReturn)


test_res_multiple
cor(test_res_multiple_10_min$refSeriesReturn, test_res_multiple_10_min$testSeriesReturn)
sum(test_res_multiple_10_min$kNNSuccess)
table(test_res_multiple_10_min$refReturnClass,
      test_res_multiple_10_min$testReturnClass)
?table

table(c("a", "a", "b"), c("a", "a", "a"))
test_res_multiple_10_min2 <- test_res_multiple_10_min %>%
  mutate(refClassWithoutFlat = ifelse(refSeriesReturn < 0, "Fall", "Growth"),
         testClassWithoutFlat = ifelse(testSeriesReturn < 0, "Fall", "Growth")) %>%
  filter(testSeriesReturn > 0.002)


table(test_res_multiple_10_min2$refClassWithoutFlat,
      test_res_multiple_10_min2$testClassWithoutFlat)

test_res_multiple_2 <- test_res_multiple_10_min %>%
  mutate(refClassWithoutFlat = ifelse(refSeriesReturn < 0, "Fall", "Growth"),
         testClassWithoutFlat = ifelse(testSeriesReturn < 0, "Fall", "Growth"))

table(test_res_multiple_2$refClassWithoutFlat,
      test_res_multiple_2$testClassWithoutFlat)


# GPW Tick data
GPW_tick <- load_financial_data("Data/GPW tick",
                                data_type = "GPW_t", include_all = T)

GPW_pattern_series <- load_financial_data("Data/PatternSeries/",
                                          data_type = "GPW_t", include_all = T)
GPWTimePattern <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = dminutes(60),
                                          playOffTime = dminutes(15), roundingUnit = "minute")
future::plan(future::multiprocess)
GPW_tick_agg <- furrr::future_map(.x = GPW_tick, .f = GPWTickAggregateAndFillNA,
                                  patternDatesToAgg = GPWTimePattern)
future::plan(future::sequential)

purrr::map(GPW_tick_agg, nrow)

GPW_tick_agg <- purrr::map(GPW_tick_agg, .f = function(x){
  window(x, start = as.POSIXct("2012-01-01 00:00:00"), end = Inf)
})


test_res <- RknnShapeDTWParallel(refSeries = GPW_tick_agg$CCC,
                                 testSeries = GPW_tick_agg[-1], 
                                 refSeriesStart = 16400, shapeDTWParams = SDP_full, includeRefSeries = T,
                                 targetDistance = "s", subsequenceWidth = 5, forecastHorizon = 50, 
                                 sd_border = 2, refSeriesLength = 150, distanceType = "I")
test_res
plot(test_res,includeFrcstPart = F, add_wp = T, lift = 0)
plot(test_res,includeFrcstPart = T, add_wp = F, lift = 0)


test_res_GPW <- RunMultipleShapeDTWkNN(refSeries = GPW_tick_agg$CCC,
                                       testSeries = GPW_tick_agg[-1], 
                                       indicesVector = seq(from = 50000, by = 20, length.out = 100), 
                                       shapeDTWParams = SDP_full, 
                                       includeRefSeries = T, 
                                       targetDistance = "s", subsequenceWidth = 5, forecastHorizon = 100,
                                       refSeriesLength = 100, sd_border = 3)

cor(test_res_GPW$refSeriesReturn,
     test_res_GPW$testSeriesReturn)
sum(test_res_GPW$kNNSuccess)

tst_filtered <- test_res_GPW %>%
  filter(testReturnClass != "Flat_move") 

table(tst_filtered$refReturnClass, tst_filtered$testReturnClass)


benchStart <- Sys.time()
tstResBench <- benchSeriesSelfClassParallel(benchmarkTS = benchmarkSeriestst, 
                                            shapeDTWParams = SDP_full, 
                                            distanceType = "I", 
                                            normalizationType = "Z")
benchEnd <- Sys.time()

table(tstResBench$activitySym, tstResBench$classificationResults)

PCAtst <- prcomp(benchmarkSeriestst$activity_record[[1]], center = T,
                 scale. = T)

benchmarkSeriesPCA <- benchmarkSeriestst %>%
  dplyr::mutate(activity_record = purrr::map(activity_record, function(x){
    PCAres <- prcomp(x, center = T, scale. = T)
    as_tibble(PCAres$x[,1:30])
  }))
benchmarkSeriesPCA$activity_record[[1]]



benchStart <- Sys.time()
tstResBench <- benchSeriesSelfClassParallel(benchmarkTS = benchmarkSeriesPCA, 
                                            shapeDTWParams = SDP, 
                                            distanceType = "I", 
                                            normalizationType = "Z", 
                                            subsequenceWidth = 0)
benchEnd <- Sys.time()
benchEnd - benchStart

filesPaths <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ArticularyWordRecognition/", 
                    list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ArticularyWordRecognition/",
                         pattern = "[0-9]{,1}_TEST"))

filesPathsTrain <- paste0("Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/", 
                          list.files(path = "Data/BenchmarkSeries/MultivariateTS/Multivariate_arff/ERing/",
                                     pattern = "[0-9]{,1}_TRAIN"))

benchmarkTS <- load_benchmark_series_MD_dataset(filesPaths = filesPaths)
trainTS <- load_benchmark_series_MD_dataset(filesPaths = filesPathsTrain)



findNNBenchmarkSeries_general(benchmarkTS = benchmarkTS, 
                              refIndex = 1, 
                              excludeCol = "tsNum", 
                              shapeDTWParams = SDP, 
                              normalizationType = "Z")


SDP_simple <- new("ShapeDescriptorParams", Descriptors = "RawSubsequence")

test_ring <- benchSeriesSelfClassParallel_general(benchmarkTS = benchmarkTS, 
                                                  shapeDTWParams = SDP, 
                                                  excludeCol = "tsNum",
                                                  normalizationType = "Z",
                                                  distanceType = "D",
                                                  targetDistance = "r")

test_ring_normal_DTW <- benchSeriesSelfClassParallel_general(benchmarkTS = benchmarkTS, 
                                                  shapeDTWParams = SDP_simple,
                                                  excludeCol = "tsNum",
                                                  normalizationType = "Z", 
                                                  subsequenceWidth = 0, 
                                                  distanceType = "D", 
                                                  targetDistance = "r")

sum(as.integer(test_ring$ClassInd) == test_ring$classificationResults) / nrow(test_ring)
sum(as.integer(test_ring_normal_DTW$ClassInd) == test_ring_normal_DTW$classificationResults) / nrow(test_ring_normal_DTW)


plot(benchmarkTS$TS[[1]]$dim_3, type = "l")
lines(benchmarkTS$TS[[2]]$dim_3, col = "red")
