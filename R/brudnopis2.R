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





FXtickAgg_Jan2020_d1min_1_dim <- purrr::map(.x = FXtickAgg_Jan2020_d1min,
                                            function(x){
                                              x[,1]
                                            })

sample_first <- chooseRandomTestLearnSets(ts_list = FXtickAgg_Jan2020_d1min_1_dim, 
                                          time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                          learn_part_length = 100, 
                                          forecast_part_length = 50, 
                                          learn_set_n = 500, 
                                          test_set_n = 100)

RunMultipleShapeDTWkNN(refSeries = sample_first$test_series_list[[3]], 
                       learnSeries = sample_first$learn_series_list, 
                       indicesVector = 1, 
                       shapeDTWParams = SDP_shape, 
                       targetDistance = "raw", 
                       distanceType = "Dependent", 
                       normalizationType = "Zscore", 
                       refSeriesLength = 100, 
                       subsequenceBreaks = 1, 
                       forecastHorizon = 50, 
                       includeRefSeries = F, 
                       sd_border = 1)

input_params <- buildParamsSetFinancialSeries(ts_list = FXtickAgg_Jan2020_d1min, 
                                   time_border = timeDate("2020-01-15 00:05:00", format = "%Y-%m-%d %H:%M:%S"),
                                   shape_DTW_params = c(SDP, SDP_shape), 
                                   trigonometric_transform_params = trigonometric_transform_params, 
                                   learn_set_n = 500,
                                   test_set_n = 50)


res2 <- runShapeDTWForDefinedParamsTable(input_params = input_params, 
                                         normalizationType = "Z")


classResultsToAccuracyMeasure(classification_results_list = res2, 
                              measure = "acc", 
                              target_class = "Fl")

classResultsToAccuracyMeasure(classResults_Jan2020_d1min_100_50_target_s,
                              measure = "acc")

classResultsToAccuracyMeasure(classResults_Jan2020_d1min_100_50,
                              measure = "acc")


randomSeries <- FXtickAgg_Jan2020_d1min$`AUDJPY-2020-01`[1000:1100,]

normts <- RcppShapeDTW::RcppTSNormalization(randomSeries@.Data[,1], normType = "Z")
subs <- RcppShapeDTW::RcppsubsequencesMatrix(normts, 4)
subsSD <- RcppShapeDTW::RcppasShapeDescriptor(subs, SDP_compound)

mean(subsSD[,1:7])
sd(subsSD[,1:7])
mean(subsSD[,8:14])
sd(subsSD[,8:14])

balloonplot(as.table(classResultsToAccuracyMeasure(classResults_Jan2020_d1min_100_50,
                                          measure = "acc")))

a <- classResultsToAccuracyMeasure(classResults_Jan2020_d1min_100_50,
                                   measure = "acc")
b <- as.matrix(a[,-1])


balloonplot(b[,-1])
chisq.test(b*100)

classResultsToAccuracyMeasure(classResults_Jan2020_d1min_100_25, 
                              measure = "p", 
                              target_class = "Fl")

table(classResults_Jan2020_d1min_100_25$dtw_type_Dependent.shape_desc_type_simple.dims1$refReturnClass)

classResultsToAccuracyMeasure(classResults_Jan2020_d1min_100_100,
                              measure = "cor")

lapply(FXtickAgg_Jan2020_d5min, nrow)

classResultsToAccuracyMeasure(classResults_Jan2020_d5min_100_25,
                              measure = "cor")
classResultsToAccuracyMeasure(classResults_Jan2020_d5min_100_50,
                              "pr",
                              "G")

sum(sign(classResults_Jan2020_d1min_100_50$dtw_type_Dependent.shape_desc_type_simple.dims1_2_3$refSeriesReturn) ==
      sign(classResults_Jan2020_d1min_100_50$dtw_type_Dependent.shape_desc_type_simple.dims1_2_3$learnSeriesReturn))


classResultsToAccuracyMeasure(classResults_Jan2020_d5min_100_100,
                              measure = "acc")

classResultsToAccuracyMeasure(classResults_Jan2020_d10min_100_25,
                              measure = "acc")

sum(sign(classResults_Jan2020_d10min_100_25$dtw_type_Dependent.shape_desc_type_simple.dims1_2$refSeriesReturn) ==
      sign(classResults_Jan2020_d10min_100_25$dtw_type_Dependent.shape_desc_type_simple.dims1_2$learnSeriesReturn))




sum(sign(runif(1000, min = -1, max = 1)) == 1)

classResultsToAccuracyMeasure(classResults_GPW_d1min_100_25,
                              "acc")

table(classResults_GPW_d1min_100_25$dtw_type_Dependent.shape_desc_type_simple.dims1$refReturnClass)
plot(density(classResults_Jan2020_d10min_100_100$dtw_type_Dependent.shape_desc_type_simple.dims1$learnSeriesReturn))

class_ind <- function(x, sds, sd_border){
  res <- ifelse(x < sds*(-sd_border),
                "Fall", ifelse(x > sds*sd_border, "Growth", "Flat_move"))
  res
}

class_count(classResults_GPW_d1min_100_25$dtw_type_Dependent.shape_desc_type_simple.dims1$kNNSuccess,
            classResults_GPW_d1min_100_25$dtw_type_Dependent.shape_desc_type_simple.dims1$refTsSD,
            1.2)
classResults_GPW_d1min_100_25 <- purrr::map(classResults_GPW_d1min_100_25, function(x){
  x$refReturnClass <- class_ind(x$refSeriesReturn, x$refTsSD, 1.2)
  x$testReturnClass <- class_ind(x$learnSeriesReturn, x$testTsSD, 1.2)
  x$kNNSuccess <- as.integer(x$refReturnClass == x$testReturnClass)
  x
})


classResultsToAccuracyMeasure(classResults_GPW_d1min_100_50, "acc", "Fl")


CCC_test_set <- list(GPW_tick_1min$CCC[1000000:1011000,])
names(CCC_test_set) <- "CCC_test"

single_series_test_results <- RunMultipleShapeDTWkNN(refSeries = CCC_test_set$CCC_test, 
                                                     learnSeries = CCC_test_set, 
                                                     indicesVector = seq(from = 10000, by = 5, length.out = 100), 
                                                     shapeDTWParams = SDP_compound, 
                                                     targetDistance = "r", 
                                                     distanceType = "D", 
                                                     normalizationType = "Z", 
                                                     refSeriesLength = 100, 
                                                     forecastHorizon = 50, 
                                                     subsequenceBreaks = 1, 
                                                     includeRefSeries = F, 
                                                     switchBackToSequential = F)

nrow(FXtickAgg_Jan2020_d1min$`EURCHF-2020-01`)
FX_test_set <- list(FXtickAgg_Jan2020_d1min$`EURCHF-2020-01`[1:11000,])
names(FX_test_set) <- "FX_test"

single_series_test_results_FX <- RunMultipleShapeDTWkNN(refSeries = FX_test_set$FX_test, 
                                                     learnSeries = FX_test_set, 
                                                     indicesVector = seq(from = 10000, by = 5, length.out = 100), 
                                                     shapeDTWParams = SDP_compound, 
                                                     targetDistance = "r", 
                                                     distanceType = "D", 
                                                     normalizationType = "Z", 
                                                     refSeriesLength = 100, 
                                                     forecastHorizon = 50, 
                                                     subsequenceBreaks = 1, 
                                                     includeRefSeries = F, 
                                                     switchBackToSequential = F)

MACD_fx <- MACD(FX_test_set$FX_test@.Data[,1])
FX_test_set$FX_test <- cbind(FX_test_set$FX_test, MACD_fx[,1]/MACD_fx[,2])
FX_test_set$FX_test <- na.omit(FX_test_set$FX_test)

single_series_test_results_FX_MACD_added <- RunMultipleShapeDTWkNN(refSeries = FX_test_set$FX_test, 
                                                        learnSeries = FX_test_set, 
                                                        indicesVector = seq(from = 9967, by = 5, length.out = 100), 
                                                        shapeDTWParams = SDP_compound, 
                                                        targetDistance = "r", 
                                                        distanceType = "D", 
                                                        normalizationType = "Z", 
                                                        refSeriesLength = 100, 
                                                        forecastHorizon = 50, 
                                                        subsequenceBreaks = 1, 
                                                        includeRefSeries = F, 
                                                        switchBackToSequential = F)

sum(single_series_test_results_FX_MACD_added$kNNSuccess)
sum(single_series_test_results_FX$kNNSuccess)
table(single_series_test_results_FX_MACD_added$refReturnClass,
      single_series_test_results_FX_MACD_added$testReturnClass)
table(single_series_test_results_FX_MACD_added$testReturnClass)
table(single_series_test_results_FX$refReturnClass)
cor(single_series_test_results_FX_MACD_added$refSeriesReturn,
    single_series_test_results_FX_MACD_added$learnSeriesReturn)

plot(single_series_test_results_FX_MACD_added$refSeriesReturn[-c(90,91)],
     single_series_test_results_FX_MACD_added$learnSeriesReturn[-c(90,91)])

which.min(single_series_test_results_FX$refSeriesReturn[-91])

ref_res <- sum(sign(single_series_test_results_FX_MACD_added$refSeriesReturn) == 
                 sign(single_series_test_results_FX_MACD_added$learnSeriesReturn))

saveRDS(single_series_test_results_FX_MACD_added, file = "Data/Results/RDSFiles/single_series_test_results_FX_MACD_added.rds")

### GPW test for one series with OBV and MACD added to the series ###


CCC_test_set <- list(GPW_tick_1min$CCC[1000000:1011000,])
names(CCC_test_set) <- "CCC_test"

CCC_MACD <- MACD(CCC_test_set$CCC_test[,1])
CCC_OBV <- OBV(CCC_test_set$CCC_test@.Data[,1], CCC_test_set$CCC_test@.Data[,2])

CCC_test_set$CCC_test <- cbind(CCC_test_set$CCC_test,
                               CCC_MACD[,1] / CCC_MACD[,2],
                               CCC_OBV)
CCC_test_set$CCC_test <- na.omit(CCC_test_set$CCC_test)


single_series_test_results_GPW_MACD_OBV <- RunMultipleShapeDTWkNN(refSeries = CCC_test_set$CCC_test, 
                                                     learnSeries = CCC_test_set, 
                                                     indicesVector = seq(from = 9967, by = 5, length.out = 100), 
                                                     shapeDTWParams = SDP_compound, 
                                                     targetDistance = "r", 
                                                     distanceType = "D", 
                                                     normalizationType = "Z", 
                                                     refSeriesLength = 100, 
                                                     forecastHorizon = 50, 
                                                     subsequenceBreaks = 1, 
                                                     includeRefSeries = F, 
                                                     switchBackToSequential = F)

GPWT_time_pattern_10min <- parsePatternGPWTickTime(GPW_pattern_series$WIG20, delta = dminutes(10),
                                                  playOffTime = dminutes(15), roundingUnit = "minute")
future::plan(future::multiprocess)
GPW_tick_10min <- furrr::future_map(.x = GPW_tick, 
                                   .f = GPWTickAggregateAndFillNA, 
                                   patternDatesToAgg = GPWT_time_pattern_10min)
future::plan(future::sequential)

### GPW test for one 10 minutes diff series with OBV and MACD added to the series ###

lapply(GPW_tick_10min, nrow)

PGNIG_test_set <- list(GPW_tick_10min$PGNIG[100000:111000,])
names(PGNIG_test_set) <- "PGNIG_test"

PGNIG_MACD <- MACD(PGNIG_test_set$PGNIG_test[,1])
PGNIG_OBV <- OBV(PGNIG_test_set$PGNIG_test@.Data[,1], PGNIG_test_set$PGNIG_test@.Data[,2])

PGNIG_test_set$PGNIG_test <- cbind(PGNIG_test_set$PGNIG_test,
                               PGNIG_MACD[,1] / PGNIG_MACD[,2],
                               PGNIG_OBV)
PGNIG_test_set$PGNIG_test <- na.omit(PGNIG_test_set$PGNIG_test)

single_series_test_results_GPW_MACD_OBV_PGNIG <- RunMultipleShapeDTWkNN(refSeries = PGNIG_test_set$PGNIG_test, 
                                                                  learnSeries = PGNIG_test_set, 
                                                                  indicesVector = seq(from = 10000, by = 5, length.out = 100), 
                                                                  shapeDTWParams = SDP_compound, 
                                                                  targetDistance = "r", 
                                                                  distanceType = "D", 
                                                                  normalizationType = "Z", 
                                                                  refSeriesLength = 100, 
                                                                  forecastHorizon = 50, 
                                                                  subsequenceBreaks = 1, 
                                                                  includeRefSeries = F, 
                                                                  switchBackToSequential = F)

plot(single_series_test_results_GPW_MACD_OBV_PGNIG$refSeriesReturn,
    single_series_test_results_GPW_MACD_OBV_PGNIG$learnSeriesReturn)
sum(sign(single_series_test_results_GPW_MACD_OBV_PGNIG$refSeriesReturn) ==
      sign(single_series_test_results_GPW_MACD_OBV_PGNIG$learnSeriesReturn))

# FX tick 5 min with MACD and OBV

nrow(FXtickAgg_Jan2020_d5min$`EURGBP-2020-01`)

EURGBP_test_set <- list(FXtickAgg_Jan2020_d5min$`EURGBP-2020-01`)
names(EURGBP_test_set) <- "EURGBP_test"

EURGBP_MACD <- MACD(EURGBP_test_set$EURGBP_test[,1])
EURGBP_OBV <- OBV(EURGBP_test_set$EURGBP_test@.Data[,1], EURGBP_test_set$EURGBP_test@.Data[,2])

EURGBP_test_set$EURGBP_test <- cbind(EURGBP_test_set$EURGBP_test,
                                     EURGBP_MACD[,1] / EURGBP_MACD[,2],
                                     EURGBP_OBV)
EURGBP_test_set$EURGBP_test <- na.omit(EURGBP_test_set$EURGBP_test)

single_series_test_results_FX_5_min_EURGBP <- RunMultipleShapeDTWkNN(refSeries = EURGBP_test_set$EURGBP_test, 
                                                                        learnSeries = EURGBP_test_set, 
                                                                        indicesVector = seq(from = 5500, by = 5, length.out = 100), 
                                                                        shapeDTWParams = SDP_compound, 
                                                                        targetDistance = "r", 
                                                                        distanceType = "D", 
                                                                        normalizationType = "Z", 
                                                                        refSeriesLength = 100, 
                                                                        forecastHorizon = 50, 
                                                                        subsequenceBreaks = 1, 
                                                                        includeRefSeries = F, 
                                                                        switchBackToSequential = F)

single_series_test_results_FX_5_min_EURGBP_v2 <- RunMultipleShapeDTWkNN(refSeries = EURGBP_test_set$EURGBP_test, 
                                                                     learnSeries = EURGBP_test_set, 
                                                                     indicesVector = seq(from = 5500, by = 5, length.out = 100), 
                                                                     shapeDTWParams = SDP_compound, 
                                                                     targetDistance = "s", 
                                                                     distanceType = "D", 
                                                                     normalizationType = "Z", 
                                                                     refSeriesLength = 100, 
                                                                     forecastHorizon = 25, 
                                                                     subsequenceBreaks = 1, 
                                                                     includeRefSeries = F, 
                                                                     switchBackToSequential = F)

sum(single_series_test_results_FX_5_min_EURGBP_v2$kNNSuccess)
sum(sign(single_series_test_results_FX_5_min_EURGBP_v2$refSeriesReturn) ==
    sign(single_series_test_results_FX_5_min_EURGBP_v2$learnSeriesReturn))

table(single_series_test_results_FX_5_min_EURGBP_v2$refReturnClass,
        single_series_test_results_FX_5_min_EURGBP_v2$testReturnClass)

a <- RknnShapeDTWParallel(refSeries = EURGBP_test_set$EURGBP_test, 
                          learnSeries = EURGBP_test_set, 
                          refSeriesStart = 5500, 
                          shapeDTWParams = SDP_compound, 
                          targetDistance = "s", 
                          distanceType = "D", 
                          normalizationType = "Z", 
                          refSeriesLength = 100, 
                          forecastHorizon = 25, 
                          subsequenceWidth = 4, 
                          trigonometricTP = NULL, 
                          subsequenceBreaks = 1, 
                          includeRefSeries = F, sd_border = 1)

table(ifelse(abs(single_series_test_results_FX_5_min_EURGBP_v2$refSeriesReturn) > 
             single_series_test_results_FX_5_min_EURGBP_v2$refTsSD * sd_bord, 1, 0) *
      sign(single_series_test_results_FX_5_min_EURGBP_v2$refSeriesReturn), 
      ifelse(abs(single_series_test_results_FX_5_min_EURGBP_v2$learnSeriesReturn) > 
               single_series_test_results_FX_5_min_EURGBP_v2$testTsSD * sd_bord, 1, 0) *
      sign(single_series_test_results_FX_5_min_EURGBP_v2$learnSeriesReturn))

# 5 min horyzont 25, inna para walut

nrow(FXtickAgg_Jan2020_d5min$`USDCAD-2020-01`)
plot(FXtickAgg_Jan2020_d5min$`USDCAD-2020-01`, type ="l")

USDCAD_test_set <- list(FXtickAgg_Jan2020_d5min$`USDCAD-2020-01`)
names(USDCAD_test_set) <- "USDCAD_test"

USDCAD_MACD <- MACD(USDCAD_test_set$USDCAD_test[,1])
USDCAD_OBV <- OBV(USDCAD_test_set$USDCAD_test@.Data[,1], USDCAD_test_set$USDCAD_test@.Data[,2])

USDCAD_test_set$USDCAD_test <- cbind(USDCAD_test_set$USDCAD_test,
                                     USDCAD_MACD[,1] / USDCAD_MACD[,2],
                                     USDCAD_OBV)
USDCAD_test_set$USDCAD_test <- na.omit(USDCAD_test_set$USDCAD_test)

single_series_test_results_FX_5_min_USDCAD <- RunMultipleShapeDTWkNN(refSeries = USDCAD_test_set$USDCAD_test, 
                                                                     learnSeries = USDCAD_test_set, 
                                                                     indicesVector = seq(from = 5500, by = 5, length.out = 100), 
                                                                     shapeDTWParams = SDP_compound, 
                                                                     targetDistance = "s", 
                                                                     distanceType = "D", 
                                                                     normalizationType = "Z", 
                                                                     refSeriesLength = 100, 
                                                                     forecastHorizon = 25, 
                                                                     subsequenceBreaks = 1, 
                                                                     includeRefSeries = F, 
                                                                     switchBackToSequential = F)

cor(abs(single_series_test_results_FX_5_min_USDCAD$refSeriesReturn),
    abs(single_series_test_results_FX_5_min_USDCAD$learnSeriesReturn))


classResultsToAccuracyMeasure(classResults_GPW_d5min_100_25, "acc")

plot(classResults_GPW_d5min_100_25$dtw_type_Independent.shape_desc_type_compound.dims1_2_3$refSeriesReturn,
     classResults_GPW_d5min_100_25$dtw_type_Independent.shape_desc_type_simple.dims1_2_3$learnSeriesReturn)
<<<<<<< HEAD
=======

require(Rcpp)
Rcpp::sourceCpp("src/RcppDTWFunctions.cpp")

a <- cumsum(rnorm(10, 1, 1))
b <- cumsum(rnorm(10, 1, 1))
distMat <- proxy::dist(a, b)
RcppAccumulatedCostMatrix(distMat, sakoeChibaWindow = 2)

RcppSimpleDTW(distMat, 4)$Dist
DTW_res <- dtw::dtw(a, b, step.pattern = dtw::symmetric1, window.type = "sa", window.size = 4)
DTW_res$distance

gcm$costMatrix[1:10,1:10]
RcppAccumulatedCostMatrix(distMat, sakoeChibaWindow = 1000)

microbenchmark::microbenchmark(RcppAccumulatedCostMatrix(distMat, sakoeChibaWindow = 2),
                               RcppAccumulatedCostMatrix(distMat, sakoeChibaWindow = 10),
                               RcppAccumulatedCostMatrix(distMat, sakoeChibaWindow = NULL))

test_res <- RknnShapeDTWParallel(refSeries = GPW_tick_agg$CCC,
                                 testSeries = GPW_tick_agg[-1], 
                                 refSeriesStart = 16400, shapeDTWParams = SDP_full, includeRefSeries = T,
                                 targetDistance = "s", subsequenceWidth = 5, forecastHorizon = 50, 
                                 sd_border = 2, refSeriesLength = 150, distanceType = "I")


CCC_test_set <- list(FXtickAgg_Jan2020_d5min$`AUDUSD-2020-01`)
names(CCC_test_set) <- "CCC_test"

CCC_MACD <- MACD(CCC_test_set$CCC_test[,1])
CCC_OBV <- OBV(CCC_test_set$CCC_test@.Data[,1], CCC_test_set$CCC_test@.Data[,2])

CCC_test_set$CCC_test <- cbind(CCC_test_set$CCC_test,
                               CCC_MACD[,1] / CCC_MACD[,2],
                               CCC_OBV)
CCC_test_set$CCC_test <- na.omit(CCC_test_set$CCC_test)

future::plan(future::multiprocess)
test_start <- Sys.time()
test_CCC <- RknnShapeDTWParallel(refSeries = CCC_test_set$CCC_test, 
                                 learnSeries = CCC_test_set, 
                                 refSeriesStart = 5050, 
                                 shapeDTWParams = SDP_compound, 
                                 targetDistance = "r", 
                                 distanceType = "D", 
                                 normalizationType = "U", 
                                 refSeriesLength = 100, 
                                 forecastHorizon = 50, 
                                 subsequenceWidth = 5, 
                                 trigonometricTP = NULL, 
                                 subsequenceBreaks = 1, 
                                 includeRefSeries = F, 
                                 sd_border = 1.5, 
                                 sakoeChibaWindow = 5)
test_stop <- Sys.time()
test_stop-test_start
future::plan(future::sequential)
test_CCC
plot(test_CCC,includeFrcstPart = T, add_wp = T, lift = 0)
plot(test_CCC,includeFrcstPart = T, add_wp = F, lift = 0)

ts1 <- matrix(cumsum(rnorm(100)), ncol = 1)
ts2 <- matrix(cumsum(rnorm(100)), ncol = 1)              

require(Rcpp)
Rcpp::sourceCpp("RcppShapeDTW/src/RcppDTWFunctions.cpp")
RcppShapeDTW::RcppSimpleDTW(proxy::dist(ts1, ts2), sakoeChibaWindow = 2)$Dist
dtw::dtw(ts1, ts2, step.pattern = dtw::symmetric1, window.type = "sak",
         window.size = 2)$distance              


# 5 min horyzont 50, waluty, z-normalizacja, sakoe-chiba

AUDUSD_test_set <- list(FXtickAgg_Jan2020_d5min$`EURGBP-2020-01`)
names(AUDUSD_test_set) <- "AUDUSD_test"

AUDUSD_MACD <- MACD(AUDUSD_test_set$AUDUSD_test[,1])
AUDUSD_OBV <- OBV(AUDUSD_test_set$AUDUSD_test@.Data[,1], AUDUSD_test_set$AUDUSD_test@.Data[,2])

AUDUSD_test_set$AUDUSD_test <- cbind(AUDUSD_test_set$AUDUSD_test,
                                     AUDUSD_MACD[,1] / AUDUSD_MACD[,2],
                                     AUDUSD_OBV)
AUDUSD_test_set$AUDUSD_test <- na.omit(AUDUSD_test_set$AUDUSD_test)

single_series_test_results_FX_5_min_AUDUSD <- RunMultipleShapeDTWkNN(refSeries = AUDUSD_test_set$AUDUSD_test, 
                                                                     learnSeries = AUDUSD_test_set, 
                                                                     indicesVector = seq(from = 2500, by = 5, length.out = 100), 
                                                                     shapeDTWParams = SDP_compound, 
                                                                     targetDistance = "r", 
                                                                     distanceType = "D", 
                                                                     normalizationType = "U", 
                                                                     refSeriesLength = 100, 
                                                                     forecastHorizon = 25, 
                                                                     subsequenceBreaks = 1, 
                                                                     includeRefSeries = F, 
                                                                     switchBackToSequential = F,
                                                                     sakoeChibaWindow = 5)

sum(single_series_test_results_FX_5_min_AUDUSD$kNNSuccess)
cor(single_series_test_results_FX_5_min_AUDUSD$refSeriesReturn,
    single_series_test_results_FX_5_min_AUDUSD$learnSeriesReturn)

table(classResults_GPW_d5min_100_50$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3$kNNSuccess,
      classResults_GPW_d5min_100_50$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3$testTsSD)

classResultsToAccuracyMeasure(classResults_GPW_d5min_100_100, "p", "Fl")


sd_bord <- 2.5

classResults_GPW_d5min_100_100_mod <- purrr::map(classResults_GPW_d5min_100_100, function(x, sdb){
  x <- x %>%
    dplyr::mutate(refReturnClass = 
                    ifelse(x$refSeriesReturn > (x$refTsSD*sdb),
                           "Growth", ifelse(x$refSeriesReturn < (x$refTsSD*-sdb),
                                            "Fall", "Flat_move"))) %>%
    dplyr::mutate(testReturnClass = 
                    ifelse(x$learnSeriesReturn > (x$testTsSD*sdb),
                           "Growth", ifelse(x$learnSeriesReturn < (x$testTsSD*-sdb),
                                            "Fall", "Flat_move"))) %>%
    dplyr::mutate(kNNSuccess = ifelse(refReturnClass == testReturnClass, 1, 0))
}, sdb = sd_bord)

classResults_GPW_d5min_100_25 <- readRDS("Data/Results/RDSFiles/classResults_GPW_d5min_100_25.rds")

classResultsToAccuracyMeasure(classResults_GPW_d5min_100_25, "cor")
classResultsToAccuracyMeasure(classResults_GPW_d5min_100_50, "cor")
classResultsToAccuracyMeasure(classResults_GPW_d5min_100_100_mod, "acc")


classResultsToAccuracyMeasure(classResults_GPW_d10min_100_100, "co", "G")
table(classResults_GPW_d10min_100_100$dtw_type_Dependent.shape_desc_type_simple.dims1$refReturnClass)

plot(classResults_GPW_d10min_100_100$dtw_type_Dependent.shape_desc_type_simple.dims1$refSeriesReturn,
     classResults_GPW_d10min_100_100$dtw_type_Dependent.shape_desc_type_simple.dims1$learnSeriesReturn)


classResultsToAccuracyMeasure(classResults_GPW_d60min_100_100_TTR_1_dim)




input_params_GPW_d60min_100_50_mod <- buildParamsSetFinancialSeries(ts_list = GPW_tick_60min, 
                                                                time_border = timeDate("2015-01-01 00:05:00", format = "%Y-%m-%d %H:%M:%S"), 
                                                                shape_DTW_params = c(SDP_traditional, SDP_compound), 
                                                                trigonometric_transform_params = TTR, 
                                                                subsequenceWidth = 4, 
                                                                learn_part_length = 100, 
                                                                forecast_part_length = 50, 
                                                                learn_set_n = 500, 
                                                                test_set_n = 100)

input_params_GPW_d60min_100_50_mod$params_table <- input_params_GPW_d60min_100_50_mod$params_table %>%
  dplyr::filter(descr == "dtw_type_Independent.shape_desc_type_compound.dims1_2")

classResults_GPW_d60min_100_50_mod <- runShapeDTWForDefinedParamsTable(input_params = input_params_GPW_d60min_100_50_mod, 
                                                                   targetDistance = "raw", 
                                                                   normalizationType = "Z", 
                                                                   sd_border = 1.5)
sum(classResults_GPW_d60min_100_50_mod$dtw_type_Independent.shape_desc_type_compound.dims1_2$kNNSuccess)




### GPW daily wersja 100 / 50 ###

input_params_GPW_daily_100_50_mod <- input_params_GPW_daily_100_50
input_params_GPW_daily_100_50_mod$params_table <- input_params_GPW_daily_100_50_mod$params_table[6,]


classResults_GPW_daily_100_50_mod <- runShapeDTWForDefinedParamsTable(input_params = input_params_GPW_daily_100_50_mod, 
                                                                  targetDistance = "raw", 
                                                                  normalizationType = "Z", 
                                                                  sd_border = 1.5, sakoeChibaWindow = NULL)

sum(classResults_GPW_daily_100_50_mod$dtw_type_Independent.shape_desc_type_simple.dims1_2_3$kNNSuccess)
cor(classResults_GPW_daily_100_50_mod$dtw_type_Independent.shape_desc_type_simple.dims1_2_3$refSeriesReturn,
    classResults_GPW_daily_100_50_mod$dtw_type_Independent.shape_desc_type_simple.dims1_2_3$learnSeriesReturn)
>>>>>>> sakoe_chiba_window
