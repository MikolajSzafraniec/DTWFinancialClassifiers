# Function to conduct normalization through unitarization
Unitarization <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

# Function to conduct normalization through z-score
Zscore <- function(x){
  (x - mean(x)) / sd(x)
}

# Function to validate scale of the growth / fall of prices based on
# the standard deviation of returns
score_return <- function(ts, r, sd_border = 1){
  
  ts_returns <- timeSeries::returns(ts, trim = T, methods = "continuous")
  ts_returns <- ts_returns[is.finite(ts_returns) & !is.na(ts_returns)]
  sd_returns <- sd(ts_returns)
  
  # Breaks for the labels fall / flat movement / growth
  breaks <- c(-Inf, -(sd_returns*sd_border), (sd_returns*sd_border), Inf)
  r_class <- cut(r, breaks = breaks, labels = c("Fall", "Flat_move", "Growth"))
  
  res <- list(
    r_class = r_class,
    ts_sd = sd_returns
  )
  return(res)
}


RknnShapeDTW <- function(refSeries,
                         testSeries,
                         refSeriesStart, #Integer index of ts
                         shapeDTWParams,
                         testSeriesName = "testSeries",
                         distanceType = c("Dependent", "Independent"),
                         normalizationType = c("Unitarization", "Zscore"),
                         refSeriesLength = 100,
                         forecastHorizon = 20,
                         subsequenceWidth = 4,
                         trigonometricTP = NULL,
                         subsequenceBreaks = 10){
  
  msg <- paste0("Proceeding test series: \'", testSeriesName, 
                "\', ref series index: ", refSeriesStart)
  message(msg)
  
  refSeriesStartTime <- time(refSeries)[refSeriesStart]
  refSeriesEndTime <- time(refSeries)[refSeriesStart + refSeriesLength - 1]
  testSeriesEndTime <- time(refSeries)[refSeriesStart - 1]
  
  refSeriesSubset <- window(refSeries, start = refSeriesStartTime, end = refSeriesEndTime)
  testSeriesSubset <- window(testSeries, start = -Inf, end = testSeriesEndTime)
  
  res <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = refSeriesSubset@.Data, 
                                      testSeries = testSeriesSubset@.Data, 
                                      forecastHorizon = forecastHorizon, 
                                      subsequenceWidth = subsequenceWidth, 
                                      subsequenceBreaks = subsequenceBreaks, 
                                      shapeDescriptorParams = shapeDTWParams, 
                                      normalizationType = normalizationType, 
                                      distanceType = distanceType, 
                                      ttParams = trigonometricTP)
  
  return(res)
}

RknnShapeDTWParallel <- function(refSeries,
                                 testSeries, # List of time series
                                 refSeriesStart, #Integer index of ts
                                 shapeDTWParams,
                                 targetDistance = c("raw", "shapeDesc"),
                                 distanceType = c("Dependent", "Independent"),
                                 normalizationType = c("Unitarization", "Zscore"),
                                 refSeriesLength = 100,
                                 forecastHorizon = 20,
                                 subsequenceWidth = 4,
                                 trigonometricTP = NULL,
                                 subsequenceBreaks = 10,
                                 includeRefSeries = TRUE,
                                 sd_border = 1){
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  tsListLen <- length(testSeries)

  if(includeRefSeries){
    testSeries[[tsListLen+1]] <- refSeries
    names(testSeries)[tsListLen+1] <- "refSeries"
  }
  
  message("Changing plan to multiprocess")
  future::plan(future::sequential)
  future::plan(future::multiprocess)
  
  results_set <- future_pmap(
    list(refSeries = list(refSeries),
         testSeries = testSeries,
         n = names(testSeries)),
    ~RknnShapeDTW(refSeries = ..1, testSeries = ..2, 
                  testSeriesName = ..3, refSeriesStart = refSeriesStart, 
                  shapeDTWParams = shapeDTWParams, 
                  refSeriesLength = refSeriesLength, forecastHorizon = forecastHorizon, 
                  subsequenceWidth = subsequenceWidth, trigonometricTP = trigonometricTP, 
                  distanceType = distanceType, subsequenceBreaks = subsequenceBreaks, 
                  normalizationType = normalizationType)
  )
  
  message("Switching plan back to sequential")
  future::plan(future::sequential)
  
  apply_at <- ifelse(targetDistance == "raw", 
                     "RawSeriesDistanceResults",
                     "ShapeDescriptorsDistanceResults")
  
  distances <- map_depth(.x = results_set, .depth = 1, .f = function(x, td, apply_at){
    
    if(targetDistance == "raw"){
      return(x[[apply_at]]$RawDistance)
    }else{
      return(x[[apply_at]]$ShapeDescriptorsDistance)
    }
    
  }, td = targetDistance, apply_at = apply_at)
  
  which_dist_min <- which.min(distances)
  
  dtw_res <- NULL
  if(targetDistance == "raw"){
    dtw_res <- results_set[[which_dist_min]]$RawSeriesDistanceResults
  }else{
    dtw_res <- results_set[[which_dist_min]]$ShapeDescriptorsDistanceResults
  }
  
  res_name <- names(testSeries)[which_dist_min]
  
  fun_to_apply <- ifelse(normalizationType == "Unitarization",
                         Unitarization,
                         Zscore)
  
  nnSeries <- testSeries[[which_dist_min]]
  
  # Defining last indicies of time series subsets
  refSeriesLastIdx <- refSeriesStart+refSeriesLength-1
  testSeriesLastIdx <- dtw_res$bestSubsequenceIdx+refSeriesLength-1
  
  refSeriesFrcstHorizonLastIdx <- refSeriesStart+refSeriesLength+forecastHorizon-1
  testSeriesFrctHorizonLastIdx <- dtw_res$bestSubsequenceIdx+refSeriesLength+forecastHorizon-1
  
  # Retrieving subseries from original series
  refSeriesIdx <- refSeriesStart:refSeriesLastIdx
  #refSeriesSubset <- refSeries@.Data[refSeriesIdx,]
  refSeriesSubset <- refSeries[refSeriesIdx,]
  
  testSeriesIdx <- (dtw_res$bestSubsequenceIdx):(testSeriesLastIdx)
  #testSeriesSubset <- nnSeries@.Data[testSeriesIdx,]
  testSeriesSubset <- nnSeries[testSeriesIdx,]
  
  # Retrieving validation results
  refSeriesIdxWithForecastHorizon <- refSeriesStart:refSeriesFrcstHorizonLastIdx
  refSeriesSubsetWithFrcstHorizon <- refSeries[refSeriesIdxWithForecastHorizon,]
  
  testSeriesIdxWithForecastHorizon <- (dtw_res$bestSubsequenceIdx):refSeriesFrcstHorizonLastIdx
  testSeriesSubsetWithForecastHorizon <- nnSeries[testSeriesIdxWithForecastHorizon,]
  
  refSeriesReturn <- log(refSeries@.Data[refSeriesFrcstHorizonLastIdx,1] / 
                           refSeries@.Data[refSeriesLastIdx,1])
  testSeriesReturn <- log(nnSeries@.Data[testSeriesFrctHorizonLastIdx,1]/
                           nnSeries@.Data[testSeriesLastIdx,1])
  
  refValResults <- score_return(ts = refSeriesSubset[,1], r = refSeriesReturn, sd_border = sd_border)
  testValResults <- score_return(ts = testSeriesSubset[,1], r = testSeriesReturn, sd_border = sd_border)
  
  refSeriesSubsetNorm <- apply(refSeriesSubset, 2, fun_to_apply)
  testSeriesSubsetNorm <- apply(testSeriesSubset, 2, fun_to_apply)
  refSeriesSubsetWithFrcstHorizonNorm <- apply(refSeriesSubsetWithFrcstHorizon, 2, fun_to_apply)
  testSeriesSubsetWithForecastHorizonNorm <- apply(testSeriesSubsetWithForecastHorizon, 2, fun_to_apply)
  
  final_results <- list(
    dtw_results = dtw_res,
    refSeries = refSeriesSubset,
    testSeries = testSeriesSubset,
    refSeriesNorm = refSeriesSubsetNorm,
    testSeriesNorm = testSeriesSubsetNorm,
    validation_results = list(
      refSeriesFull = refSeriesSubsetWithFrcstHorizon,
      testSeriesFull = testSeriesSubsetWithForecastHorizon,
      refSeriesFullNorm = refSeriesSubsetWithFrcstHorizonNorm,
      testSeriesFullNorm = testSeriesSubsetWithForecastHorizonNorm,
      refSeriesReturn = refSeriesReturn,
      testSeriesReturn = testSeriesReturn,
      refReturnClass = refValResults$r_class,
      testReturnClass = testValResults$r_class,
      refTsSD = refValResults$ts_sd,
      testTsSD = testValResults$ts_sd,
      kNNSuccess = ifelse(refValResults$r_class==testValResults$r_class, 1, 0)
    )
  )
  
  class(final_results) <- "DTWResults"
  
  return(final_results)
}


refSeries <- FXtickAgg[100000:110000,]
testSeries <- FXtickAgg[1:50000,]

SDP <- new("ShapeDescriptorParams", Descriptors = "slopeDescriptor",
           "Additional_params" = list("slopeWindow" = 4L))


test_res <- RknnShapeDTWParallel(refSeries = refSeries,
                                 testSeries = list(testSeries = refSeries), 
                                 refSeriesStart = 5000, shapeDTWParams = SDP, includeRefSeries = F,
                                 targetDistance = "sh", subsequenceWidth = 5, forecastHorizon = 50, 
                                 sd_border = 3)

test_res

par(mfrow = c(2, 1))
plot(test_res$refSeriesNorm@.Data[,1], type = "l")
lines(test_res$testSeriesNorm@.Data[,1], col = "red")

plot(test_res$refSeriesNorm@.Data[,2], type = "l")
lines(test_res$testSeriesNorm@.Data[,2], col = "red")


par(mfrow = c(2, 1))
plot(test_res$validation_results$refSeriesFullNorm@.Data[,1], type = "l")
lines(test_res$validation_results$testSeriesFullNorm@.Data[,1], col = "red")

plot(test_res$validation_results$refSeriesFullNorm@.Data[,2], type = "l")
lines(test_res$validation_results$testSeriesFullNorm@.Data[,2], col = "red")
