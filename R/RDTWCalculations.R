Unitarization <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

Zscore <- function(x){
  (x - mean(x)) / sd(x)
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
  
  res <- kNNShapeDTWCpp(referenceSeries = refSeriesSubset@.Data, 
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
                                 includeRefSeries = TRUE){
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  tsListLen <- length(testSeries)
  
  if(includeRefSeries){
    testSeries[[tsListLen+1]] <- refSeries
    names(testSeries)[tsListLen+1] <- "refSeries"
  }
  
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
  
  future::plan(future::sequential)
  
  apply_at <- ifelse(targetDistance == "raw", 
                     "RawSeriesDistanceResults",
                     "ShapeDescriptorsDistanceResults")
  
  distances <- map_at(.x = results_set, .at = apply_at, .f = function(x, td){
    
    if(targetDistance == "raw"){
      return(x$RawDistance)
    }else{
      return(x$ShapeDescriptorsDistance)
    }
    
  }, td = targetDistance)
  
  which_dist_min <- which.min(distances)
  
  dtw_res <- ifelse(targetDistance == "raw",
                    results_set[[which_dist_min]]$RawSeriesDistanceResults,
                    results_set[[which_dist_min]]$ShapeDescriptorsDistanceResults)
  
  res_name <- names(testSeries)[which_dist_min]
  
  fun_to_apply <- ifelse(normalizationType == "Unitarization",
                         Unitarization,
                         Zscore)
  
  refSeriesIdx <- refSeriesStart:(refSeriesStart+refSeriesLength-1)
  refSeriesSubset <- refSeries@.Data[refSeriesIdx,]
  refSeriesSubset <- apply(refSeriesSubset, 2, fun_to_apply)
  
  testSeriesIdx <- dtw_res$bestSubsequenceIdx:(bestSubsequenceIdx+refSeriesLength-1)
  testSeriesSubset <- testSeries[[which_dist_min]]@.Data[testSeriesIdx,]
  testSeriesSubset <- apply(testSeriesSubset, 2, fun_to_apply)
  
  final_results <- list(
    dtw_results = dtw_res,
    refSeries = refSeriesSubset,
    testSeries = testSeriesSubset
  )
  
  class(final_results) <- "DTWResults"
  
  return(final_results)
}

daty <- seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 1000)
data1 <- matrix(cumsum(rnorm(2000, 0, 1)), ncol = 2)
data2 <- matrix(cumsum(rnorm(2000, 0, 1)), ncol = 2)

tsTest_1 <- timeSeries(data = data1, charvec = daty)
tsTest_2 <- timeSeries(data = data2, charvec = daty)

testRes <- RknnShapeDTWParallel(refSeries = tsTest_1, testSeries = list(tsTest = tsTest_2), 
                                refSeriesStart = 500, shapeDTWParams = SDP)

f1 <- function(x, y){x + y}
future_pmap(.l = list(x = 1, y = c(1, 2)), ~f1(..1, ..2))



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

require(parallel)
n_cores <- parallel::detectCores()
cl <- makeCluster(n_cores)

clusterCall(cl = cl, fun = function(x){
  require(Rcpp)
  Rcpp::sourceCpp("src/RcppDTWFunctions.cpp")
})
clusterExport(cl = cl, varlist = c("refSeriesStart", #Integer index of ts
                                   "shapeDTWParams",
                                   "targetDistance",
                                   "distanceType",
                                   "normalizationType",
                                   "refSeriesLength",
                                   "forecastHorizon",
                                   "subsequenceWidth",
                                   "trigonometricTP",
                                   "subsequenceBreaks",
                                   "includeRefSeries",
                                   "RknnShapeDTW"))

results_set <- parLapply(cl = cl, X = testSeries, fun = RknnShapeDTW, 
                refSeries = refSeries,
                refSeriesStart = refSeriesStart, 
                shapeDTWParams = shapeDTWParams, 
                refSeriesLength = refSeriesLength, forecastHorizon = forecastHorizon, 
                subsequenceWidth = subsequenceWidth, trigonometricTP = trigonometricTP, 
                distanceType = distanceType, subsequenceBreaks = subsequenceBreaks, 
                normalizationType = normalizationType)

