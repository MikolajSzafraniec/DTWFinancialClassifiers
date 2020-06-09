RknnEuclidean <- function(refSeries,
                          learnSeries,
                          refSeriesStart, #Integer index of ts
                          knn = 500,
                          normalizationType = c("Unitarization", "Zscore"),
                          refSeriesLength = 100,
                          forecastHorizon = 100,
                          subsequenceBreaks = 1){
  
  normalizationType <- match.arg(normalizationType)
  
  refSeriesStartTime <- time(refSeries)[refSeriesStart]
  refSeriesEndTime <- time(refSeries)[refSeriesStart + refSeriesLength - 1]
  learnSeriesEndTime <- time(refSeries)[refSeriesStart]
  
  refSeriesSubset <- window(refSeries, start = refSeriesStartTime, end = refSeriesEndTime)
  learnSeriesSubset <- window(learnSeries, start = -Inf, end = learnSeriesEndTime)
  
  res <- RcppShapeDTW::knnEuclideanCpp(refSeries = refSeriesSubset@.Data, 
                                       testSeries = learnSeriesSubset@.Data, 
                                       nn = knn, 
                                       forecastHorizon = forecastHorizon, 
                                       subsequenceBreaks = subsequenceBreaks, 
                                       normalizationType = normalizationType)
  
  return(res)
}

RknnEuclideanMultipleSeries <- function(refSeries,
                                        learnSeries, # List of time series
                                        refSeriesStart, #Integer index of ts
                                        knn = 500,
                                        normalizationType = c("Unitarization", "Zscore"),
                                        refSeriesLength = 100,
                                        forecastHorizon = 100,
                                        subsequenceBreaks = 1,
                                        includeRefSeries = T){
  
  normalizationType <- match.arg(normalizationType)
  tsListLen <- length(learnSeries)
  
  if(includeRefSeries){
    learnSeries[[tsListLen+1]] <- refSeries
    names(learnSeries)[tsListLen+1] <- "refSeries"
  }
  
  if(class(future::plan())[2] == "sequential"){
    message("Changing plan to multiprocess")
    future::plan(future::multiprocess)
  }
  
  results_set <- furrr::future_pmap(
    list(refSeries = list(refSeries),
         learnSeries = learnSeries),
    ~RknnEuclidean(refSeries = ..1, learnSeries = ..2, 
                   refSeriesStart = refSeriesStart, knn = knn,
                   normalizationType = normalizationType,
                   refSeriesLength = refSeriesLength,
                   forecastHorizon = forecastHorizon,
                   subsequenceBreaks = subsequenceBreaks)
  )
  
  results_set_indexed <- purrr::imap(results_set, function(res_table, ind){
    res <- cbind(res_table, ts_ind = ind)
    return(res)
  })
  
  results_set_indexed <- do.call(what = rbind, args = results_set_indexed)
  
  order_dists <- order(results_set_indexed[,"Distance"], decreasing = F)
  results_set_final <- results_set_indexed[order_dists,]
  
  if(nrow(results_set_final) <= knn){
    return(results_set_final)
  }else{
    return(results_set_final[1:knn,])
  }
}

retrieveLearningSetFromList <- function(learnSeriesList,
                                        knnMatrix,
                                        refSeriesLength,
                                        maxForecastHorizon){
  
  indices <- 1:nrow(knnMatrix)
  
  res <- purrr::map(indices, function(idx,
                                      .knnMatrix, 
                                      .learnSeriesList, 
                                      .refSeriesLength, 
                                      .maxForecastHorizon){
    
    knn_matrix_row <- .knnMatrix[idx,]
    
    series_idx <- knn_matrix_row["ts_ind"]
    first_row_idx <- knn_matrix_row["Idx"]
    last_row_idx <- first_row_idx + .refSeriesLength + .maxForecastHorizon - 1
    subset <- .learnSeriesList[[series_idx]][first_row_idx:last_row_idx,]
    return(subset)
    
  },
  .knnMatrix = knnMatrix,
  .learnSeriesList = learnSeriesList, 
  .refSeriesLength = refSeriesLength, 
  .maxForecastHorizon = maxForecastHorizon)
  
  return(res)
}


RknnShapeDTWParallelSimplified <- function(refSeries,
                                           learnSeriesList,
                                           refSeriesStart,
                                           shapeDTWParams,
                                           targetDistance = c("raw", "shapeDesc"),
                                           distanceType = c("Dependent", "Independent"),
                                           normalizationType = c("Unitarization", "Zscore"),
                                           refSeriesLength = 100,
                                           forecastHorizons = c(25, 50, 100),
                                           subsequenceWidth = 4,
                                           trigonometricTP = NULL,
                                           subsequenceBreaks = 1,
                                           includeRefSeries = F,
                                           sakoeChibaWindow = NULL){
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  tsListLen <- length(learnSeries)
  
  if(includeRefSeries){
    learnSeries[[tsListLen+1]] <- refSeries
  }
  
  if(class(future::plan())[2] == "sequential"){
    message("Changing plan to multiprocess")
    future::plan(future::multiprocess)
  }
  
  max_frcst_horizon = max(forecastHorizons)
  
  results_set <- furrr::future_pmap(
    list(refSeries = list(refSeries),
         learnSeries = learnSeries),
    ~RknnShapeDTW(refSeries = ..1, learnSeries = ..2, 
                  refSeriesStart = refSeriesStart, 
                  shapeDTWParams = shapeDTWParams, 
                  refSeriesLength = refSeriesLength, forecastHorizon = max_frcst_horizon, 
                  subsequenceWidth = subsequenceWidth, trigonometricTP = trigonometricTP, 
                  distanceType = distanceType, subsequenceBreaks = subsequenceBreaks, 
                  normalizationType = normalizationType,
                  sakoeChibaWindow = sakoeChibaWindow)
  )
  
  apply_at <- ifelse(targetDistance == "raw", 
                     "RawSeriesDistanceResults",
                     "ShapeDescriptorsDistanceResults")
  
  distances <- map_depth(.x = results_set, .depth = 1, .f = function(x, td, apply_at){
    
    if(td == "raw"){
      return(x[[apply_at]]$RawDistance)
    }else{
      return(x[[apply_at]]$ShapeDescriptorsDistance)
    }
    
  }, td = targetDistance, apply_at = apply_at)
  
  which_dist_min <- which.min(distances)
}


RknnShapeDTWParallel <- function(refSeries,
                                 learnSeries, # List of time series
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
                                 sd_border = 1,
                                 sakoeChibaWindow = NULL){
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  tsListLen <- length(learnSeries)
  
  if(includeRefSeries){
    learnSeries[[tsListLen+1]] <- refSeries
    names(learnSeries)[tsListLen+1] <- "refSeries"
  }
  
  if(class(future::plan())[2] == "sequential"){
    message("Changing plan to multiprocess")
    future::plan(future::multiprocess)
  }
  
  results_set <- furrr::future_pmap(
    list(refSeries = list(refSeries),
         learnSeries = learnSeries),
    ~RknnShapeDTW(refSeries = ..1, learnSeries = ..2, 
                  refSeriesStart = refSeriesStart, 
                  shapeDTWParams = shapeDTWParams, 
                  refSeriesLength = refSeriesLength, forecastHorizon = forecastHorizon, 
                  subsequenceWidth = subsequenceWidth, trigonometricTP = trigonometricTP, 
                  distanceType = distanceType, subsequenceBreaks = subsequenceBreaks, 
                  normalizationType = normalizationType,
                  sakoeChibaWindow = sakoeChibaWindow)
  )
  
  #message("Switching plan back to sequential")
  #future::plan(future::sequential)
  
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
  
  res_name <- names(learnSeries)[which_dist_min]
  
  fun_to_apply <- ifelse(normalizationType == "Unitarization",
                         Unitarization,
                         Zscore)
  
  nnSeries <- learnSeries[[which_dist_min]]
  
  # Defining last indicies of time series subsets
  refSeriesLastIdx <- refSeriesStart+refSeriesLength-1
  learnSeriesLastIdx <- dtw_res$bestSubsequenceIdx+refSeriesLength-1
  
  refSeriesFrcstHorizonLastIdx <- refSeriesStart+refSeriesLength+forecastHorizon-1
  learnSeriesFrctHorizonLastIdx <- dtw_res$bestSubsequenceIdx+refSeriesLength+forecastHorizon-1
  
  # Retrieving subseries from original series
  refSeriesIdx <- refSeriesStart:refSeriesLastIdx
  #refSeriesSubset <- refSeries@.Data[refSeriesIdx,]
  refSeriesSubset <- refSeries[refSeriesIdx,]
  
  learnSeriesIdx <- (dtw_res$bestSubsequenceIdx):(learnSeriesLastIdx)
  #learnSeriesSubset <- nnSeries@.Data[learnSeriesIdx,]
  learnSeriesSubset <- nnSeries[learnSeriesIdx,]
  
  # Retrieving validation results
  refSeriesIdxWithForecastHorizon <- refSeriesStart:refSeriesFrcstHorizonLastIdx
  refSeriesSubsetWithFrcstHorizon <- refSeries[refSeriesIdxWithForecastHorizon,]
  
  learnSeriesIdxWithForecastHorizon <- (dtw_res$bestSubsequenceIdx):learnSeriesFrctHorizonLastIdx
  learnSeriesSubsetWithForecastHorizon <- nnSeries[learnSeriesIdxWithForecastHorizon,]
  
  refSeriesReturn <- log(refSeries@.Data[refSeriesFrcstHorizonLastIdx,1] / 
                           refSeries@.Data[refSeriesLastIdx,1])
  learnSeriesReturn <- log(nnSeries@.Data[learnSeriesFrctHorizonLastIdx,1]/
                             nnSeries@.Data[learnSeriesLastIdx,1])
  
  refValResults <- score_return(ts = refSeriesSubset[,1], r = refSeriesReturn, sd_border = sd_border)
  testValResults <- score_return(ts = learnSeriesSubset[,1], r = learnSeriesReturn, sd_border = sd_border)
  
  refSeriesSubsetNorm <- apply(refSeriesSubset, 2, fun_to_apply)
  learnSeriesSubsetNorm <- apply(learnSeriesSubset, 2, fun_to_apply)
  refSeriesSubsetWithFrcstHorizonNorm <- apply(refSeriesSubsetWithFrcstHorizon, 2, fun_to_apply)
  learnSeriesSubsetWithForecastHorizonNorm <- apply(learnSeriesSubsetWithForecastHorizon, 2, fun_to_apply)
  
  final_results <- list(
    nn_name = res_name,
    dtw_results = dtw_res,
    refSeries = refSeriesSubset,
    learnSeries = learnSeriesSubset,
    refSeriesNorm = refSeriesSubsetNorm,
    learnSeriesNorm = learnSeriesSubsetNorm,
    validation_results = list(
      distanceType = distanceType,
      refSeriesLength = refSeriesLength,
      forecastHorizon = forecastHorizon,
      refSeriesFull = refSeriesSubsetWithFrcstHorizon,
      learnSeriesFull = learnSeriesSubsetWithForecastHorizon,
      refSeriesFullNorm = refSeriesSubsetWithFrcstHorizonNorm,
      learnSeriesFullNorm = learnSeriesSubsetWithForecastHorizonNorm,
      refSeriesReturn = refSeriesReturn,
      learnSeriesReturn = learnSeriesReturn,
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
