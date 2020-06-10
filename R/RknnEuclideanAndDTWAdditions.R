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
                                        subsequenceBreaks = 1){
  
  normalizationType <- match.arg(normalizationType)
  tsListLen <- length(learnSeries)
  
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

calcReturn <- function(target_series,
                       idx_begin,
                       target_series_length,
                       forecast_horizon,
                       return_type = c("natural", "log")){
  
  return_type <- match.arg(return_type)
  
  last_actual_val <- target_series[idx_begin + target_series_length - 1]
  last_frcst_val <- target_series[idx_begin + target_series_length + forecast_horizon - 1]
  res <- switch(
    return_type,
    natural = (last_frcst_val - last_actual_val) / last_actual_val,
    log = log(last_frcst_val/last_actual_val)
  )
  return(res)
}

calcSD <- function(target_series,
                   idx_begin, 
                   target_series_lenght,
                   return_type = c( "simple", "continuous")){
  
  return_type <- match.arg(return_type)
  
  subset_series <- target_series[idx_begin:(idx_begin+target_series_lenght-1)]
  series_returns <- timeSeries::returns(
    subset_series,
    method = return_type,
    trim = TRUE,
    na.rm = TRUE
  )
  series_returns <- na.omit(series_returns)
  
  return(sd(series_returns))
}

assignReturnClass <- function(rt, sd_border, sd){
  res <- cut.default(rt,
              breaks = c(-Inf, -sd_border*sd, sd_border*sd, Inf), 
              labels = c("Sell", "Hold", "Buy"))
  return(as.character(res))
}

RknnShapeDTWParallelSimplified <- function(refSeries,
                                           learnSeriesList,
                                           refSeriesStart,
                                           shapeDTWParams,
                                           targetDistance = c("raw", "shapeDesc"),
                                           distanceType = c("Dependent", "Independent"),
                                           normalizationType = c("Unitarization", "Zscore"),
                                           knn = 100,
                                           refSeriesLength = 100,
                                           forecastHorizons = c(25, 50, 100),
                                           sd_borders = c(1, 1.5, 2),
                                           subsequenceWidth = 4,
                                           trigonometricTP = NULL,
                                           subsequenceBreaks = 1,
                                           subsequenceBreaksknnEuclid = 1,
                                           includeRefSeries = F,
                                           sakoeChibaWindow = NULL){
  
  stopifnot(length(forecastHorizons) == length(sd_borders))
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  tsListLen <- length(learnSeriesList)
  
  if(includeRefSeries){
    learnSeriesList[[tsListLen+1]] <- refSeries
  }
  
  max_frcst_horizon = max(forecastHorizons)
  
  knnEuclideanMatrix <- RknnEuclideanMultipleSeries(refSeries = refSeries, 
                                                    learnSeries = learnSeriesList, 
                                                    refSeriesStart = refSeriesStart, 
                                                    knn = knn, 
                                                    normalizationType = normalizationType, 
                                                    refSeriesLength = refSeriesLength, 
                                                    forecastHorizon = max_frcst_horizon, 
                                                    subsequenceBreaks = subsequenceBreaksknnEuclid)
  
  learningSetRetrieved <- retrieveLearningSetFromList(learnSeriesList = learnSeriesList, 
                                                      knnMatrix = knnEuclideanMatrix, 
                                                      refSeriesLength = refSeriesLength, 
                                                      maxForecastHorizon = max_frcst_horizon)
  
  if(class(future::plan())[2] == "sequential"){
    message("Changing plan to multiprocess")
    future::plan(future::multiprocess)
  }
  
  results_set <- furrr::future_pmap(
    list(refSeries = list(refSeries),
         learnSeriesList = learningSetRetrieved),
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
  
  dtw_res <- NULL
  if(targetDistance == "raw"){
    dtw_res <- results_set[[which_dist_min]]$RawSeriesDistanceResults
  }else{
    dtw_res <- results_set[[which_dist_min]]$ShapeDescriptorsDistanceResults
  }
  
  nnSeries <- learningSetRetrieved[[which_dist_min]]
  nnEuclidean <- learningSetRetrieved[[1]]
  
  target_series_returns <- purrr::map_dbl(forecastHorizons, 
                                      ~calcReturn(target_series = refSeries, 
                                                  idx_begin = refSeriesStart, 
                                                  target_series_length = refSeriesLength, 
                                                  forecast_horizon = .x, 
                                                  return_type = "n"))
  names(target_series_returns) <- paste0("target_series_", forecastHorizons, "_returns")
  
  learn_series_returns <- purrr::map_dbl(forecastHorizons, 
                                     ~calcReturn(target_series = nnSeries, 
                                                 idx_begin = dtw_res$bestSubsequenceIdx, 
                                                 target_series_length = refSeriesLength, 
                                                 forecast_horizon = .x, 
                                                 return_type = "n"))
  names(learn_series_returns) <- paste0("learn_series_", forecastHorizons, "_returns")
  
  euclid_series_returns <- purrr::map_dbl(forecastHorizons, 
                                         ~calcReturn(target_series = nnEuclidean, 
                                                     idx_begin = 1, 
                                                     target_series_length = refSeriesLength, 
                                                     forecast_horizon = .x, 
                                                     return_type = "n"))
  names(euclid_series_returns) <- paste0("euclid_series_", forecastHorizons, "_returns")
  
  target_series_sd <- calcSD(target_series = refSeries, 
                             idx_begin = refSeriesStart,
                             target_series_lenght = refSeriesLength, 
                             return_type = "s")
  
  learn_series_sd <- calcSD(target_series = nnSeries, 
                            idx_begin = dtw_res$bestSubsequenceIdx,
                            target_series_lenght = refSeriesLength, 
                            return_type = "s")
  
  euclid_series_sd <- calcSD(target_series = nnEuclidean, 
                             idx_begin = 1,
                             target_series_lenght = refSeriesLength, 
                             return_type = "s")
  
  target_series_classes <- purrr::pmap_chr(.l = list(target_series_returns,
                                                     sd_borders,
                                                     list(target_series_sd)), 
                                           ~assignReturnClass(rt = ..1, 
                                                              sd_border = ..2, 
                                                              sd = ..3))
  names(target_series_classes) <- paste0("target_series_", forecastHorizons, "_return_class")
  
  learn_series_classes <- purrr::pmap_chr(.l = list(learn_series_returns,
                                                     sd_borders,
                                                     list(learn_series_sd)), 
                                           ~assignReturnClass(rt = ..1, 
                                                              sd_border = ..2, 
                                                              sd = ..3))
  names(learn_series_classes) <- paste0("learn_series_", forecastHorizons, "_return_class")
  
  euclid_series_classes <- purrr::pmap_chr(.l = list(euclid_series_returns,
                                                    sd_borders,
                                                    list(euclid_series_sd)), 
                                          ~assignReturnClass(rt = ..1, 
                                                             sd_border = ..2, 
                                                             sd = ..3))
  names(euclid_series_classes) <- paste0("euclid_series_", forecastHorizons, "_return_class")
  
  res <- c(as.list(c(target_series_returns,
                     learn_series_returns,
                     euclid_series_returns)),
           as.list(c(target_series_classes,
                     learn_series_classes,
                     euclid_series_classes)),
           target_series_sd = target_series_sd,
           learn_series_sd = learn_series_sd,
           euclid_series_sd = euclid_series_sd,
           dtw_res = list(dtw_res),
           best_series_ind = knnEuclideanMatrix[which_dist_min,],
           best_euclid = knnEuclideanMatrix[1,])
  
  return(res)
}


