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
  
  testSeriesIdxWithForecastHorizon <- (dtw_res$bestSubsequenceIdx):testSeriesFrctHorizonLastIdx
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
    nn_name = res_name,
    dtw_results = dtw_res,
    refSeries = refSeriesSubset,
    testSeries = testSeriesSubset,
    refSeriesNorm = refSeriesSubsetNorm,
    testSeriesNorm = testSeriesSubsetNorm,
    validation_results = list(
      distanceType = distanceType,
      refSeriesLength = refSeriesLength,
      forecastHorizon = forecastHorizon,
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

# This function adding warping paths lines to the plot
add_matching_lines <- function(plotToUpdate, warpingMatrix, tsRef, tsTest, col = "red"){
  
  warpingPathLength <- nrow(warpingMatrix)
  
  for(i in 1:warpingPathLength){
    x <- warpingMatrix[i,1]
    xend <- warpingMatrix[i,2]
    y <- tsRef[x]
    yend <- tsTest[xend]
    
    plotToUpdate <- plotToUpdate + 
      geom_segment(x = x, y = y, xend = xend, yend = yend, col = col)
  }
  
  return(plotToUpdate)
}

plot.DTWResults <- function(dtwResults, lift = 1, includeFrcstPart = FALSE, wpCol = "red",
                            add_wp = T){
  
  n_dim <- ncol(test_res$refSeries)
  dim_names <- names(test_res$refSeries)
  data_list <- vector(mode = "list", length = n_dim)
  names(data_list) <- dim_names
  
  for(i in 1:n_dim){
    
    df_names <- c("N", "refSeries", "testSeries")
    
    if(includeFrcstPart){
      data_list[[i]] <- data.frame(1:nrow(test_res$validation_results$refSeriesFullNorm@.Data),
                                   test_res$validation_results$refSeriesFullNorm@.Data[,i] + lift,
                                   test_res$validation_results$testSeriesFullNorm@.Data[,i])
      colnames(data_list[[i]]) <- df_names
    }else{
      data_list[[i]] <- data.frame(1:nrow(test_res$refSeriesNorm@.Data),
                                   test_res$refSeriesNorm@.Data[,i] + lift,
                                   test_res$testSeriesNorm@.Data[,i])
      colnames(data_list[[i]]) <- df_names
    }
  }
  
  data_list_pivoted <- purrr::map(.x = data_list, .f = function(df){
    df <- df %>%
      tidyr::pivot_longer(cols = c("refSeries", "testSeries"), names_to = "ts",
                          values_to = "val")
    return(df)
  })
  
  plot_list <- purrr::imap(.x = data_list_pivoted, .f = function(df, df_name){
    pl <- ggplot(data = df, aes(x = N, y = val ,colour = ts)) +
      geom_line(size = 0.8) +
      ggplot2::ggtitle(df_name)
  })
  
  if(add_wp){
    wp <- test_res$dtw_results$WarpingPaths
    n_wp <- length(wp)
    
    # In case when the Trigonometric transform was added to the DTW algorithm
    if(n_wp > n_dim)
      wp <- wp[1:n_dim]
    
    plot_list <- purrr::pmap(list(pl = plot_list, df = data_list, 
                                wp = wp), 
                           .f = function(pl, df, wp, col){
                             
                             pl <- add_matching_lines(plotToUpdate = pl, 
                                                       warpingMatrix = wp, 
                                                       tsRef = df$refSeries, 
                                                       tsTest = df$testSeries, 
                                                       col = col)
                             
                             return(pl) 
                           }, col = wpCol)
  }
  
  if(includeFrcstPart){
    plot_list <- purrr::pmap(list(pl = plot_list, df = data_list, 
                                     refSeriesLength = test_res$validation_results$refSeriesLength), 
                                .f = function(pl, df, refSeriesLength){
                                  pl <- pl +
                                    geom_vline(xintercept = refSeriesLength,
                                               linetype = "dotted",
                                               color = "blue",
                                               size = 1.5) +
                                    geom_hline(yintercept = df$refSeries[refSeriesLength],
                                               linetype = "dashed",
                                               color = "red") +
                                    geom_hline(yintercept = df$testSeries[refSeriesLength],
                                               linetype = "dashed",
                                               color = "blue")
                                  return(pl)
                                })
  }
  
  grid.arrange(grobs = plot_list, nrow = n_dim)
}


