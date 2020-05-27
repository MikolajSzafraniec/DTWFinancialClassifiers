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
                         learnSeries,
                         refSeriesStart, #Integer index of ts
                         shapeDTWParams,
                         learnSeriesName = "learnSeries",
                         distanceType = c("Dependent", "Independent"),
                         normalizationType = c("Unitarization", "Zscore"),
                         refSeriesLength = 100,
                         forecastHorizon = 20,
                         subsequenceWidth = 4,
                         trigonometricTP = NULL,
                         subsequenceBreaks = 10){

  
  refSeriesStartTime <- time(refSeries)[refSeriesStart]
  refSeriesEndTime <- time(refSeries)[refSeriesStart + refSeriesLength - 1]
  learnSeriesEndTime <- time(refSeries)[refSeriesStart]
  
  refSeriesSubset <- window(refSeries, start = refSeriesStartTime, end = refSeriesEndTime)
  learnSeriesSubset <- window(learnSeries, start = -Inf, end = learnSeriesEndTime)
  
  res <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = refSeriesSubset@.Data, 
                                      testSeries = learnSeriesSubset@.Data, 
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
                                 sd_border = 1){
  
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
         learnSeries = learnSeries,
         n = names(learnSeries)),
    ~RknnShapeDTW(refSeries = ..1, learnSeries = ..2, 
                  learnSeriesName = ..3, refSeriesStart = refSeriesStart, 
                  shapeDTWParams = shapeDTWParams, 
                  refSeriesLength = refSeriesLength, forecastHorizon = forecastHorizon, 
                  subsequenceWidth = subsequenceWidth, trigonometricTP = trigonometricTP, 
                  distanceType = distanceType, subsequenceBreaks = subsequenceBreaks, 
                  normalizationType = normalizationType)
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
  
  n_dim <- ncol(dtwResults$refSeries)
  dim_names <- names(dtwResults$refSeries)
  data_list <- vector(mode = "list", length = n_dim)
  names(data_list) <- dim_names
  
  for(i in 1:n_dim){
    
    df_names <- c("N", "refSeries", "learnSeries")
    
    if(includeFrcstPart){
      data_list[[i]] <- data.frame(1:nrow(dtwResults$validation_results$refSeriesFullNorm@.Data),
                                   dtwResults$validation_results$refSeriesFullNorm@.Data[,i] + lift,
                                   dtwResults$validation_results$learnSeriesFullNorm@.Data[,i])
      colnames(data_list[[i]]) <- df_names
    }else{
      data_list[[i]] <- data.frame(1:nrow(dtwResults$refSeriesNorm@.Data),
                                   dtwResults$refSeriesNorm@.Data[,i] + lift,
                                   dtwResults$learnSeriesNorm@.Data[,i])
      colnames(data_list[[i]]) <- df_names
    }
  }
  
  data_list_pivoted <- purrr::map(.x = data_list, .f = function(df){
    df <- df %>%
      tidyr::pivot_longer(cols = c("refSeries", "learnSeries"), names_to = "ts",
                          values_to = "val")
    return(df)
  })
  
  plot_list <- purrr::imap(.x = data_list_pivoted, .f = function(df, df_name){
    pl <- ggplot(data = df, aes(x = N, y = val ,colour = ts)) +
      geom_line(size = 0.8) +
      ggplot2::ggtitle(df_name)
  })
  
  if(add_wp){
    wp <- dtwResults$dtw_results$WarpingPaths
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
                                                       tsTest = df$learnSeries, 
                                                       col = col)
                             
                             return(pl) 
                           }, col = wpCol)
  }
  
  if(includeFrcstPart){
    plot_list <- purrr::pmap(list(pl = plot_list, df = data_list, 
                                     refSeriesLength = dtwResults$validation_results$refSeriesLength), 
                                .f = function(pl, df, refSeriesLength){
                                  pl <- pl +
                                    geom_vline(xintercept = refSeriesLength,
                                               linetype = "dotted",
                                               color = "blue",
                                               size = 1.5) +
                                    geom_hline(yintercept = df$refSeries[refSeriesLength],
                                               linetype = "dashed",
                                               color = "red") +
                                    geom_hline(yintercept = df$learnSeries[refSeriesLength],
                                               linetype = "dashed",
                                               color = "blue")
                                  return(pl)
                                })
  }
  
  grid.arrange(grobs = plot_list, nrow = n_dim)
}

get_current_timestamp <- function(format = "%Y_%m_%d_%H_%M_%S", tz = ""){
  ts <- strftime(Sys.time(), format = format, tz = tz)
  ts
}


# This function runs 1NN shape DTW for multiple starting points of reference
# series and write results to the data frame
RunMultipleShapeDTWkNN <- function(refSeries,
                                   learnSeries,
                                   indicesVector,
                                   shapeDTWParams,
                                   logPath = "Logs",
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
                                   loggingThreshold = "DEBUG",
                                   switchBackToSequential = T){
  
  stopifnot(!is.null(names(learnSeries)))
  
  # Matching arguments with multiple possible values
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  firstIndex <- indicesVector[1]
  refSeriesTimeBeggining <- time(refSeries)[firstIndex]
  
  availableRecords <- purrr::map_lgl(.x = learnSeries, function(ts, timeBegin, recordBorder){
    
    ar <- nrow(window(ts, start = -Inf, end = timeBegin))
    res <- ifelse(ar < recordBorder, F, T)
    return(res)
    
  }, timeBegin = refSeriesTimeBeggining, recordBorder = refSeriesLength + forecastHorizon)
  
  # Filtering set of test series
  learnSeries <- learnSeries[availableRecords]
  
  # Testing if reference series should really be included too
  if(includeRefSeries){
    refSeriesAvailabilty <- nrow(window(refSeries, start = -Inf, end = refSeriesTimeBeggining))
    if(refSeriesAvailabilty < (refSeriesLength + forecastHorizon))
      includeRefSeries <- FALSE
  }
  
  time_stamp <- get_current_timestamp()
  log_file <- log4r::file_appender(file = paste0(logPath, "/run_", time_stamp, ".log"))
  current_logger <- log4r::logger(threshold = loggingThreshold,
                                  appenders = list(log_file, 
                                                   log4r::console_appender()))
  
  #message("Switching plan to multiprocess")
  #future::plan(future::sequential)
  #future::plan(future::multiprocess)
  
  tryCatch(
    withCallingHandlers(
      {
        if(class(future::plan())[2] == "sequential"){
          message("Switching plan to multiprocess.")
          future::plan(future::multiprocess)
        }
        
        res <- purrr::map_dfr(.x = indicesVector, .f = function(idx){
          message(paste0("Processing data for part of reference series beggining with index: ", idx))
          
          kNNResults <- RknnShapeDTWParallel(refSeries = refSeries, 
                                             learnSeries = learnSeries, 
                                             refSeriesStart = idx,
                                             shapeDTWParams = shapeDTWParams, 
                                             targetDistance = targetDistance, 
                                             distanceType = distanceType, 
                                             normalizationType = normalizationType, 
                                             refSeriesLength = refSeriesLength, 
                                             forecastHorizon = forecastHorizon, 
                                             subsequenceWidth = subsequenceWidth, 
                                             trigonometricTP = trigonometricTP, 
                                             subsequenceBreaks = subsequenceBreaks, 
                                             includeRefSeries = includeRefSeries, 
                                             sd_border = sd_border)
          
          res <- data.frame(
            "kNNSuccess" = kNNResults$validation_results$kNNSuccess,
            "refReturnClass" = as.character(kNNResults$validation_results$refReturnClass),
            "testReturnClass" = as.character(kNNResults$validation_results$testReturnClass),
            "refSeriesReturn" = kNNResults$validation_results$refSeriesReturn,
            "learnSeriesReturn" = kNNResults$validation_results$learnSeriesReturn,
            "refTsSD" = kNNResults$validation_results$refTsSD,
            "testTsSD" = kNNResults$validation_results$testTsSD,
            stringsAsFactors = F
          )
        
          return(res)
        })
      
      if(switchBackToSequential){
        message("Switching plan to sequential.")
        future::plan(future::sequential)  
      }
      
      }, 
      error = function(e){
        log4r::error(current_logger, e)
      }, 
      warning = function(w){
        log4r::warn(current_logger, w)
      },
      message = function(m){
        log4r::info(current_logger, m$message)
      }
    ),
    error = function(e){
      msg <- paste0(time_stamp, "failed:\n", e)
      stop(msg)
    }
  )
  
  return(res)
}

sampleRandomTestLearnSets <- function(ts_list,
                                      time_border,
                                      learn_part_length = 100,
                                      forecast_part_length = 50,
                                      learn_set_n = 100,
                                      test_set_n = 100){
  
  ts_list_n <- length(ts_list)
  
  learning_part_to_choose <- purrr::map(ts_list, 
                                        .f = function(x_ts){
                                          res <- window(x_ts, start = -Inf, end = time_border)
                                          return(res)
                                        })
  
  testing_part_to_choose <- purrr::map(ts_list, 
                                       .f = function(x_ts){
                                         res <- window(x_ts, start = time_border, end = Inf)
                                         return(res)
                                       })
  
  learning_part_nrs <- purrr::map_dbl(learning_part_to_choose, 
                                      .f = function(x) {nrow(x)})
  
  testing_part_nrs <- purrr::map_dbl(testing_part_to_choose, 
                                     .f = function(x) {nrow(x)})
  
  learn_frcst_set_whole_length <- (learn_part_length + forecast_part_length - 1)
  learning_part_max_begins <- learning_part_nrs - learn_frcst_set_whole_length
  testing_part_max_begins <- testing_part_nrs - learn_frcst_set_whole_length
  
  learn_series_sample <- sample(1:ts_list_n, size = learn_set_n, replace = T)
  test_series_sample <- sample(1:ts_list_n, size = test_set_n, replace = T)

  learn_series_indexes <- purrr::imap(table(learn_series_sample),
                                      function(n, i, max_begs){
                                        i <- as.numeric(i)
                                        max_beg <- max_begs[[i]]
                                        idxs <- sample(1:max_beg, 
                                                       size = n, 
                                                       replace = F)
                                        return(idxs)
                                      }, max_begs = learning_part_max_begins)
  
  learn_series_indexes_tbl <- do.call(rbind, 
                                      purrr::imap(.x = learn_series_indexes, 
                                              .f = function(start_idx, i_ts){
                                                i_ts <- rep(as.numeric(i_ts), times = length(start_idx))
                                                res <- cbind(i_ts, start_idx)
                                                return(res)
                                              }))
  
  test_series_indexes <- purrr::imap(table(test_series_sample),
                                      function(n, i, max_begs){
                                        i <- as.numeric(i)
                                        max_beg <- max_begs[[i]]
                                        idxs <- sample(1:max_beg, 
                                                       size = n, 
                                                       replace = F)
                                        return(idxs)
                                      }, max_begs = testing_part_max_begins)
  
  test_series_indexes_tbl <- do.call(rbind, 
                                      purrr::imap(.x = test_series_indexes, 
                                                  .f = function(start_idx, i_ts){
                                                    i_ts <- rep(as.numeric(i_ts), times = length(start_idx))
                                                    res <- cbind(i_ts, start_idx)
                                                    return(res)
                                                  }))
  
  learn_series_res_list <- purrr::pmap(list(ts_ind = as.list(learn_series_indexes_tbl[,1]),
                                            start_ind = as.list(learn_series_indexes_tbl[,2]),
                                            ts_set = list(learning_part_to_choose)), 
                                       .f = function(ts_ind, start_ind, ts_set, sample_len){
                                         sample_indxs <- start_ind:(start_ind+sample_len-1)
                                         res <- ts_set[[ts_ind]][sample_indxs,]
                                         return(res)
                                       }, sample_len = learn_part_length + forecast_part_length)
  names(learn_series_res_list) <- paste(names(learning_part_to_choose)[learn_series_indexes_tbl[,1]],
                                        learn_series_indexes_tbl[,2], sep = "_")
  
  test_series_res_list <- purrr::pmap(list(ts_ind = as.list(test_series_indexes_tbl[,1]),
                                           start_ind = as.list(test_series_indexes_tbl[,2]),
                                           ts_set = list(testing_part_to_choose)), 
                                      .f = function(ts_ind, start_ind, ts_set, sample_len){
                                        sample_indxs <- start_ind:(start_ind+sample_len-1)
                                        res <- ts_set[[ts_ind]][sample_indxs,]
                                        return(res)
                                      }, sample_len = learn_part_length + forecast_part_length)
  names(test_series_res_list) <- paste(names(testing_part_to_choose)[test_series_indexes_tbl[,1]],
                                       test_series_indexes_tbl[,2], sep = "_")
  
  res <- list(learn_series_list = learn_series_res_list,
              test_series_list = test_series_res_list)
  
  return(res)
}

buildParamsSetFinancialSeries <- function(ts_list,
                                          time_border,
                                          shape_DTW_params,
                                          trigonometric_transform_params,
                                          subsequenceWidth = 4,
                                          learn_part_length = 100,
                                          forecast_part_length = 50,
                                          learn_set_n = 500,
                                          test_set_n = 100){
 
  dims <- list(1, c(1,2), c(1, 2, 3))
  dtw_types = c("Dependent", "Independent")
  params_set <- as_tibble(expand.grid(
    dims = dims,
    dtw_types = dtw_types,
    sdp = shape_DTW_params,
    stringsAsFactors = F
  ), stringsAsFactors = F)
  
  params_set_full <- params_set %>%
    dplyr::mutate(
      dimensions = ifelse(
        purrr::map(dims, length) == 1,
        list(1),
        list(c(1, 2))
      ),
      trig_tran_params = ifelse(
        purrr::map(dims, length) == 3,
        list(trigonometric_transform_params),
        list(NULL)
      ),
      descr = purrr::pmap_chr(., 
                          function(dtw_types,
                                   sdp,
                                   dims){
                            paste(
                              "dtw_type_", dtw_types, ".",
                              "shape_desc_type_", sdp@Type, ".",
                              "dims", paste(dims, sep = "_", collapse = "_"),
                              sep = ""
                            )
                          }),
      subsequenceWidth = ifelse(purrr::map(sdp, function(x) {x@Type}) == "simple",
                                1,
                                subsequenceWidth)
    ) %>%
    dplyr::select(-"dims")
  
  learn_test_sets <- sampleRandomTestLearnSets(ts_list = ts_list, 
                                               time_border = time_border, 
                                               learn_part_length = learn_part_length,
                                               forecast_part_length = forecast_part_length,
                                               learn_set_n = learn_set_n,
                                               test_set_n = test_set_n)
  
  res <- list(
    params_table = params_set_full,
    learn_test_sets = learn_test_sets,
    learn_part_length = learn_part_length,
    forecast_part_length = forecast_part_length
  )
  
  return(res)
}

runShapeDTWForDefinedParamsTable <- function(input_params,
                                             targetDistance = c("raw", "shapeDesc"),
                                             normalizationType = c("Unitarization", "Zscore"),
                                             subsequenceBreaks = 1,
                                             includeRefSeries = FALSE,
                                             sd_border = 1.5){
  
  targetDistance <- match.arg(targetDistance)
  normalizationType <- match.arg(normalizationType)
  
  descr <- dplyr::pull(input_params$params_table, descr)
  input_params_filtered <- input_params$params_table %>%
    dplyr::select(-descr)
  
  if(class(future::plan())[2] == "sequential"){
    message("Changing plan to multiprocess")
    future::plan(future::multiprocess)
  }
  
  result_tables <- purrr::pmap(c(input_params_filtered,
                                 tab_ind = list(1:nrow(input_params_filtered)),
                                 learn_set = list(list(input_params$learn_test_sets$learn_series_list)),
                                 test_set = list(list(input_params$learn_test_sets$test_series_list)),
                                 learn_part_length = list(input_params$learn_part_length),
                                 forecast_part_length = list(input_params$forecast_part_length)),
                               
                               function(dtw_types,
                                        sdp,
                                        dimensions,
                                        trig_tran_params,
                                        targetDistance,
                                        normalizationType,
                                        subsequenceWidth,
                                        subsequenceBreaks,
                                        includeRefSeries,
                                        sd_border,
                                        learn_set,
                                        test_set,
                                        learn_part_length,
                                        forecast_part_length,
                                        tab_ind){
                                 
                                 msg <- paste("Calculating table number ", tab_ind, "\n", sep = "")
                                 message(msg)
                                 
                                 learn_set_filtered <- purrr::map(learn_set, .f = function(x, dims){
                                   x[,dims]
                                 }, dims = dimensions)
                                 
                                 test_set_filtered <- purrr::map(test_set, .f = function(x, dims){
                                   x[,dims]
                                 }, dims = dimensions)
                                 
                                 res <- purrr::map_dfr(.x = test_set_filtered, .f = function(test_set){
                                   
                                   kNNResults <- RknnShapeDTWParallel(refSeries = test_set, 
                                                                      learnSeries = learn_set_filtered, 
                                                                      refSeriesStart = 1,
                                                                      shapeDTWParams = sdp, 
                                                                      targetDistance = targetDistance, 
                                                                      distanceType = dtw_types, 
                                                                      normalizationType = normalizationType, 
                                                                      refSeriesLength = learn_part_length, 
                                                                      forecastHorizon = forecast_part_length, 
                                                                      subsequenceWidth = subsequenceWidth, 
                                                                      trigonometricTP = trig_tran_params, 
                                                                      subsequenceBreaks = subsequenceBreaks, 
                                                                      includeRefSeries = includeRefSeries, 
                                                                      sd_border = sd_border)
                                   
                                   res <- data.frame(
                                     "kNNSuccess" = kNNResults$validation_results$kNNSuccess,
                                     "refReturnClass" = as.character(kNNResults$validation_results$refReturnClass),
                                     "testReturnClass" = as.character(kNNResults$validation_results$testReturnClass),
                                     "refSeriesReturn" = kNNResults$validation_results$refSeriesReturn,
                                     "learnSeriesReturn" = kNNResults$validation_results$learnSeriesReturn,
                                     "refTsSD" = kNNResults$validation_results$refTsSD,
                                     "testTsSD" = kNNResults$validation_results$testTsSD,
                                     stringsAsFactors = F
                                   )
                                   
                                   return(res)
                                 })
                               }, 
                               sd_border = sd_border,
                               includeRefSeries = includeRefSeries,
                               subsequenceBreaks = subsequenceBreaks,
                               normalizationType = normalizationType,
                               targetDistance = targetDistance
                               )
  
  names(result_tables) <- descr
  message("Switching plan back to sequential")
  future::plan(future::sequential)
  return(result_tables)
}

classResultsToAccuracyMeasure <- function(classification_results_list,
                                          measure = c("acc", "prec", "rec", "corr"),
                                          target_class = c("Fall", "Growth", "Flat_move")){
  
  measure = match.arg(measure)
  target_class = match.arg(target_class)
  
  acc_res_list <- purrr::map(classification_results_list,
                        function(crt,measure, target_class){
                          
                          res <- switch(
                            measure,
                            acc = sum(crt$refReturnClass == crt$testReturnClass) / nrow(crt),
                            
                            prec = 
                            {
                              crt_filtered <- crt %>% 
                                dplyr::filter(testReturnClass == target_class)
                              
                              sum(crt_filtered$refReturnClass == crt_filtered$testReturnClass) / nrow(crt_filtered)
                            },
                            
                            rec = 
                            {
                              crt_filtered <- crt %>% 
                                dplyr::filter(refReturnClass == target_class)
                              sum(crt_filtered$refReturnClass == crt_filtered$testReturnClass) / nrow(crt_filtered)
                            },
                            
                            corr = cor(crt$refSeriesReturn, crt$learnSeriesReturn)
                          )
                          
                          return(res)
                        }, measure = measure, target_class = target_class)
  
  accResNames <- names(acc_res_list)
  accResNamesSplitted <- stringr::str_split(string = accResNames, 
                                            pattern = "\\.", simplify = T)
  
  colnames(accResNamesSplitted) <- c("DistMatrixType", "DTWType", "Dimensions")
  
  accResTibble <- as_tibble(accResNamesSplitted, stringsAsFactors = F) %>%
    mutate(accuracyRes = unlist(acc_res_list))
  
  accResTibblePivoted <- accResTibble %>%
    tidyr::pivot_wider(names_from = c("DistMatrixType",
                                      "DTWType"), 
                       values_from = "accuracyRes")
  
  return(accResTibblePivoted)
}
