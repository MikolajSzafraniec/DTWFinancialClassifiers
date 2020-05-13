load_benchmark_series_MD_dataset <- function(filesPaths, classIndicatorCol = NULL){
  
  dataList <- purrr::map(filesPaths, .f = farff::readARFF)
  nDim <- length(dataList)
  
  if(is.null(classIndicatorCol)){
    ncols <- purrr::map_int(dataList, ncol)
    
    if(!all(ncols == ncols[1]))
      stop("Number of columns among listed files is not equal.")
    
    classIndicatorCol <- ncols[1]
  }
  
  classColName <- colnames(dataList[[1]])[classIndicatorCol]
  
  dataGathered <- purrr::imap_dfr(dataList, .f = function(x, i, classColName){
    
    res <- x %>%
      mutate(dimnum = paste("dim", i, sep = "_"), tsNum = 1:nrow(x)) %>%
      tidyr::pivot_longer(cols = -c(classColName, "dimnum", "tsNum"),
                          names_to = "ObsNum", 
                          values_to = "TS_val") %>%
      mutate(ObsNum = as.numeric(stringr::str_match(ObsNum,
                                         "(?<=channel_[0-9]{1,3}_).+")) + 1)
      
    
    return(res)
    
  }, classColName = classColName)
  
  res <- dataGathered %>%
    tidyr::pivot_wider(names_from = dimnum,
                       values_from = TS_val) %>%
    tidyr::nest(TS = -c(classColName, "tsNum")) %>%
    mutate(TS = purrr::map(TS, .f = function(x){
      x %>%
        dplyr::arrange(ObsNum) %>%
        dplyr::select(-ObsNum)
    })) %>%
    dplyr::rename("ClassInd" = classColName)
  
  return(res)
}


load_benchmark_series_activities <- function(benchmarkDataPath, dimNumbers = NULL){
  
  # Getting list of all files stored in all subdirectories
  filesList <- list.files(benchmarkDataPath, all.files = T, recursive = T)
  
  # Reading files and writing them to data frame along with additional informations
  dataSet <- purrr::map_dfr(.x = filesList, .f = function(fp, dataPath, dimNumbers){
    
    activitySym <- stringr::str_sub(fp, start = 1, end = 3)
    personSym <- stringr::str_sub(fp, start = 5, end = 6)
    segmentSym <- stringr::str_sub(fp, start = 8, end = 10)
    
    fullPath <- paste(dataPath, fp, sep = "/")
    

    dataSet <- data.table::fread(file = fullPath, select = dimNumbers,
                                 sep = ",", dec = ".", header = F, 
                                 data.table = F)
    
    nDim <- ncol(dataSet)
    dimNames <- paste0("dim_", 1:nDim)
    colnames(dataSet) <- dimNames
    
    res <- dataSet %>%
      dplyr::mutate(activitySym = activitySym,
                    personSym = personSym,
                    segmentSym = segmentSym) %>%
      tidyr::nest(activity_record = dimNames)
    
    return(res)
    
  }, dataPath = benchmarkDataPath, dimNumbers = dimNumbers)
  
  return(dataSet)
}


findNNBenchmarkSeries_activities <- function(benchmarkTS,
                                  refIndex, 
                                  shapeDTWParams,
                                  targetDistance = c("raw", "shape"),
                                  distanceType = c("Dependent", "Independent"),
                                  normalizationType = c("Unitarization", "Zscore"),
                                  subsequenceWidth = 4,
                                  trigonometricTP = NULL){
  
  message("Looking for the NN for the series: ", refIndex, "/", nrow(benchmarkTS))
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  refPerson <- benchmarkTS %>%
    dplyr::pull(personSym) %>%
    .[refIndex]
  
  testSet <- benchmarkTS %>%
    dplyr::filter(personSym != refPerson)
  
  referenceTibble <- benchmarkTS %>%
    dplyr::pull(activity_record) %>%
    .[refIndex]
  
  results_set <- purrr::pmap(.l = list(refTS = referenceTibble,
                                       testTS = testSet$activity_record), 
                             .f = function(refTS, testTS){
                               refTS <- as.matrix(refTS)
                               testTS <- as.matrix(testTS)
                             
                               res <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = refTS, 
                                                                   testSeries = testTS, 
                                                                   forecastHorizon = 0, 
                                                                   subsequenceWidth = subsequenceWidth, 
                                                                   subsequenceBreaks = 1, 
                                                                   shapeDescriptorParams = SDP, 
                                                                   normalizationType = normalizationType, 
                                                                   distanceType = distanceType)
                               })
  
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
  
  res <- testSet %>%
    dplyr::select(activitySym, personSym, segmentSym) %>%
    .[which_dist_min,]
  
  return(res)
}

benchSeriesSelfClassParallel_activities <- function(benchmarkTS, 
                                         shapeDTWParams,
                                         targetDistance = c("raw", "shape"),
                                         distanceType = c("Dependent", "Independent"),
                                         normalizationType = c("Unitarization", "Zscore"),
                                         subsequenceWidth = 4,
                                         trigonometricTP = NULL){
  
  targetDistance = match.arg(targetDistance)
  distanceType = match.arg(distanceType)
  normalizationType = match.arg(normalizationType)
  
  indexesList <- 1:nrow(benchmarkTS)
  
  if(class(future::plan())[2] == "sequential"){
    message("Switching plan to multiprocess.")
    future::plan(future::multiprocess)
  }
  
  resTS <- benchmarkTS %>%
    dplyr::mutate(classificationResults = 
                    {
                      furrr::future_map_chr(.x = indexesList, 
                                            function(ind){
                                              class_res <- findNNBenchmarkSeries_activities(benchmarkTS = benchmarkTS, 
                                                                                 refIndex = ind, 
                                                                                 shapeDTWParams = shapeDTWParams, 
                                                                                 targetDistance = targetDistance, 
                                                                                 distanceType = distanceType, 
                                                                                 normalizationType = normalizationType, 
                                                                                 subsequenceWidth = subsequenceWidth, 
                                                                                 trigonometricTP = trigonometricTP)
                                              return(class_res$activitySym)
                                            }, 
                                            .progress = F)
                    })
  
  message("Switching plan to sequential.")
  future::plan(future::sequential)
  
  return(resTS)
}


#####################################################################################

findNNBenchmarkSeries_general <- function(benchmarkTS,
                                          refIndex, 
                                          shapeDTWParams,
                                          testSet = NULL,
                                          excludeCol = NULL,
                                          targetDistance = c("raw", "shape"),
                                          distanceType = c("Dependent", "Independent"),
                                          normalizationType = c("Unitarization", "Zscore"),
                                          subsequenceWidth = 4,
                                          trigonometricTP = NULL){
  
  message("Looking for the NN for the series: ", refIndex, "/", nrow(benchmarkTS))
  
  targetDistance <- match.arg(targetDistance)
  distanceType <- match.arg(distanceType)
  normalizationType <- match.arg(normalizationType)
  
  if(is.null(testSet)){
    
    if(is.null(excludeCol)) stop("Param excludeCol must be specified when testSet is not passed")
    
    excludeCol <- dplyr::sym(excludeCol)
    
    refObject <- benchmarkTS %>%
      dplyr::pull(!!excludeCol) %>%
      .[refIndex]
    
    testSet <- benchmarkTS %>%
      dplyr::filter(!!excludeCol != refObject)
  }
  
  referenceTibble <- benchmarkTS %>%
    dplyr::pull(TS) %>%
    .[refIndex]
  
  results_set <- purrr::pmap(.l = list(refTS = referenceTibble,
                                       testTS = testSet$TS), 
                             .f = function(refTS, testTS){
                               refTS <- as.matrix(refTS)
                               testTS <- as.matrix(testTS)
                               
                               res <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = refTS, 
                                                                   testSeries = testTS, 
                                                                   forecastHorizon = 0, 
                                                                   subsequenceWidth = subsequenceWidth, 
                                                                   subsequenceBreaks = 1, 
                                                                   shapeDescriptorParams = shapeDTWParams, 
                                                                   normalizationType = normalizationType, 
                                                                   distanceType = distanceType)
                             })
  
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
  
  res <- testSet %>%
    dplyr::select(ClassInd, tsNum) %>%
    .[which_dist_min,]
  
  return(res)
}

benchSeriesSelfClassParallel_general <- function(benchmarkTS, 
                                                 shapeDTWParams,
                                                 testSet = NULL,
                                                 excludeCol = NULL,
                                                 targetDistance = c("raw", "shape"),
                                                 distanceType = c("Dependent", "Independent"),
                                                 normalizationType = c("Unitarization", "Zscore"),
                                                 subsequenceWidth = 4,
                                                 trigonometricTP = NULL){
  
  targetDistance = match.arg(targetDistance)
  distanceType = match.arg(distanceType)
  normalizationType = match.arg(normalizationType)
  
  indexesList <- 1:nrow(benchmarkTS)
  
  if(class(future::plan())[2] == "sequential"){
    message("Switching plan to multiprocess.")
    future::plan(future::multiprocess)
  }
  
  resTS <- benchmarkTS %>%
    dplyr::mutate(classificationResults = 
    {
      furrr::future_map_chr(.x = indexesList, 
                            function(ind){
                              class_res <- findNNBenchmarkSeries_general(benchmarkTS = benchmarkTS, 
                                                                 refIndex = ind,
                                                                 testSet = testSet,
                                                                 excludeCol = excludeCol,
                                                                 shapeDTWParams = shapeDTWParams, 
                                                                 targetDistance = targetDistance, 
                                                                 distanceType = distanceType, 
                                                                 normalizationType = normalizationType, 
                                                                 subsequenceWidth = subsequenceWidth, 
                                                                 trigonometricTP = trigonometricTP)
                              return(class_res$ClassInd)
                            }, 
                            .progress = T)
    })
  
  message("Switching plan to sequential.")
  future::plan(future::sequential)
  
  return(resTS)
}
