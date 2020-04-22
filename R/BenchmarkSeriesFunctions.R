load_benchmark_series <- function(benchmarkDataPath, dimNumbers = NULL){
  
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

