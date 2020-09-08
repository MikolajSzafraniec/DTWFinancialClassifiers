################################################################################################################

classResultsToAccuracyMeasureEuclidPreprocessing <- function(classification_results_list,
                                                             measure = c("acc", "balanced_acc", "prec", "rec", "corr", "sign_acc"),
                                                             target_class = c("Sell", "Hold", "Buy")){
  
  measure = match.arg(measure)
  target_class = match.arg(target_class)
  
  forecast_horizons <- stringr::str_extract(colnames(classification_results_list[[1]]), 
                                            pattern = "[0-9]{1,3}") %>% 
    .[!is.na(.)] %>%
    unique(.)
  
  names_ref_series_returns <- paste0("target_series_", forecast_horizons, "_returns")
  names_learn_series_returns <- paste0("learn_series_", forecast_horizons, "_returns")
  names_ref_series_class <- paste0("target_series_", forecast_horizons, "_return_class")
  names_learn_series_class <- paste0("learn_series_", forecast_horizons, "_return_class")
  
  res <- purrr::pmap(list(names_ref_series_returns,
                          names_learn_series_returns,
                          names_ref_series_class,
                          names_learn_series_class),
                     
                     function(nrsr, nlsr, nrsc, nlsc, crl, measure, target_class){
                       
                       acc_res_list <- purrr::map(.x = crl, .f = function(crt, 
                                                                          nrsr, 
                                                                          nlsr, 
                                                                          nrsc, 
                                                                          nlsc, 
                                                                          measure, 
                                                                          target_class){
                         
                         res <- switch (
                           measure,
                           acc = sum(crt[[nrsc]] == crt[[nlsc]]) / nrow(crt),
                           
                           balanced_acc = 
                             {
                               acc_sell <- 
                                 sum(crt[[nrsc]] == "Sell" & (crt[[nlsc]] == crt[[nrsc]])) /
                                 sum(crt[[nrsc]] == "Sell")
                               
                               acc_buy <- 
                                 sum(crt[[nrsc]] == "Buy" & (crt[[nlsc]] == crt[[nrsc]])) /
                                 sum(crt[[nrsc]] == "Buy")
                               
                               acc_hold <- 
                                 sum(crt[[nrsc]] == "Hold" & (crt[[nlsc]] == crt[[nrsc]])) /
                                 sum(crt[[nrsc]] == "Hold")
                               
                               sum(acc_sell, acc_buy, acc_hold) / 3
                             },
                           
                           prec = 
                             {
                               crt_filtered <- crt %>% 
                                 dplyr::filter(!!dplyr::sym(nlsc) == target_class)
                               
                               sum(crt_filtered[[nrsc]] == crt_filtered[[nlsc]]) / nrow(crt_filtered)
                             },
                           
                           rec = 
                             {
                               crt_filtered <- crt %>% 
                                 dplyr::filter(!!dplyr::sym(nrsc) == target_class)
                               sum(crt_filtered[[nrsc]] == crt_filtered[[nlsc]]) / nrow(crt_filtered)
                             },
                           
                           corr = cor(crt[[nrsr]], crt[[nlsr]]),
                           
                           sign_acc = sum(sign(crt[[nrsr]]) == sign(crt[[nlsr]])) / nrow(crt)
                         )
                         
                         return(res)
                       }, 
                       measure = measure, 
                       target_class = target_class, 
                       nrsr = nrsr, 
                       nlsr = nlsr, 
                       nrsc = nrsc, 
                       nlsc = nlsc)
                       
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
                       
                     }, 
                     crl = classification_results_list, 
                     measure = measure, 
                     target_class = target_class)
  
  names(res) <- paste0("res_frcst_horizon_", forecast_horizons)
  
  names_ref_series_returns <- paste0("target_series_", forecast_horizons, "_returns")
  names_euclid_series_returns <- paste0("euclid_series_", forecast_horizons, "_returns")
  names_ref_series_class <- paste0("target_series_", forecast_horizons, "_return_class")
  names_euclid_series_class <- paste0("euclid_series_", forecast_horizons, "_return_class")
  
  euclid_table <- purrr::pmap(list(names_ref_series_returns,
                                   names_euclid_series_returns,
                                   names_ref_series_class,
                                   names_euclid_series_class),
                              function(nrsr, nesr, nrsc, nesc, crl, measure, target_class){
                                
                                crt_1_dim <- crl$dtw_type_Dependent.shape_desc_type_simple.dims1
                                crt_2_dim <- crl$dtw_type_Dependent.shape_desc_type_simple.dims1_2
                                
                                euclid_1_dim <- switch (
                                  measure,
                                  acc = sum(crt_1_dim[[nrsc]] == crt_1_dim[[nesc]]) / nrow(crt_1_dim),
                                  
                                  balanced_acc = 
                                    {
                                      acc_sell <- 
                                        sum(crt_1_dim[[nrsc]] == "Sell" & (crt_1_dim[[nesc]] == crt_1_dim[[nrsc]])) /
                                        sum(crt_1_dim[[nrsc]] == "Sell")
                                      
                                      acc_buy <- 
                                        sum(crt_1_dim[[nrsc]] == "Buy" & (crt_1_dim[[nesc]] == crt_1_dim[[nrsc]])) /
                                        sum(crt_1_dim[[nrsc]] == "Buy")
                                      
                                      acc_hold <- 
                                        sum(crt_1_dim[[nrsc]] == "Hold" & (crt_1_dim[[nesc]] == crt_1_dim[[nrsc]])) /
                                        sum(crt_1_dim[[nrsc]] == "Hold")
                                      
                                      sum(acc_sell, acc_buy, acc_hold) / 3
                                    },
                                  
                                  prec = 
                                    {
                                      crt_filtered <- crt_1_dim %>% 
                                        dplyr::filter(!!dplyr::sym(nesc) == target_class)
                                      
                                      sum(crt_filtered[[nrsc]] == crt_filtered[[nesc]]) / nrow(crt_filtered)
                                    },
                                  
                                  rec = 
                                    {
                                      crt_filtered <- crt_1_dim %>% 
                                        dplyr::filter(!!dplyr::sym(nrsc) == target_class)
                                      sum(crt_filtered[[nrsc]] == crt_filtered[[nesc]]) / nrow(crt_filtered)
                                    },
                                  
                                  corr = cor(crt_1_dim[[nrsr]], crt_1_dim[[nesr]]),
                                  
                                  sign_acc = sum(sign(crt_1_dim[[nrsr]]) == sign(crt_1_dim[[nesr]])) / nrow(crt_1_dim)
                                )
                                
                                euclid_2_dim <- switch (
                                  measure,
                                  acc = sum(crt_2_dim[[nrsc]] == crt_2_dim[[nesc]]) / nrow(crt_2_dim),
                                  
                                  balanced_acc = 
                                    {
                                      acc_sell <- 
                                        sum(crt_2_dim[[nrsc]] == "Sell" & (crt_2_dim[[nesc]] == crt_2_dim[[nrsc]])) /
                                        sum(crt_2_dim[[nrsc]] == "Sell")
                                      
                                      acc_buy <- 
                                        sum(crt_2_dim[[nrsc]] == "Buy" & (crt_2_dim[[nesc]] == crt_2_dim[[nrsc]])) /
                                        sum(crt_2_dim[[nrsc]] == "Buy")
                                      
                                      acc_hold <- 
                                        sum(crt_2_dim[[nrsc]] == "Hold" & (crt_2_dim[[nesc]] == crt_2_dim[[nrsc]])) /
                                        sum(crt_2_dim[[nrsc]] == "Hold")
                                      
                                      sum(acc_sell, acc_buy, acc_hold) / 3
                                    },
                                  
                                  prec = 
                                    {
                                      crt_filtered <- crt_2_dim %>% 
                                        dplyr::filter(!!dplyr::sym(nesc) == target_class)
                                      
                                      sum(crt_filtered[[nrsc]] == crt_filtered[[nesc]]) / nrow(crt_filtered)
                                    },
                                  
                                  rec = 
                                    {
                                      crt_filtered <- crt_2_dim %>% 
                                        dplyr::filter(!!dplyr::sym(nrsc) == target_class)
                                      sum(crt_filtered[[nrsc]] == crt_filtered[[nesc]]) / nrow(crt_filtered)
                                    },
                                  
                                  corr = cor(crt_2_dim[[nrsr]], crt_2_dim[[nesr]]),
                                  
                                  sign_acc = sum(sign(crt_2_dim[[nrsr]]) == sign(crt_2_dim[[nesr]])) / nrow(crt_2_dim)
                                )
                                
                                return(c(euclid_1_dim = euclid_1_dim, euclid_2_dim = euclid_2_dim))
                                
                              }, crl = classification_results_list, measure = measure, target_class = target_class)
  
  euclid_table <- do.call(rbind, euclid_table)
  rownames(euclid_table) <- paste0("res_frcst_horizon_", forecast_horizons)
  
  res <- c(res, euclid_table = list(euclid_table))
  
  return(res)
}

combineTables <- function(tables_list){
  
  res_binded <- purrr::map(1:12, function(i, .tables_list){
    do.call(rbind, purrr::map_depth(.tables_list, 1, function(x){
      x[[i]]
    }))
  }, .tables_list = tables_list)
  
  names(res_binded) <- names(tables_list[[1]])
  
  res_binded
}

expandTable <- function(tab, val_name = "val", cols = 2:5){
  
  temp_table <- tab %>%
    pivot_longer(cols = all_of(cols), names_to = "algorithm", values_to = val_name)
  
  new_measure_names <- paste(temp_table$algorithm, temp_table$Dimensions, sep = "_")
  
  res <- temp_table %>%
    select(val_name) %>%
    mutate(algorithm = new_measure_names)
  
  return(res)
}

expandTableSetEuclidCombine <- function(tableSet, val_name = "val"){
  
  setLength <- length(tableSet)
  expandedTabs <- purrr::map(tableSet[-setLength], ~expandTable(., val_name))
  euclidExpanded <- purrr::map(1:8, function(i, euclidTable, .val_name){
    res <- tibble(
      euclidTable[i,],
      c("euclid_1_dim", "euclid_2_dim")
    )
    
    colnames(res) <- c(.val_name, "algorithm")
    
    res
  }, euclidTable = tableSet$euclid_table, .val_name = val_name)
  
  res_list <- purrr::pmap(list(expandedTabs, euclidExpanded), .f = function(x, y){
    rbind(x, y)
  })
  
  res_list
}


accuracyResultsOneTable <- function(resultsList){
  
  args_list <- list(
    acc = list(measure = "acc"),
    balanced_acc = list(measure = "balanced_acc"),
    prec_buy = list(measure = "prec", target_class = "Buy"),
    rec_buy = list(measure = "rec", target_class = "Buy"),
    prec_sell = list(measure = "prec", target_class = "Sell"),
    rec_sell = list(measure = "rec", target_class = "Sell"),
    prec_hold = list(measure = "prec", target_class = "Hold"),
    rec_hold = list(measure = "rec", target_class = "Hold"),
    acc_no_hold = list(measure = "sign_acc"),
    cor = "cor"
  )
  
  separatedMeasuresSets <- purrr::map(args_list, function(args, .resList){
    
    args <- c(classification_results_list = list(.resList), args)
    res <- do.call(classResultsToAccuracyMeasureEuclidPreprocessing, args = args)
    res
    
  }, .resList = resultsList)
  
  separatedMeasuresSetsExpanded <- purrr::imap(separatedMeasuresSets,
                                               ~expandTableSetEuclidCombine(tableSet = ..1, 
                                                                            val_name = ..2))
  
  res <- purrr::pmap(.l = separatedMeasuresSetsExpanded, function(...){
    Reduce(function(...) merge(..., by='algorithm', all.x=TRUE), x = list(...))
  })
  
  res
}

############ Combining results for different type of financial series and one measure ###################
combineOneMeaseruDifferentSeriesTypes <- function(FX_set, GPW_small_comp, GPW_big_comp,
                                                  measure = c("acc",
                                                              "balanced_acc",
                                                              'prec_buy',
                                                              "rec_buy",
                                                              "prec_sell",
                                                              "rec_sell",
                                                              "prec_hold",
                                                              "rec_hold",
                                                              "acc_no_hold",
                                                              "cor")){
  
  all_observations <- combineTables(list(FX_set, GPW_small_comp, GPW_big_comp))

  measure <- match.arg(measure)
  
  args_list <- list(
    acc = list(measure = "acc"),
    balanced_acc = list(measure = "balanced_acc"),
    prec_buy = list(measure = "prec", target_class = "Buy"),
    rec_buy = list(measure = "rec", target_class = "Buy"),
    prec_sell = list(measure = "prec", target_class = "Sell"),
    rec_sell = list(measure = "rec", target_class = "Sell"),
    prec_hold = list(measure = "prec", target_class = "Hold"),
    rec_hold = list(measure = "rec", target_class = "Hold"),
    acc_no_hold = list(measure = "sign_acc"),
    cor = "cor"
  )
  
  chosen_args <- args_list[[measure]]
  
  separatedMeasures <- purrr::map(list(
    GPW_small_comp = GPW_small_comp,
    GPW_big_comp = GPW_big_comp,
    FX_set = FX_set,
    all_observations = all_observations
  ), function(x, .args){
    do.call(classResultsToAccuracyMeasureEuclidPreprocessing, 
            args = c(classification_results_list = list(x), .args))
  }, .args = chosen_args)
  
  
  separatedMeasuresSetsExpanded <- purrr::imap(separatedMeasures,
                                               ~expandTableSetEuclidCombine(tableSet = ..1, 
                                                                            val_name = ..2))
  res <- purrr::pmap(.l = separatedMeasuresSetsExpanded, function(...){
    Reduce(function(...) merge(..., by='algorithm', all.x=TRUE), x = list(...))
  })
  
  res
}

