class_results <- function(return_vec, border_vec){
  res <- ifelse(return_vec > border_vec,
                "Growth", ifelse(return_vec < -border_vec,
                                 "Fall", "Flat_move"))
  res
}


update_results <- function(class_res_table,
                           sd_border, 
                           border_type = c("Both", 
                                           "Learn", 
                                           "Test")){
  border_type <- match.arg(border_type)
  
  if(border_type == "Both"){
    test_series_border <- class_res_table$refTsSD * sd_border
    learn_series_border <- class_res_table$testTsSD * sd_border
  }else if(border_type == "Learn"){
    test_series_border <- class_res_table$testTsSD * sd_border
    learn_series_border <- class_res_table$testTsSD * sd_border
  }else{
    test_series_border <- class_res_table$refTsSD * sd_border
    learn_series_border <- class_res_table$refTsSD * sd_border
  }
  
  class_res_table$refReturnClass <- class_results(class_res_table$refSeriesReturn,
                                                  test_series_border)
  class_res_table$testReturnClass <- class_results(class_res_table$learnSeriesReturn,
                                                   learn_series_border)
  class_res_table$kNNSuccess <- ifelse(class_res_table$refReturnClass == class_res_table$testReturnClass,
                            1, 0)
  return(class_res_table)
}


as_single_table <- function(tabs_list){
  res <- purrr::imap_dfr(tabs_list, function(tab, tbl_name){
    tab_nrow <- nrow(tab)
    data_from_names <- stringr::str_split_fixed(string = tbl_name, pattern = "_", n = 3)
    names_data_matrix <- matrix(data = rep(data_from_names, times = tab_nrow),
                                nrow = tab_nrow, byrow = T)
    colnames(names_data_matrix) <- c("Market", "Delta", "Frcst_horizon")
    tab_update <- cbind(names_data_matrix, tab)
    tab_update
  })
  
  return(res)
}

