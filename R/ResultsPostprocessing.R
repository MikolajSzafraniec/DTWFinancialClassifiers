# Loading required packages
require(dplyr)
require(stringr)

# Sourcing R scripts
source("R/RDTWCalculations.R")
source("R/ResultsPostprocessingFunctions.R")

# Data loading
files_paths <- list.files("Data/Results/FinalResultsRDS/", 
                          full.names = T)
res_names <- stringr::str_extract(
  list.files("Data/Results/FinalResultsRDS/", 
                        full.names = F), 
  pattern = "[^\\.]+")

names(files_paths) <- res_names
analysis_results <- purrr::map(files_paths, readRDS)
analysis_results_updated <- analysis_results

which_100_25 <- stringr::str_which(
  string = names(analysis_results_updated), 
  pattern = "100_25")

which_100_50 <- stringr::str_which(
  string = names(analysis_results_updated), 
  pattern = "100_50")

which_100_100 <- stringr::str_which(
  string = names(analysis_results_updated), 
  pattern = "100_100")

analysis_results_updated[which_100_25] <- purrr::map_depth(
  .x = analysis_results_updated[which_100_25],
  .f = function(x){
    res <- update_results(x, 1, "B")
    res
  }, .depth = 2
)

analysis_results_updated[which_100_50] <- purrr::map_depth(
  .x = analysis_results_updated[which_100_50],
  .f = function(x){
    res <- update_results(x, 1.5, "B")
    res
  }, .depth = 2
)

analysis_results_updated[which_100_100] <- purrr::map_depth(
  .x = analysis_results_updated[which_100_100],
  .f = function(x){
    res <- update_results(x, 2, "B")
    res
  }, .depth = 2
)

results_tables_acc <- purrr::map(analysis_results_updated,
                                 ~classResultsToAccuracyMeasure(.x, measure = "ac"))
results_tables_balanced_acc <- purrr::map(analysis_results_updated,
                                          ~classResultsToAccuracyMeasure(.x, measure = "ba"))
results_tables_pr_Growth <- purrr::map(analysis_results_updated,
                                       ~classResultsToAccuracyMeasure(.x, measure = "pr", "G"))
results_tables_pr_Fall <- purrr::map(analysis_results_updated,
                                     ~classResultsToAccuracyMeasure(.x, measure = "pr", "Fa"))
results_tables_pr_Keep <- purrr::map(analysis_results_updated,
                                     ~classResultsToAccuracyMeasure(.x, measure = "pr", "Fl"))
results_tables_re_Growth <- purrr::map(analysis_results_updated,
                                       ~classResultsToAccuracyMeasure(.x, measure = "re", "G"))
results_tables_re_Fall <- purrr::map(analysis_results_updated,
                                     ~classResultsToAccuracyMeasure(.x, measure = "re", "Fa"))
results_tables_re_Keep <- purrr::map(analysis_results_updated,
                                     ~classResultsToAccuracyMeasure(.x, measure = "re", "Fl"))

write.csv2(as_single_table(results_tables_acc), file = "Data/Results/Results prepared/results_tables_acc.csv")
write.csv2(as_single_table(results_tables_balanced_acc), file = "Data/Results/Results prepared/results_tables_balanced_acc.csv")
write.csv2(as_single_table(results_tables_pr_Growth), file = "Data/Results/Results prepared/results_tables_pr_Growth.csv")
write.csv2(as_single_table(results_tables_pr_Fall), file = "Data/Results/Results prepared/results_tables_pr_Fall.csv")
write.csv2(as_single_table(results_tables_pr_Keep), file = "Data/Results/Results prepared/results_tables_pr_Keep.csv")
write.csv2(as_single_table(results_tables_re_Growth), file = "Data/Results/Results prepared/results_tables_re_Growth.csv")
write.csv2(as_single_table(results_tables_re_Fall) ,file = "Data/Results/Results prepared/results_tables_re_Fall.csv")
write.csv2(as_single_table(results_tables_re_Keep), file = "Data/Results/Results prepared/results_tables_re_Keep.csv")


