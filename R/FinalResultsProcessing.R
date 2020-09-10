nrow(res_binded$dtw_type_Dependent.shape_desc_type_simple.dims1)
cor.test(res_binded$dtw_type_Dependent.shape_desc_type_simple.dims1_2_3$target_series_150_returns,
         res_binded$dtw_type_Dependent.shape_desc_type_simple.dims1_2_3$learn_series_150_returns)

N <- 100
a <- sample(c("A", "B", "C"), size = N, replace = T)
b <- sample(c("A", "B", "C"), size = N, replace = T)

tab <- table(a, b)
QPress <- (N-(49*3))^2 / (N*(3-1))

qnorm(0.95)
qchisq(0.95, df = 2)
qchisq(0.99, df = 1)

classResultsToAccuracyMeasureEuclidPreprocessing(classification_results_list = res$`EURPLN-2019-04`, 
                                                 measure = "cor")

cor.test(res$`CADJPY-2019-04`$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3$target_series_5_returns,
         res$`CADJPY-2019-04`$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3$learn_series_5_returns)

resultsFile <- readRDS(file = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_daily_results_ref_100.rds")
tableMerged <- combineTables(tables_list = resultsFile)
oneTab <- accuracyResultsOneTable(tableMerged)
oneTaboneCompany <- accuracyResultsOneTable(resultsFile$EURCHF)

pheatmap(as.matrix(abc$dtw_type_Independent.shape_desc_type_compound.dims1_2), display_numbers = T, color = colorRampPalette(c('white','red'))(100), 
         cluster_rows = F, cluster_cols = F, fontsize_number = 15,
         xlab = "a")

resultsGPW <- readRDS(file = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_tick_d30min_results_ref_100.rds")
resultsFX <- readRDS(file = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_tick_d30min_results_ref_100.rds")

GPW_small_comp <- combineTables(resultsGPW[c(1, 3, 5, 8, 9)])
GPW_big_comp <- combineTables(resultsGPW[-c(1, 3, 5, 8, 9)])
FX_set <- combineTables(resultsFX)

a <- combineOneMeaseruDifferentSeriesTypes(FX_set = FX_set, GPW_small_comp = GPW_small_comp,
                                           GPW_big_comp = GPW_big_comp, measure = "bal")

a$res_frcst_horizon_150
cor.test(GPW_big_comp$dtw_type_Independent.shape_desc_type_compound.dims1_2$target_series_100_returns,
         GPW_big_comp$dtw_type_Independent.shape_desc_type_compound.dims1_2$learn_series_100_returns)
######################################################################

FX_daily_parsed <- readRDS("../Magisterka tekst/SeriesProcessed/GPW_daily_parsed.rds")
sum(time(FX_daily_filtered$CADJPY) < timeDate(as.Date("2015-01-01"))) + 1 

max(time(FX_d_30$`AUDUSD-2019-04`))
max(time(FX_daily_parsed$CCC))









##################################################################################
# CZESC wlasciwa #
##################################################################################

Global_daily_accuracy <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                 measure = "acc", 
                 aggregation_lvl = "daily")

Global_daily_cor <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                                     measure = "cor", 
                                     aggregation_lvl = "daily")

Global_daily_balanced <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                                     measure = "bal", 
                                     aggregation_lvl = "daily")


# Comparing global accuracy for DTW shape dependent 1 dim, 2 dim and 3 dim
write.csv2(Global_daily_accuracy$dtw_type_Dependent.shape_desc_type_compound.dims1, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1dim.csv")
write.csv2(Global_daily_accuracy$dtw_type_Dependent.shape_desc_type_compound.dims1_2, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_dim.csv")
write.csv2(Global_daily_accuracy$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_3dim.csv")

# Szacowanie minimalnej wartosci istotnej statystycznie
QPressPVal(3, 1500, 545)
545/1500

# Comparing global balanced for DTW shape dependent 1 dim, 2 dim and 3 dim
write.csv2(Global_daily_balanced$dtw_type_Dependent.shape_desc_type_compound.dims1, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1dim_balanced.csv")
write.csv2(Global_daily_balanced$dtw_type_Dependent.shape_desc_type_compound.dims1_2, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_dim_balanced.csv")
write.csv2(Global_daily_balanced$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_3dim_balanced.csv")

# Assesing different methods for daily returns
GPW_daily_200 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_daily_results_ref_200.rds")
FX_daily_200 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_daily_results_ref_200.rds")
daily_200_whole_set <- combineTables(c(GPW_daily_200, FX_daily_200))
daily_results_200_one_table <- accuracyResultsOneTable(daily_200_whole_set)
daily_results_200_one_table$res_frcst_horizon_150

write.csv2(daily_results_200_one_table$res_frcst_horizon_150, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/daily_200_150_different_measures.csv")

# Szacowanie minimum dwie klasy
QPressPVal(2, 1500, 788)

# Wyniki jakosci dla różnyc rynków daily


differentSeriesDaily <- combineOneMeaseruDifferentSeriesTypes(FX_set = combineTables(FX_daily_200), 
                                                              GPW_small_comp = combineTables(GPW_daily_200[c(1, 3, 5, 9, 10)]), 
                                                              GPW_big_comp = combineTables(GPW_daily_200[-c(1, 3, 5, 9, 10)]), 
                                                              measure = "bal")

write.csv2(differentSeriesDaily$res_frcst_horizon_150, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/groups_daily_balanced_accuracy.csv")

# Szacowanie 3 klasy 500 obserwacji
QPressPVal(3, 500, 193)



################# Analiza dla agregacji na poziomie 10 minut #################################
Global_d10min_balanced <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                                          measure = "bal", 
                                          aggregation_lvl = "tick_d10min")


# Comparing global accuracy for DTW shape dependent 1 dim, 2 dim and 3 dim
write.csv2(Global_d10min_balanced$dtw_type_Dependent.shape_desc_type_compound.dims1, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1dim_10min.csv")
write.csv2(Global_d10min_balanced$dtw_type_Dependent.shape_desc_type_compound.dims1_2, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_dim_10min.csv")
write.csv2(Global_d10min_balanced$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_3dim_10min.csv")



# różne typy danych
GPW_d10min_50 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_tick_d10min_results_ref_50.rds")
FX_d10min_50 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_tick_d10min_results_ref_50.rds")


differentSeriesd10min <- combineOneMeaseruDifferentSeriesTypes(FX_set = combineTables(FX_d10min_50), 
                                                              GPW_small_comp = combineTables(GPW_d10min_50[c(1, 3, 5, 9, 10)]), 
                                                              GPW_big_comp = combineTables(GPW_d10min_50[-c(1, 3, 5, 9, 10)]), 
                                                              measure = "balanced_acc")


write.csv2(differentSeriesd10min$res_frcst_horizon_5, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/groups_d10min_balanced_accuracy.csv")

# Szacowanie 3 klasy 500 obserwacji
QPressPVal(3, 500, 193)


table(GPW_small_comp$dtw_type_Dependent.shape_desc_type_compound.dims1_2$target_series_5_return_class,
      GPW_small_comp$dtw_type_Dependent.shape_desc_type_compound.dims1_2$learn_series_5_return_class)
