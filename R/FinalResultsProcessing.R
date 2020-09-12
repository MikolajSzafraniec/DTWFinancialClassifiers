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
QPressPVal(3, 1500, 545)


table(GPW_small_comp$dtw_type_Dependent.shape_desc_type_compound.dims1_2$target_series_5_return_class,
      GPW_small_comp$dtw_type_Dependent.shape_desc_type_compound.dims1_2$learn_series_5_return_class)

################# Analiza dla agregacji na poziomie 1 minuty #################################
Global_d1min_cor <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                                           measure = "cor", 
                                           aggregation_lvl = "tick_d1min")

# Comparing global cor for DTW shape dependent 1 dim, 2 dim and 3 dim
write.csv2(Global_d1min_cor$dtw_type_Dependent.shape_desc_type_compound.dims1, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1dim_1min.csv")
write.csv2(Global_d1min_cor$dtw_type_Dependent.shape_desc_type_compound.dims1_2, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_dim_1min.csv")
write.csv2(Global_d1min_cor$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_3dim_1min.csv")

CorPval(0.0507, 1500)*2

# różne typy danych
GPW_d1min_400 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_tick_d1min_results_ref_400.rds")
FX_d1min_400 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_tick_d1min_results_ref_400.rds")


differentSeriesd1min <- combineOneMeaseruDifferentSeriesTypes(FX_set = combineTables(FX_d1min_400), 
                                                               GPW_small_comp = combineTables(GPW_d1min_400[c(1, 3, 5, 9, 10)]), 
                                                               GPW_big_comp = combineTables(GPW_d1min_400[-c(1, 3, 5, 9, 10)]), 
                                                               measure = "cor")


write.csv2(differentSeriesd1min$res_frcst_horizon_75, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/groups_d1min_cor.csv")

CorPval(0.0507, 1500)*2
CorPval(0.0878, 500)*2

################# Analiza dla agregacji na poziomie 5 minut #################################
Global_d5min_cor <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                                     measure = "cor", 
                                     aggregation_lvl = "tick_d5min")

# Comparing global cor for DTW shape dependent 1 dim, 2 dim and 3 dim
write.csv2(Global_d5min_cor$dtw_type_Dependent.shape_desc_type_compound.dims1, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1dim_5min.csv")
write.csv2(Global_d5min_cor$dtw_type_Dependent.shape_desc_type_compound.dims1_2, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_dim_5min.csv")
write.csv2(Global_d5min_cor$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_3dim_5min.csv")

# różne typy danych
GPW_d5min_200 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_tick_d5min_results_ref_200.rds")
FX_d5min_200 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_tick_d5min_results_ref_200.rds")

differentSeriesd3min <- combineOneMeaseruDifferentSeriesTypes(FX_set = combineTables(FX_d5min_200), 
                                                              GPW_small_comp = combineTables(GPW_d5min_200[c(1, 3, 5, 9, 10)]), 
                                                              GPW_big_comp = combineTables(GPW_d5min_200[-c(1, 3, 5, 9, 10)]), 
                                                              measure = "cor")


write.csv2(differentSeriesd5min$res_frcst_horizon_150, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/groups_d5min_cor.csv")


################# Analiza dla agregacji na poziomie 30 minut #################################
Global_d30min_ba <- GatherAllResults(files_folder = "../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/", 
                                     measure = "balan", 
                                     aggregation_lvl = "tick_d30min")

# Comparing global BA for DTW shape dependent 1 dim, 2 dim and 3 dim
write.csv2(Global_d30min_ba$dtw_type_Dependent.shape_desc_type_compound.dims1, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1dim_30min.csv")
write.csv2(Global_d30min_ba$dtw_type_Dependent.shape_desc_type_compound.dims1_2, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_dim_30min.csv")
write.csv2(Global_d30min_ba$dtw_type_Dependent.shape_desc_type_compound.dims1_2_3, 
           file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/Global_shape_1_2_3dim_30min.csv")

QPressPVal(3, 1500, 545)

# różne typy danych
GPW_d30min_400 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_tick_d30min_results_ref_400.rds")
FX_d30min_400 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/FX_tick_d30min_results_ref_400.rds")

differentSeriesd30min <- combineOneMeaseruDifferentSeriesTypes(FX_set = combineTables(FX_d30min_400), 
                                                              GPW_small_comp = combineTables(GPW_d30min_400[c(1, 3, 5, 9, 10)]), 
                                                              GPW_big_comp = combineTables(GPW_d30min_400[-c(1, 3, 5, 9, 10)]), 
                                                              measure = "bal")


write.csv2(differentSeriesd30min$res_frcst_horizon_200, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/CSV/groups_d30min_balanced.csv")

QPressPVal(3, 1500, 545)
QPressPVal(3, 500, 193)

# Dzienne niezwyczajne korelacje
GPWDaily400 <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_daily_results_ref_400.rds")
PGNIGNDaily400 <- classResultsToAccuracyMeasureEuclidPreprocessing(GPWDaily400$PGNIG, "cor")

write.csv2(PGNIGNDaily400$res_frcst_horizon_200, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/HighCorPGNIG.csv")

CorPval(0.23, 100)*2

# scatterplot
ggplot(GPWDaily400$PGNIG$dtw_type_Independent.shape_desc_type_compound.dims1_2, aes(x = target_series_200_returns, y = learn_series_200_returns)) +
  geom_point() +
  xlab("Real return") +
  ylab("Forecasted return") + ggtitle("PGNIG") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
  
classResultsToAccuracyMeasureEuclidPreprocessing(GPWDaily400$PKOBP, "cor")

# plot czesc uczaca testowa

PGNIG_data_to_plot <- as.data.frame(GPW_daily_filtered$PGNIG) %>%
  mutate(date = as.Date(time(GPW_daily_filtered$PGNIG)))


ggplot2::ggplot(PGNIG_data_to_plot, aes(date)) +
  ggplot2::geom_line(aes(y = closePrice)) +
  labs(title = "PGNiG", 
       subtitle  = paste(
         strftime(min(PGNIG_data_to_plot$date), "%d.%m.%Y"), " - ", 
         strftime(max(PGNIG_data_to_plot$date), "%d.%m.%Y"))) +
  geom_vline(xintercept = as.Date("2013-06-01"), size = 1, col = "red")

# Ujemna korelacja
PKOBPDaily400 <- classResultsToAccuracyMeasureEuclidPreprocessing(GPWDaily400$PKOBP, "cor")

write.csv2(PKOBPDaily400$res_frcst_horizon_150, file = "../Magisterka tekst/Wyniki/Tabele rozdzial 5/LowCorPKOBP.csv")

cor(GPWDaily400$PKOBP$dtw_type_Independent.shape_desc_type_compound.dims1_2$target_series_150_returns[30:100],
    GPWDaily400$PKOBP$dtw_type_Independent.shape_desc_type_compound.dims1_2$learn_series_150_returns[30:100])

# scatterplot
ggplot(GPWDaily400$PKOBP$dtw_type_Independent.shape_desc_type_compound.dims1_2[30:100,], 
       aes(x = target_series_200_returns, y = learn_series_200_returns)) +
  geom_point() +
  xlab("Real return") +
  ylab("Forecasted return") + ggtitle("PKOBP") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# plot czesc uczaca testowa

PKOBP_data_to_plot <- as.data.frame(GPW_daily_filtered$PKOBP) %>%
  mutate(date = as.Date(time(GPW_daily_filtered$PKOBP)))


ggplot2::ggplot(PKOBP_data_to_plot, aes(date)) +
  ggplot2::geom_line(aes(y = closePrice)) +
  labs(title = "PKOBP", 
       subtitle  = paste(
         strftime(min(PKOBP_data_to_plot$date), "%d.%m.%Y"), " - ", 
         strftime(max(PKOBP_data_to_plot$date), "%d.%m.%Y"))) +
  geom_vline(xintercept = as.Date("2013-06-01"), size = 1, col = "red")
