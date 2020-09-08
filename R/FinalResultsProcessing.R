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

pheatmap(as.matrix(oneTaboneCompany$res_frcst_horizon_50[,-1]), display_numbers = T, color = colorRampPalette(c('white','red'))(100), 
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