# Załadowanie niezbędnych pakietów
necPack <- function() {
  c("timeSeries", "lubridate", "stringr", "dplyr", "Rcpp", "dtw")
}

source("R/PackageLoading.R")
# Załadowanie funkcji pakietu Rcpp
Rcpp::sourceCpp("src/RcppDTWFunctions.cpp")

# Załadowanie definicji klas
source("R/ClassDefinitions.R")

###########################################################################################
###      Funkcja przekształcająca obiekt klasy timeSeries o dowolnej liczbie            ###
###         wymiarów w obiekt klasy SubsequenceSeries zawierający szeregi               ###
###                     przekształcone w listę podsekwencji                             ###
###########################################################################################

asSubsequence <- function(ts, subsequenceLenght){
  tsDim <- ncol(ts@.Data)
  tsNames <- colnames(ts@.Data)
  subsequencesList <- vector(mode = "list", length = tsDim)
  names(subsequencesList) <- tsNames
  
  for(i in 1:tsDim){
    subsequencesList[[i]] <- subsequencesMatrix(ts@.Data[,i], subsequenceLenght)
  }
  
  res <- new("SubsequenceSeries",
             Time = time(ts),
             Subsequences = subsequencesList)
  res
}

###########################################################################################
###  Funkcja przekształcająca podsekwencje zawarte w obiekcie klasy SubsequenceSeries   ###
###      w ich deskryptory kształtu zgodnie z podanymi parametrami. Funkcja zwraca      ###
###                       obiekt klasy ShapeDescriptorsSeries                           ###
###########################################################################################

asShapeDescriptors <- function(SubsequenceSeriesObject, ShapeDescriptorParamsObject){
  
}