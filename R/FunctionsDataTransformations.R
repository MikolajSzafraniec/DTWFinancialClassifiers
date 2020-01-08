# Załadowanie niezbędnych pakietów
necPack <- function() {
  c("timeSeries", "lubridate", "stringr", "dplyr", "Rcpp", "dtw", "RcppArmadillo")
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

asSubsequence <- function(ts, subsequenceWidth){
  tsDim <- ncol(ts@.Data)
  tsNames <- colnames(ts@.Data)
  subsequencesList <- vector(mode = "list", length = tsDim)
  names(subsequencesList) <- tsNames
  
  for(i in 1:tsDim){
    subsequencesList[[i]] <- subsequencesMatrix(ts@.Data[,i], subsequenceWidth)
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
  
  shapeDescriptors <- lapply(SubsequenceSeriesObject@Subsequences, asShapeDescriptorCpp,
                             shapeDescriptorParams = ShapeDescriptorParamsObject)
  
  res <- new("ShapeDescriptorsSeries",
             Time = SubsequenceSeriesObject@Time,
             shapeDescriptors = shapeDescriptors)
  return(res)
}

###########################################################################################
###         Funkcja przekształcająca szereg czasowy w jego wybraną transformatę         ###
###                                      trygonometryczną.                              ###
###########################################################################################

trigonometricTransform <- function(inputTS, transformType = c("sinus", "cosinus", "hilbert")){
  transformType <- match.arg(transformType)
  res <- trigonometicTransformCpp(inputTS, transformType)
  return(res)
}

