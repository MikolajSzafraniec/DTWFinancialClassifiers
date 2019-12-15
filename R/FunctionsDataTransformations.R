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
  tsDim <- length(SubsequenceSeriesObject@Subsequences) 
  tsNames <- names(SubsequenceSeriesObject@Subsequences)
  shapeDescriptors <- vector(mode = "list", length = tsDim)
  names(shapeDescriptors) <- tsNames
  
  for(i in 1:tsDim){
    shapeDescriptors[[i]] <- asShapeDescriptorCpp(SubsequenceSeriesObject@Subsequences[[i]],
                                                  ShapeDescriptorParamsObject)
  }
  
  res <- new("ShapeDescriptorsSeries",
             Time = SubsequenceSeriesObject@Time,
             shapeDescriptors = shapeDescriptors)
  return(res)
}

sub <- asSubsequence(FXDayAgg, 10)
SDP1 <- new("ShapeDescriptorParams", Type = "compound", 
           Descriptors = c("RawSubsequence","slopeDescriptor", "PAADescriptor", "derivativeDescriptor"), 
           Additional_params = list(Weights = c(1, 1, 1, 1),
                                    slopeWindow = 2L, PAAWindow = 2L))
SDP2 <- new("ShapeDescriptorParams", Descriptors = "slopeDescriptor", Additional_params = 
              list(slopeWindow = 4L, Weights = 1, PAAWindow = 1L))

SDP1 <- new("ShapeDescriptorParams", Type = "compound", 
            Descriptors = c("slopeDescriptor", "PAADescriptor", "derivativeDescriptor"), 
            Additional_params = list(Weights = c(1, 1, 1),
                                     slopeWindow = 2L, PAAWindow = 2L))


SDP1 <- list(Type = "compound", 
            Descriptors = c("RawSubsequence","slopeDescriptor", "PAADescriptor", "derivativeDescriptor"), 
            Additional_params = list(Weights = c(1, 1, 1, 1),
                                     slopeWindow = 2L, PAAWindow = 2L))
SDP2 <- list(Type = "simple", Descriptors = "slopeDescriptor", Additional_params = 
              list(slopeWindow = 4L, Weights = 1, PAAWindow = 1L))



shapeDesc <- asShapeDescriptors(sub, SDP2)
a <- NULL
for(i in 1:100){
  a <- asShapeDescriptors(sub, SDP1)
}

