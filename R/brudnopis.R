ramka <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))

ramka_v2 <- ramka %>%
  mutate(d = a*b)

ramka_v3 <- ramka %>%
  mutate(a = (b^2))


?switch


czas <- as.POSIXct(c("2019-01-01 00:00:01.002",
                     "2019-01-01 00:00:01.045",
                     "2019-01-01 00:00:01.992"),
                   format = "%Y-%m-%d %H:%M:%OS", tz = "CET")
czas

format(czas, format = "%Y-%m-%d %H:%M:%OS4")

require(timeSeries)

my_ts <- timeSeries(data = c(1, 2, 3), charvec = czas, FinCenter = "CET")


#################################################
############ TEST ŁADOWANIA DANYCH ##############
#################################################

# Test ładowania danych
FX_tick <- load_financial_data("C:/Dane/Dokumenty/Studia/Praca magisterska/Dane/FX tick",
                               data_type = "FOREX_tick", include_all = T)

FX_day <- load_financial_data("C:/Dane/Dokumenty/Studia/Praca magisterska/Dane/FX day",
                              data_type = "FOREX_da", include_all = T)

GPW_tick <- load_financial_data("C:/Dane/Dokumenty/Studia/Praca magisterska/Dane/GPW tick",
                                data_type = "GPW_t", include_all = T)

GPW_day <- load_financial_data("C:/Dane/Dokumenty/Studia/Praca magisterska/Dane/GPW day",
                               data_type = "GPW_d", include_all = T)

head(FX_tick$`AUDJPY-2019-01`)
head(FX_day$USDRUB)
head(GPW_tick$CCC)
head(GPW_day$TORPOL)

FX_tick$`AUDJPY-2019-01`[1:100,]

##########################################################################
#################               Testy agregacji         ##################
##########################################################################

require(timeSeries)

ciąg <- seq.POSIXt(from = as.POSIXct("2019-01-01 22:05:25.800", tz = "CET", format = "%Y-%m-%d %H:%M:%OS"),
                   to = as.POSIXct("2019-01-01 22:05:30.800", tz = "CET", format = "%Y-%m-%d %H:%M:%OS"),
                   by = as.difftime("00:00:00.001", format = "%H:%M:%OS"))


ts_1 <- timeSeries(data = FX_tick$`AUDJPY-2019-01`$Close[1:100],
                   charvec = FX_tick$`AUDJPY-2019-01`$Date[1:100])

aggregate(ts_1, by = as.timeDate(ciąg, FinCenter ="Warsaw"), FUN = function(x){
  x[length(x)]
}, format = "%Y-%m-%d %H:%M:%OS")

ciąg[1:10]


daty <- as.Date(c("2019-01-01", "2019-01-02", "2019-01-03",
                  "2019-01-04", "2019-01-05", "2019-01-06",
                  "2019-01-20", "2019-01-21", "2019-01-22",
                  "2019-01-23", "2019-01-24", "2019-01-25",
                  "2019-01-26", "2019-01-27", "2019-01-28"))

daty_agg <- as.Date(c("2019-01-01", "2019-01-05", "2019-01-07",
                      "2019-01-12", "2019-01-15", "2019-01-22",
                      "2019-01-28"))

ts_test <- timeSeries(data = 1:length(daty), charvec = daty)
ts_agg <- aggregate(ts_test, by = as.timeDate(daty_agg), FUN = function(x){
  x[length(x)]
})

timeSeries(data = numeric(0), charvec = as.Date(c("2019-01-01", "2019-01-02")))

require(dplyr)

ts_complete <- data.frame(timestamp = as.POSIXct(seq.Date(from = as.Date("2019-01-01"),
                                                          to = as.Date("2019-01-10"),
                                                          by = "day"), tz = "CET"))

ts_incomplete <- data.frame(timestamp = as.POSIXct(as.Date(c("2019-01-03", "2019-01-04",
                                                     "2019-01-06", "2019-01-09")),
                                                   tz = "CET"),
                            dane = c(1, 2, 3, 4), dane2 = c(100, 200, 300, 400))

ts_full <- full_join(ts_complete, ts_incomplete, by = "timestamp")
ts_ts <- as.timeSeries(ts_full, FinCenter = "Warsaw")
na.omit(ts_ts, method = "ie", interp = "before")

mapply(na.omit, ts_ts[,1], ts_ts[,2], MoreArgs = list(method = c("ie", "ie"),
                                                interp = c("before", "before")))

mapply(function(x, y) {x*y}, c(1, 2), c(5, 6))

fapply(ts_ts, FUN = na.omit, method = list("ie", "ie"), interp = list("before", "before"))



do.call(cbind, args = list(timeSeries(data = c(1, 2, 3), charvec = as.Date(c("2019-01-01",
                                                                             "2019-01-02",
                                                                             "2019-01-03"))),
                           timeSeries(data = c(4, 5, 6, 7), charvec = as.Date(c("2019-01-02",
                                                                            "2019-01-03",
                                                                            "2019-01-04",
                                                                            "2019-01-05"))),
                           timeSeries(data = c(8, 9), charvec = as.Date(c("2019-01-05",
                                                                          "2019-01-06")))))


### Po napisaniu własnej funkcji
setwd("C:/Dane/Dokumenty/Studia/Praca magisterska/Projekt R DTW/R_DTW_Project/R")
source("Functions.R")
require(timeSeries)
require(dplyr)

tsToTestDates <- as.POSIXct(c(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-01-10"),
                            by = "day"),
                   seq.Date(from = as.Date("2019-01-15"), to = as.Date("2019-01-20"),
                            by = "day"),
                   seq.Date(from = as.Date("2019-01-25"), to = as.Date("2019-01-31"),
                            by = "day")), tz = "CET")

startData <- data.frame(timestamp = tsToTestDates, price = rnorm(length(tsToTestDates), 5, 0.5),
                         volume = rnorm(length(tsToTestDates), 1000, 100))

datesAll <- as.POSIXct(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-01-31"),
                       by = "day"), tz = "CET")

datesAllDF <- data.frame(timestamp = datesAll)

startingTS <- as.timeSeries(full_join(datesAllDF, startData, by = "timestamp"),
                            FinCenter = "Warsaw")

tsNaFilled <- timeSeriesApply(startingTS, funToApply = na.omit, args = list(
  method = c("ie", "z"),
  interp = c("before")
))

datesToAggregate <- as.timeDate(c("2019-01-05", "2019-01-10",
                                  "2019-01-15", "2019-01-20",
                                  "2019-01-25", "2019-01-31"))

tsAggregate <- timeSeriesApply(tsNaFilled, aggregate,
                               args = list(by = list(datesToAggregate),
                                           FUN = c(function(x){
                                             x[length(x)]
                                           },
                                           function(x){
                                             length(x)
                                           })))

######################################################################
################## Godziny handlu na FOREX - czas GMT ################
######################################################################

head(GPW_tick$WIG20)

GPW_tick_ts <- timeSeries(data = cbind(GPW_tick$WIG20$Time, GPW_tick$WIG20$Time),
                          charvec = strptime(GPW_tick$WIG20$Day,
                                             format = "%Y%m%d"), FinCenter = "Warsaw")

datesAgg <- unique(time(GPW_tick_ts))

GPW_tick_ts_agg <- timeSeriesApply(GPW_tick_ts, funToApply = aggregate,
                                   args = list(by = list(datesAgg),
                                               FUN = c(function(x) {min(x)},
                                                       function(x) {max(x)})))

GPW_tick_ts_agg
write.csv2(as.data.frame(GPW_tick_ts_agg),
           file = "C:/Dane/Dokumenty/Studia/Praca magisterska/WIG20TimeAgg.csv")



###############################################################################################
###                                   Funkcje RCPP                                          ###
###############################################################################################

require(Rcpp)
Rcpp::compileAttributes()

setwd("C:/Dane/Dokumenty/Studia/Praca magisterska/Projekt R DTW/R_DTW_Project")
Rcpp::sourceCpp("znak.cpp")


vectorQuadR <- function(x, a, b, d){
  (a*(x^2)) + (b*x) + d
}

vectorQuadR(c(1, 2, 3), 2, 2, 2)

x <-rnorm(10000)
a <- 2
b <- 2
d <- 3

microbenchmark::microbenchmark(vectorQuadR(x, a, b, d), 
                               vectorQuadLoop(x, a, b, d), 
                               vectorQuadSugar(x, a, b, d),
                               times = 100000L)


vignette(package = "Rcpp")


dlugoscPodlist(list(c(1, 2, 3), c(1, 2), c("a"), c(1, 2, 3, 4, 5)))
microbenchmark::microbenchmark(dlugoscPodlist(list(c(1, 2, 3), c(1, 2), c("a"), c(1, 2, 3, 4, 5))),
                               dlugoscPodlistPodrecznik(list(c(1, 2, 3), c(1, 2), c("a"), c(1, 2, 3, 4, 5))),
                               times = 1000000)



MyLapply(list(rnorm(100), rnorm(200), rnorm(1000)),
         sum)

MyLapplyR <- function(x, f){
  n <- length(x)
  res <- vector(mode = "list", length = n)
  
  for(i in 1:n){
    res[[i]] <- f(x[[i]])
  }
  
  return(res)
}

a <- rnorm(100)
b <- rnorm(200)
d <- rnorm(1000)

microbenchmark::microbenchmark(MyLapplyR(list(a, b, d),
                                         sum),
                               MyLapply(list(a, b, d),
                                        sum),
                               lapply(list(a, b, d),
                                      sum),
                               times = 20000)

a <- c(1, 2, 3)
names(a) <- c("jeden", "dwa", "trzy")


fa <- factor(sample(c("a", "b", "c"),
                    size = 1000, replace = T))

table(fa)

MyTable(fa)

MyTableR <- function(x){
  lev <- levels(x)
  nlev <- length(lev)
  nfac <- length(x)
  res <- integer(nlev)
  
  for(i in 1:nfac)
    res[x[i]] <- res[x[i]] + 1
  
  names(res) <- lev
  
  return(res)
}

MyTableR(fa)

microbenchmark::microbenchmark(MyTableR(fa), MyTable(fa), table(fa), times = 10000)
table

Rcpp::sourceCpp("kolejki.cpp")

n <- 100000
system.time({
  q1 <- queue_create()
  for(i in 1:n){
    wektor <- rnorm(100)
    queue_push(q1, wektor)
  }
}
)

system.time({
  l1 <- list()
  for(i in 1:n){
    wektor <- rnorm(100)
    l1[[length(l1)+1]] <- wektor
  }
}
)

q1
warnings()

q1

require(dtw)

?dtw

class(symmetric2)
str(symmetric1)
unclass(symmetric1)
mvmStepPattern(100)
mvmStepPattern(20)
unclass(mvmStepPattern(20))

Rcpp::sourceCpp("znak.cpp")
ts_1 <- c(1, 1, 2, 3, 2, 0)
ts_2 <- c(0, 1, 1, 2, 3, 2, 1)
a <- dist(ts_1, ts_2)^2
ts_1 <- cumsum(rnorm(100, 0, 1))
ts_2 <- cumsum(rnorm(200, 0, 1))
a <- dist(ts_2, ts_1, method = "euclidean")
?dtw
dtw_norm <- dtw(a, distance.only = F, step.pattern = symmetric1)
dtw_norm$distance
dtw_norm$index1
dtw_norm$index2
DTWR <- MyDtwRcpp(a)
DTWR[[1]]

length(DTWR[[2]])
length(dtw_norm$index1)

ind_1 <- cbind(dtw_norm$index1, dtw_norm$index2)
ind_2 <- cbind(unlist(lapply(DTWR[[2]], function(x) {x[1]}))[length(DTWR[[2]]):1],
               unlist(lapply(DTWR[[2]], function(x) {x[2]}))[length(DTWR[[2]]):1])
dtw_norm$index1
unlist(lapply(DTWR[[2]], function(x) {x[1]}))[length(DTWR[[2]]):1]

accumulatedCostMatrix(a)[1:20, 1:20]

system.time(dtw(a, distance.only = T, step.pattern = symmetric1)$distance)
system.time(MyDtwRcpp(a))


accumulatedCostMatrix(matrix(1:4, 2, 2))

sum(a[cbind(dtw_norm$index1, dtw_norm$index2)])
dtw_norm$distance

par(mfrow = c(2, 1))
plot(ts_2, type = "l", ylim = c(min(ts_1, ts_2), max(ts_1, ts_2)))
lines(ts_1, col = "green")
for(i in 1:nrow(ind_1)){
  lines(c(ind_1[i,1], ind_1[i,2]),
        c(ts_1[ind_1[i,1]], ts_2[ind_1[i,2]]), col = "red")
}

plot(ts_2, type = "l", ylim = c(min(ts_1, ts_2), max(ts_1, ts_2)))
lines(ts_1, col = "green")
for(i in 1:nrow(ind_2)){
  lines(c(ind_2[i,1], ind_2[i,2]),
        c(ts_1[ind_2[i,1]], ts_2[ind_2[i,2]]), col = "red")
}

lines(c(20, 60), c(0, 5), col = "red")
a[1:2, 1:2]
dtw
a[1:5, 1:5]

dtw:::globalCostMatrix(a)$costMatrix[1:10, 1:10]
accumulatedCostMatrix(a)[1:10, 1:10]


require(timeSeries)

?timeSeries

myTS = timeSeries(data = matrix(1:10, ncol = 2),
                  charvec = seq.Date(from = as.Date("2019-01-01"),
                                     by = "day", length.out = 5))

str(myTS)
unclass(myTS)

myTS@positions
as.POSIXct(myTS@positions)
class(myTS@positions)
as.POSIXct(myTS@positions, origin = as.Date("1970-01-01"), tz = "GMT")












timeSeriesToAggregate[1:10,]
timeSeriesApply(timeSeriesToAggregate,
                funToApply = aggregate,
                args = list(by = list(TimeParsed),
                            FUN = funAggBy))

tsToHandle = timeSeriesToAggregate
funToApply = aggregate

function(tsToHandle, funToApply, args){
  
  argsLengths <- lapply(args, length)
  tsVarLength <- ncol(tsToHandle)
  
  if(!all(argsLengths == tsVarLength))
    args <- lapply(args, rep, length.out = tsVarLength)
  
  timeSeriesSplitted <- vector("list", length = tsVarLength)
  timeSeriesAfterApply <- vector("list", length = tsVarLength)
  
  for(i in 1:tsVarLength){
    timeSeriesSplitted[[i]] <- tsToHandle[,i]
  }
  
  timeSeriesSplitted[[2]][1:10,]
  
  i <- 1
  
  for(i in 1:tsVarLength){
    addsArgs <- lapply(args, function(x, i) {
      x[[i]]
    }, i = i)
    
    allArgs <- c(x = timeSeriesSplitted[i], addsArgs)
    
    timeSeriesAfterApply[[i]] <- do.call(timeSeries::aggregate, allArgs)
  }
  
  res <- do.call(cbind, timeSeriesAfterApply)
  return(res)
  
}


ts_test <- timeSeries::aggregate(x = allArgs$x, by = addsArgs$by,
                                 FUN = addsArgs$FUN)

ts_test[1:10,]
allArgs$x[1:900,]

FXtickAgg@.Data

par(mfrow = c(2, 1))
plot(FXtickAgg$ClosePrice[1000:5000], type = "l")
plot(FXtickAgg$pseudoVolume[1000:5000], type = "l")

FXtickAgg[1:100,]

a <- list(a = 1, b = 2)

R.version.string


subsequencesMatrix(1:2, 1)


a <- asSubsequence(FXDayAgg, 5)
SDP <- new("ShapeDescriptorParams", Type = "compound", 
           Descriptors = c("RawSubsequence","derivativeDescriptor"), Additional_params = list(Weights = c(1, 1)))
b <- asShapeDescriptor(a@Subsequences$closePrice, SDP)
b
