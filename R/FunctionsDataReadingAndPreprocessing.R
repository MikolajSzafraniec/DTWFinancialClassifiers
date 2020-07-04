# Funkcja ładująca dane z wybranego folderu i przetwarzająca je na podstawie wybranego
# parametru - GPW dzienne, GPW tickowe, Forex dzienne lub Forex tickowe

load_financial_data <- function(folder_path, data_type = c("GPW_daily", "GPW_tick",
                                                           "FOREX_daily", "FOREX_tick"),
                                include_columns = c("Date", "Close", "Volume"),
                                include_all = F, first_header = NULL,
                                file_col_names = NULL){
  
  currentPath <- getwd()
  pathAbsolute <- isAbsolutePath(folder_path)
  
  if(pathAbsolute){
    setwd(folder_path)
  }else{
    setwd(paste(currentPath, folder_path, sep = "/"))
  }
  
  d_type <- match.arg(data_type)
  files_name <- list.files(getwd())

  fin_inst_names <- str_extract(files_name, pattern = "[^\\.]+")
  
  # Parametr wskazujący, czy w pierwszym wierszu pliku znajdują się nazwy kolumn
  if(is.null(first_header)){
    first_header <- is.element(d_type, c("FOREX_daily", "GPW_daily"))
  }
  
  # Nazwy kolumn w zależności od typu danych
  if(is.null(file_col_names)){
    file_col_names <- list(GPW_daily = c("Instrument", "Date", "Open", "High", "Low",
                                         "Close", "Volume"),
                           FOREX_daily = c("Instrument", "Date", "Open", "High", "Low",
                                           "Close", "Volume"),
                           FOREX_tick = c("Instrument", "Date", "Open", "Close"),
                           GPW_tick = c("Instrument", "ControlCol", "Day", "Time",
                                        "Open","High", "Low", "Close", "Volume",
                                        "ControlCol2"))
    
  }else{
    file_col_names <- list(d_type = file_col_names)
  }
  
  # Załadowanie danych z plików tekstowych do listy wypełnionej ramkami danych 
  data_list <- lapply(files_name, function(x, col_n, first_header, data_type,
                                           include_columns){
    
    tab <- read.table(file = x, header = first_header, sep = ",",
                      dec = ".", stringsAsFactors = F)
    
    colnames(tab) <- col_n
    
    options(digits.secs = 3)
    
    if(data_type == "GPW_tick"){
      tab <- tab %>%
        mutate(Time = ifelse(nchar(Time) == 5,
                             paste("0", Time, sep = ""),
                             Time))
    }
    
    
    
    # Przetworzenie kolumn z datą do odpowiedniego formatu
    tab <- switch(data_type,
                  GPW_daily = tab %>%
                    mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")),
                  FOREX_daily = tab %>%
                    mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")),
                  FOREX_tick = tab %>%
                    mutate(Date = as.POSIXct(as.character(Date), 
                                             format = "%Y%m%d %H:%M:%OS", tz = "GMT")),
                  GPW_tick = tab %>%
                    mutate(Date = as.POSIXct(paste(Day, Time, sep = " "),
                                             format = "%Y%m%d %H%M%S", tz = "GMT")))
    
    if(include_all)
      res <- tab
    else
      res <- tab[,include_columns]
    
    #res <- tab[,include_columns]
    return(res)
    
  }, col_n = file_col_names[[d_type]], first_header = first_header,
  data_type = d_type, include_columns = include_columns)
  
  names(data_list) <- fin_inst_names
  
  setwd(currentPath)
  return(data_list)
}

##############################################################################################
###           Funkcja aplikująca wybraną funkcję dla każdej zmiennej szeregu czasowego,    ###
###             z możliwością wykorzystania innych argumentów dla każdej ze zmiennych.     ###
###             Argumenty dodatkowe (args) muszą być nazwane. Jeśli jeden z argumentów     ###
###              nie jest skalarem lecz wektorem powinien być umieszczony jako osobna      ###
###                                             podlista.                                  ###
##############################################################################################

timeSeriesApply <- function(tsToHandle, funToApply, args){
  
  argsLengths <- lapply(args, length)
  tsVarLength <- ncol(tsToHandle)
  
  if(!all(argsLengths == tsVarLength))
    args <- lapply(args, rep, length.out = tsVarLength)
  
  timeSeriesSplitted <- vector("list", length = tsVarLength)
  timeSeriesAfterApply <- vector("list", length = tsVarLength)
  
  for(i in 1:tsVarLength){
    timeSeriesSplitted[[i]] <- tsToHandle[,i]
  }
  
  i <- 1
  
  for(i in 1:tsVarLength){
    addsArgs <- lapply(args, function(x, i) {
      x[[i]]
    }, i = i)
    
    allArgs <- c(timeSeriesSplitted[i], addsArgs)
    
    timeSeriesAfterApply[[i]] <- do.call(funToApply, allArgs)
  }
  
  res <- do.call(cbind, timeSeriesAfterApply)
  return(res)
  
}

##############################################################################################
###    Funkcja generująca dla danych typu FX tick odpowiedni szereg czasowy do agregacji   ###
###                danych w oparciu o wybraną deltę (ilość minut / sekund)                 ###
##############################################################################################

parseTimeFXTick <- function(FXTickDateTime, delta = dminutes(5)){
  
  stopifnot(delta < dminutes(60))
  
  FXDayHours <- unique(strftime(FXTickDateTime, tz = "GMT",
                           format = "%Y-%m-%d %H"))
  
  start <- as.POSIXct("1970-01-01 00:00:00", tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  end <- as.POSIXct("1970-01-01 01:00:00", tz = "GMT", format = "%Y-%m-%d %H:%M:%S") -
    delta
  
  minsecSequence <- strftime(seq(from = start, to = end, by = delta), "%M:%S")
  
  allDatesCombinations <- expand.grid(FXDayHours, minsecSequence)
  
  allDatesCombPasted <- paste(allDatesCombinations$Var1, allDatesCombinations$Var2,
                              sep = ":")
  
  res <- sort(strptimeDate(allDatesCombPasted, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
  return(res)
}

###########################################################################################
###      Funkcja agregująca szereg typu FXtick (cena zamknięcia + pseudowolumen)        ###
###            zgodnie z okreslonym interwałem i zastępująca braki danych               ###
###########################################################################################

FXTickAggregateAndFillNA <- function(FXTickData, delta = dminutes(5),
                                     funAggBy = c(function(x){
                                       x[length(x)]
                                     },
                                     function(x){
                                       length(x)
                                     }), argMethods = c("ie", "z"),
                                     argInterp = c("before")){
  
  TimeParsed <- parseTimeFXTick(FXTickData$Date, delta = delta)
  dfTimeParsed <- data.frame(time = TimeParsed)
  colnames(dfTimeParsed) <- "timestamp"
  
  timeSeriesToAggregate <- timeSeries(data = cbind(FXTickData$Close,
                                                   FXTickData$Close),
                                      charvec = FXTickData$Date)
  
  timeSeriesAfterAggregate <- timeSeriesApply(timeSeriesToAggregate,
                                              funToApply = aggregate,
                                              args = list(by = list(TimeParsed),
                                                          FUN = funAggBy))
  
  dfTimeSeriesAfterAggregate <- data.frame(timestamp = time(timeSeriesAfterAggregate),
                                           ClosePrice = timeSeriesAfterAggregate$TS.1,
                                           pseudoVolume = timeSeriesAfterAggregate$TS.2)
  colnames(dfTimeSeriesAfterAggregate)[1] <- "timestamp"

  
  fullDFWithNa <- full_join(dfTimeParsed, dfTimeSeriesAfterAggregate,
                                    by = "timestamp")
  
  fullTimeSeriesWithNa <- timeSeries(data = fullDFWithNa[,c(2,3)],
                                     charvec = fullDFWithNa$timestamp)
  
  fullTimeSeriesNaOmitted <- timeSeriesApply(fullTimeSeriesWithNa,
                                             funToApply = na.omit,
                                             args = list(method = argMethods,
                                                         interp = argInterp))

  fullTimeSeriesNaOmitted
}

#############################################################################################
###          Funkcja agregująca szereg typu GPWtick (cena zamknięcia + wolumen)           ###
###              zgodnie z okreslonym interwałem i zastępująca braki danych               ###
#############################################################################################

# setwd("C:/Dane/Dokumenty/Studia/Praca magisterska/Projekt R DTW/R_DTW_Project/R")
# source(GPW_hours.R)

GPWTickAggregateAndFillNA <- function(GPWTickData, patternDatesToAgg,
                                      funAggBy = c(function(x){
                                        x[length(x)]
                                      },
                                      function(x){
                                        sum(x)
                                      }), argMethods = c("ie", "z"),
                                      argInterp = c("before")){
  
  minTime <- min(GPWTickData$Date)
  maxTime <-max(GPWTickData$Date)
  timeFilter <- (patternDatesToAgg > minTime) & (patternDatesToAgg < maxTime)
  patternDatesToAgg <- as.timeDate(patternDatesToAgg[timeFilter], FinCenter = "London")
  dfTimeParsed <- data.frame(time = patternDatesToAgg)
  colnames(dfTimeParsed) <- "timestamp"
  
  timeSeriesToAggregate <- timeSeries(data = cbind(GPWTickData$Close,
                                                   GPWTickData$Volume),
                                      charvec = GPWTickData$Date, tz = "GMT", FinCenter = "London")
  
  timeSeriesAfterAggregate <- timeSeriesApply(timeSeriesToAggregate,
                                              funToApply = aggregate,
                                              args = list(by = list(timeDate(dfTimeParsed$timestamp)),
                                                          FUN = funAggBy))
  
  dfTimeSeriesAfterAggregate <- data.frame(timestamp = time(timeSeriesAfterAggregate),
                                           ClosePrice = timeSeriesAfterAggregate$TS.1,
                                           Volume = timeSeriesAfterAggregate$TS.2)
  
  colnames(dfTimeSeriesAfterAggregate)[1] <- "timestamp"
  
  fullDFWithNa <- full_join(dfTimeParsed, dfTimeSeriesAfterAggregate,
                            by = "timestamp")
  
  fullTimeSeriesWithNa <- timeSeries(data = fullDFWithNa[,c(2,3)],
                                     charvec = fullDFWithNa$timestamp)
  
  
  fullTimeSeriesNaOmitted <- timeSeriesApply(fullTimeSeriesWithNa,
                                             funToApply = na.omit,
                                             args = list(method = argMethods,
                                                         interp = argInterp))
  
  fullTimeSeriesNaOmitted
}
######################################################################################
###   Funkcja wyciągająca niezbędne zmienne z danych typu GPW_day i zamieniająca   ###
###                        je w szereg czasowych (klasa timeSeries)                ###  
######################################################################################

GPWDailyParse <- function(GPWDailyData){
  
  GPWDailyData <- GPWDailyData %>%
    dplyr::rename(closePrice = Close)
  
  results <- timeSeries(data = GPWDailyData[,c("closePrice", "Volume")],
                        charvec = GPWDailyData$Date)
  return(results) 
}


######################################################################################
###   Funkcja wyciągająca niezbędne zmienne z danych typu FX_day i zamieniająca    ###
###                        je w szereg czasowych (klasa timeSeries)                ###  
######################################################################################

FXDailyParse <- function(FXDailyData){
  
  FXDailyData <- FXDailyData %>%
    mutate(Variability = ifelse(!is.nan((Close - Open) / (High - Low)),
                                        (Close - Open) / (High - Low),
                                        0)) %>%
    dplyr::rename(closePrice = Close)
  
  results <- timeSeries(data = FXDailyData[,c("closePrice", "Variability")],
                        charvec = FXDailyData$Date)
  return(results) 
}


##########################################################################################
### Skonstruowanie ciągu dat na podstawie serii wzorcowej (np. indeksu WIG lub WIG20)  ###
###            stanowiącego podstawę agregacji serii typu GPW Tick                     ###
##########################################################################################

# patternPath <- "C:/Dane/Dokumenty/Studia/Praca magisterska/Dane/PatternSeries"
# patternGPWTickData <- load_financial_data(patternPath, data_type = "GPW_t", include_all = T)
parsePatternGPWTickTime <- function(patternGPWData, delta = dminutes(1), 
                                    playOffTime = dminutes(15), roundingUnit = "minute"){
  
  originTS <- timeSeries(data = cbind(patternGPWData$Time, patternGPWData$Time),
                         charvec = strptime(patternGPWData$Day,
                                            tz = "GMT", format = "%Y%m%d"), FinCenter = "London")
  
  daysToAgg <- unique(time(originTS))  
  originTSAgg <- timeSeriesApply(originTS, funToApply = aggregate,
                                 args = list(by = list(daysToAgg),
                                             FUN = c(function(x) {min(x)},
                                                     function(x) {max(x)})))
  
  originTSAggDF <- data.frame(Date = time(originTSAgg),
                              hourStart = originTSAgg$TS.1,
                              hourEnd = originTSAgg$TS.2,
                              stringsAsFactors = F)
  
  colnames(originTSAggDF) <- c("Date", "hourStart", "hourEnd")
  
  dayHourList <- apply(originTSAggDF, MARGIN = 1, FUN = function(x, playOffTime, roundingUnit,
                                                                 delta){
    
    hourStartParsed <- strptime(x[2], format = "%H%M%S", tz = "GMT")
    hourEndParsed <- strptime(x[3], format = "%H%M%S", tz = "GMT")
    hourEndParsedPleyOffIncluded <- hourEndParsed - playOffTime
    
    if(hourEndParsedPleyOffIncluded <= hourStartParsed)
      hourEndParsedPleyOffIncluded <- hourEndParsed
    
    hourStartRound <- floor_date(hourStartParsed, unit = roundingUnit)
    hourEndRound <- ceiling_date(hourEndParsedPleyOffIncluded, unit = roundingUnit)
    dailySeq <- seq.POSIXt(from = hourStartRound, to = hourEndRound,
                    by = delta)

    dailySeqFormat <- strftime(dailySeq, format = "%H:%M:%S", tz = "GMT")
    lenSeq <- length(dailySeqFormat)
    
    dayRep <- rep(strftime(x[1], "%Y-%m-%d", tz = "GMT"), times = lenSeq)
    
    timePasted <- paste(dayRep, dailySeqFormat, sep = " ")
    timeResult <- strptime(timePasted, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    return(timeResult)
    
  }, playOffTime = playOffTime, roundingUnit = roundingUnit, delta = delta)
  
  res <- do.call(c, dayHourList)
  res <- with_tz(res, tzone = "GMT")
  res
}

