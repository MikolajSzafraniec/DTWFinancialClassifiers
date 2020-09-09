# Plot 1: CCC prices vs random walk
GPW_daily <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_daily_parsed.rds")

set.seed(5)
random_walk <- cumsum(rnorm(500))+GPW_daily$CCC$closePrice[3001]

par(mfrow = c(2, 1))

plot(GPW_daily$CCC$closePrice[3001:3500], type = "l", ylab = "value", xlab = "", main = "CCC price")
plot(random_walk, type = "l", ylab = "value", xlab = "", main = "Random walk")

# Plot 2: KGHM heteroscedastic price changes variance
require(ggplot2)
GPW_daily_parsed <- readRDS("Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_daily_parsed.rds")

KGHM_data <- data.frame(date = as.Date(time(GPW_daily_parsed$KGHM)[1:2700]),
                        ClosePrice = GPW_daily_parsed$KGHM$closePrice[1:2700])

ggplot(KGHM_data, aes(x = date, y = ClosePrice)) +
  geom_line() +
  labs(title = "KGHM close prices", 
       subtitle  = "10.07.1997 - 18.04.2008") +
  ylab("Close price") + xlab("Date")

# Plot 3: Differences between normal and observed distribution curve of stock returns
require(gridExtra)

Orlen_length <- nrow(GPW_daily_parsed$KGHM)
Orlen_returns <- log(GPW_daily_parsed$KGHM$closePrice[2:Orlen_length] /
                       GPW_daily_parsed$KGHM$closePrice[1:(Orlen_length-1)])
Orlen_sd <- sd(Orlen_returns)
Orlen_mean <- mean(Orlen_returns)

Sniezka_length <- nrow(GPW_daily_parsed$SNIEZKA)
Sniezka_returns <- log(GPW_daily_parsed$SNIEZKA$closePrice[2:Sniezka_length] /
                        GPW_daily_parsed$SNIEZKA$closePrice[1:(Sniezka_length-1)])
Sniezka_sd <- sd(Sniezka_returns)
Sniezka_mean <- mean(Sniezka_returns)

Orlen_dens <- density(Orlen_returns)
Sniezka_dens <- density(Sniezka_returns)

Orlen_norm_dens <- dnorm(
  x = seq(from = min(Orlen_dens$x), to = max(Orlen_dens$x), length.out = length(Orlen_dens$x)), 
  sd = Orlen_sd, 
  mean = Orlen_mean
)

Sniezka_norm_dens <- dnorm(
  x = seq(from = min(Sniezka_dens$x), to = max(Sniezka_dens$x), length.out = length(Sniezka_dens$x)), 
  sd = Sniezka_sd, 
  mean = Sniezka_mean
)

plot_df <- data.frame(
  orlen_x <- Orlen_dens$x,
  Sniezka_x <- Sniezka_dens$x,
  Sniezka_y <- Sniezka_dens$y,
  orlen_y <- Orlen_dens$y,
  orlen_norm <- Orlen_norm_dens,
  Sniezka_norm <- Sniezka_norm_dens
)

orlen_plot <- ggplot(plot_df, aes(orlen_x)) + 
  geom_line(aes(y = orlen_y, colour = "PKN Orlen log returns")) + 
  geom_line(aes(y = orlen_norm, colour = "Normal curve")) + 
  labs(title = "PKN Orlen log returns density vs normal density", 
       subtitle  = paste(
         strftime(min(time(GPW_daily_parsed$KGHM)), "%d.%m.%Y"), " - ", 
         strftime(max(time(GPW_daily_parsed$KGHM)), "%d.%m.%Y"))) +
  ylab("Density") + xlab("Log return")

Sniezka_plot <- ggplot(plot_df, aes(Sniezka_x)) + 
  geom_line(aes(y = Sniezka_y, colour = "Śnieżka log returns")) +
  geom_line(aes(y = Sniezka_norm, colour = "Normal curve")) +
  labs(title = "Śnieżka log returns density vs normal density", 
       subtitle  = paste(
         strftime(min(time(GPW_daily_parsed$SNIEZKA)), "%d.%m.%Y"), " - ", 
         strftime(max(time(GPW_daily_parsed$SNIEZKA)), "%d.%m.%Y"))) +
  ylab("Density") + xlab("Log return")


grid.arrange(grobs = list(orlen_plot, Sniezka_plot), nrow = 2)

# Plot 4: Returns, squared return and ACF of squared returns

GPW_daily_parsed <- readRDS(file = "Data/EuclidPreproSingleStock/GPWTickSetsProcessed/GPW_daily_parsed.rds")
CCC_len <- length(GPW_daily_parsed$CCC$closePrice)

CCC_log_returns <- log(GPW_daily_parsed$CCC$closePrice[2:CCC_len] /
                                 GPW_daily_parsed$CCC$closePrice[1:(CCC_len-1)])

squared_acf <- acf(CCC_log_returns^2, lag.max = 100)
ci <- qnorm((1 + 0.95)/2)/sqrt(squared_acf$n.used)

plot_df_returns <- data.frame(
  date = as.Date(time(GPW_daily_parsed$CCC)[2:CCC_len]),
  log_returns = CCC_log_returns,
  squared_returns = CCC_log_returns^2
)

plot_df_acf <- data.frame(
  squared_return_acf = as.vector(squared_acf$acf),
  lag_num = as.vector(squared_acf$lag)
)

plot_log_returns <- ggplot(plot_df_returns, aes(x = date, y = log_returns)) +
  geom_line() +
  labs(title = "CCC daily log returns", 
       subtitle  = paste(
         strftime(min(time(GPW_daily_parsed$CCC)), "%d.%m.%Y"), " - ", 
         strftime(max(time(GPW_daily_parsed$CCC)), "%d.%m.%Y"))) +
  ylab("Log return") + xlab("Date")

plot_squared_returns <- ggplot(plot_df_returns, aes(x = date, y = squared_returns)) +
  geom_line() +
  labs(title = "CCC daily squared returns", 
       subtitle  = paste(
         strftime(min(time(GPW_daily_parsed$CCC)), "%d.%m.%Y"), " - ", 
         strftime(max(time(GPW_daily_parsed$CCC)), "%d.%m.%Y"))) +
  ylab("Squared return") + xlab("Date")

ACF_plot <- ggplot(plot_df_acf, aes(x = lag_num, y = squared_return_acf)) +
  geom_bar(stat = "identity", fill="darkblue", color = "white") +
  #theme_dark() +
  ylim(-0.05, 1) +
  geom_hline(yintercept=ci, linetype="dashed", color = "red", size = 0.5) +
  geom_hline(yintercept=-ci, linetype="dashed", color = "red", size = 0.5) +
  labs(title = "CCC ACF function of daily squared returns", 
       subtitle  = paste(
         strftime(min(time(GPW_daily_parsed$CCC)), "%d.%m.%Y"), " - ", 
         strftime(max(time(GPW_daily_parsed$CCC)), "%d.%m.%Y"))) +
  ylab("ACF") + xlab("Lag")

grid.arrange(grobs = list(plot_log_returns, plot_squared_returns, ACF_plot), nrow = 3)

# Plot 5: WIG informatyka during info-bubble
WIG_info <- load_financial_data("Data/GPW day/", data_type = "GPW_d")$`WIG-INFO`
WIG_info_parsed <- GPWDailyParse(WIG_info)

WIG_info_plot_df <- WIG_info_parsed %>%
  as.data.frame(.) %>%
  mutate(date = as.Date(rownames(.))) %>%
  dplyr::filter(date < as.Date("2003-01-01"),
                date > as.Date("1997-12-31"))


WIG_info_plot <- ggplot(WIG_info_plot_df, aes(x = date, y = closePrice)) +
  geom_line() +
  labs(title = "WIG-informatyka", 
       subtitle  = paste(
         strftime(min(WIG_info_plot_df$date), "%d.%m.%Y"), " - ", 
         strftime(max(WIG_info_plot_df$date), "%d.%m.%Y"))) +
  ylab("Value") + xlab("Date")

WIG_info_plot

# Plot 6: Delta airlanes after 11 September 2011

library(plotly)
library(quantmod)

DAL <- read.csv2("../MagisterkaTekst/Ilustracje/DeltaAirlines.csv", stringsAsFactors = F)
Sys.setlocale("LC_TIME", "English")

DAL <- DAL %>%
  dplyr::mutate(Date = as.POSIXct(strptime(Date, format = "%B %d, %Y"))) #%>%
 # filter(Date < as.Date("2001-11-01"))
Sys.setlocale("LC_TIME", "Polish_Poland.1250")

# fig <- DAL %>%
#   plot_ly(x = ~Date, type = "candlestick",
#           open = ~DAL$Open, close = ~DAL$Close,
#           high = ~DAL$High, low = ~DAL$Low) %>%
#   layout(title = "Delta Airlines prices",
#          xaxis = list(rangeslider = list(visible = F)))
# 
# fig
# PlotCandlestick(x=DAL$Date, y=as.matrix(DAL[,c("Open", "High", "Low", "Close")]), border=NA, las=1, ylab="")

DAL_xts <- xts(
  as.matrix(DAL[,c("Open", "High", "Low", "Close")]),
  order.by = DAL$Date
)

candleChart(DAL_xts, multi.col=F,theme='white', up.col = "white", dn.col = "black",
            name = "Delta Airlines")

WIG_data <- read.table(file = "../MagisterkaTekst/Ilustracje/WIG20.mst",
                       header = T, sep = ",", stringsAsFactors = F)

WIG_data_to_plot <- WIG_data %>%
  mutate(date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  filter(date > as.Date("2013-04-25"), date < as.Date("2013-12-30"))

WIG_xts <- xts(
  as.matrix(WIG_data_to_plot[,c("X.OPEN.", "X.HIGH.", "X.LOW.", "X.CLOSE.")]),
  order.by = WIG_data_to_plot$date
)

candleChart(WIG_xts, multi.col=F,theme='white', up.col = "white", dn.col = "black",
            name = "WIG20 - OFE reform")

abline(v = 22.1, col = "red", lwd = 1)
text(x = 62, y = 2520, labels = "September 4th 2013", col = "red")

# Plot 6: Southwest Airlines after 11 September attacks
require(quantmod)

# source: https://www.macrotrends.net/stocks/charts/LUV/southwest-airlines/stock-price-history

LUV_data <- read.csv("../Magisterka tekst/Ilustracje/MacroTrends_Data_Download_LUV.csv",
                     stringsAsFactors = F)

LUV_data <- LUV_data %>%
  mutate(date = as.Date(date)) %>%
  dplyr::filter(date >= as.Date("2001-01-15"), date < as.Date("2002-05-01"))

LUV_xts <- xts(
  x = as.matrix(LUV_data[,c("open", "high", "low", "close")]),
  order.by = LUV_data$date
)

Sys.setlocale("LC_TIME", "English")

quantmod::candleChart(
  LUV_xts, name = "Southwest Airlines",
  up.col = "white",
  dn.col = "black",
  theme = "white"
)

# Plot 7: WIG 20 after OFE reform

WIG_20_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/WIG20.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

WIG_20_data <- WIG_20_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2013-08-20"), Date < as.Date("2013-12-31"))

WIG_20_xts <- xts(
  x = as.matrix(WIG_20_data[,c("X.OPEN.", "X.HIGH.",  "X.LOW.", "X.CLOSE.")]),
  order.by = WIG_20_data$Date
)

quantmod::candleChart(
  WIG_20_xts, name = "WIG20",
  up.col = "white",
  dn.col = "black",
  theme = "white"
)

abline(v = 31, col = "red")
text("September 4th 2013", x = 72, y = 2520, col = "red", cex = 0.8)

# Plot 10 classic candlestick plot
library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~AAPL.Open, close = ~AAPL.Close,
                      high = ~AAPL.High, low = ~AAPL.Low) 
fig <- fig %>% layout(title = "Basic Candlestick Chart",
                      xaxis = list(rangeslider = list(visible = F)))

fig


# Plot 11 time series with trends
require(ggplot2)
require(gridExtra)

price_t0 <- 50
returns_growth <- rnorm(100, 0.002, 0.01)
returns_fall <- rnorm(100, -0.002, 0.01)

ts_growth <- price_t0*cumprod(1+returns_growth)
ts_fall <- price_t0*cumprod(1+returns_fall)

plot(ts_growth, type = "l")
plot(ts_fall, type = "l")

df_plot <- data.frame(
  ind = 1:100,
  ts_growth = ts_growth,
  ts_fall = ts_fall
)

plot_growth <- ggplot(df_plot, aes(x = ind, y = ts_growth)) +
  geom_line() +
  labs(title = "Rising trend",
       subtitle = "Expected value = 0.2%, sd = 1%") +
  ylab("Value") + xlab("Index") 

plot_fall <- ggplot(df_plot, aes(x = ind, y = ts_fall)) +
  geom_line() +
  labs(title = "Downward trend",
       subtitle = "Expected value = -0.2%, sd = 1%") +
  ylab("Value") + xlab("Index")



# Plot xx Point and figure plot
library(rpnf) # Load rpnf library
data(DOW) # (Offline) Load free available sample data from https://www.quandl.com/data/WIKI/DOW
pnfdata <- pnfprocessor(
  high=DOW$High,
  low=DOW$Low,
  date=DOW$Date,
  boxsize=1L,
  log=FALSE)
pnfplottxt(pnfdata,boxsize=1L,log=FALSE)

# Plot 19: WIG 20 logarithmic and linear scale

require(dplyr)
require(ggplot2)
require(gridExtra)

CDPROJ_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/CDPROJEKT.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

CDP_data <- CDPROJ_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2005-08-20"), Date < as.Date("2020-12-31"))

CDP_standard_plot <- ggplot(CDP_data, aes(x = Date, y = X.CLOSE.)) +
  geom_line() +
  labs(title = "CD Projekt - standard scale", 
       subtitle  = paste(
         strftime(min((CDP_data$Date)), "%d.%m.%Y"), " - ", 
         strftime(max((CDP_data$Date)), "%d.%m.%Y"))) +
  ylab("") + xlab("Date") 

CDP_log_plot <- ggplot(CDP_data, aes(x = Date, y = X.CLOSE.)) +
  geom_line() +
  labs(title = "CD Projekt - logarithmic scale", 
       subtitle  = paste(
         strftime(min((CDP_data$Date)), "%d.%m.%Y"), " - ", 
         strftime(max((CDP_data$Date)), "%d.%m.%Y"))) +
  ylab("") + xlab("Date") +
  scale_y_log10()

grid.arrange(grobs = list(CDP_standard_plot, CDP_log_plot), ncol = 2)

# Plot 20: CCC glowa i ramiona

require(dplyr)
require(ggplot2)
require(gridExtra)

CCC_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/CCC.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

CCC_d <- CCC_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2000-08-20"), Date < as.Date("2020-12-31"))

CCC_plot <- ggplot(CCC_d, aes(x = Date, y = X.CLOSE.)) +
  geom_line() +
  labs(title = "CCC", 
       subtitle  = paste(
         strftime(min((CCC_d$Date)), "%d.%m.%Y"), " - ", 
         strftime(max((CCC_d$Date)), "%d.%m.%Y"))) +
  ylab("") + xlab("Date") +
  scale_y_log10()

CCC_plot

# Plot 22: glowa i ramiona jako trend + sinusoida

sinusoida <- sin(seq(from = 0, to = 7*pi, length.out = 100))*10
trend <- c(1:36, 35:(-28))
plot(sinusoida+trend, type = "l")

plot(trend, type = "l", ylim = c(-50, 50), xlab = "", ylab = "")
lines(sinusoida, lty = "dashed")
lines(sinusoida+trend, col = "red", lwd = 2)

# Plot 25 Formacja spodka JSW

require(dplyr)
require(ggplot2)
require(gridExtra)

JSW_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/JSW.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

JSW_d <- JSW_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2010-08-20"), Date < as.Date("2017-01-01"))

JSW_plot <- ggplot(JSW_d, aes(x = Date, y = X.CLOSE.)) +
  geom_line() +
  labs(title = "JSW", 
       subtitle  = paste(
         strftime(min((JSW_d$Date)), "%d.%m.%Y"), " - ", 
         strftime(max((JSW_d$Date)), "%d.%m.%Y"))) +
  ylab("") + xlab("Date")

JSW_plot

# Plot 28: CCC glowa i ramiona + wolumen

require(dplyr)
require(quantmod)
require(xts)

CCC_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/CCC.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

CCC_d <- CCC_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2017-07-01"), Date < as.Date("2018-07-01"))

CCC <- xts(x = 
                 data.frame(
                   open = CCC_d$X.OPEN.,
                   high = CCC_d$X.HIGH.,
                   low = CCC_d$X.LOW.,
                   close = CCC_d$X.CLOSE.,
                   volume = CCC_d$X.VOL.
                 ), order.by = CCC_d$Date)

Sys.setlocale("LC_TIME", "English")

quantmod::chartSeries(PGNING, theme="white", TA="addVo();addBBands();addCCI()")

Sys.setlocale("LC_TIME", "Polish_Poland.1250")

# Plot 31: moving averages

require(dplyr)
require(quantmod)
require(xts)

PGNING_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/PGNIG.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

PGNING_d <- PGNING_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2018-01-01"), Date < as.Date("2018-09-01"))

PGNIG <- xts(x = 
             data.frame(
               Open = PGNING_d$X.OPEN.,
               High = PGNING_d$X.HIGH.,
               Low = PGNING_d$X.LOW.,
               Close = PGNING_d$X.CLOSE.,
               Volume = PGNING_d$X.VOL.
             ), order.by = PGNING_d$Date)

Sys.setlocale("LC_TIME", "English")
quantmod::chartSeries(PGNIG, theme="white", type = "b")
quantmod::addSMA(n = 5, col = "black")
quantmod::addSMA(n = 20, col = "red")

# Plot 32: RSI, OBV

require(dplyr)
require(quantmod)
require(xts)

AGORA_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/AGORA.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

AGORA_d <- AGORA_data %>%
  mutate(Date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d"))) %>%
  dplyr::filter(Date > as.Date("2019-01-01"), Date < as.Date("2020-09-01"))

AGORA <- xts(x = 
               data.frame(
                 Open = AGORA_d$X.OPEN.,
                 High = AGORA_d$X.HIGH.,
                 Low = AGORA_d$X.LOW.,
                 Close = AGORA_d$X.CLOSE.,
                 Volume = AGORA_d$X.VOL.
               ), order.by = AGORA_d$Date)

Sys.setlocale("LC_TIME", "English")
quantmod::chartSeries(AGORA, theme="white", type = "b")
addRSI()
addOBV(1)

# Plot 35: Distorted time series

ts_1 <- rep(10, times = 30)
ts_2 <- rep(20, times = 30)

ts_1[10] <- 15
ts_2[15] <- 25

ts_1[20] <- 12.5
ts_2[25] <- 22.5

df_to_plot <- data.frame(
  index = 1:30,
  ts_1 = ts_1, 
  ts_2 = ts_2
)

df_pivoted <- df_to_plot %>%
  tidyr::pivot_longer(cols = c("ts_1", "ts_2"), names_to = "ts")

ggplot2::ggplot(data = df_pivoted,
                aes(x=index, y=value, colour=ts)) +
  geom_line() + ylab("")

# Plot 36 - DTW i Euclid

ts_1 <- rep(10, times = 30)
ts_2 <- rep(20, times = 30)

ts_1[10] <- 15
ts_2[15] <- 25

ts_1[20] <- 12.5
ts_2[25] <- 22.5

dtw_proper <- dtw::dtw(ts_1, ts_2 -10, step.pattern = symmetric1, keep.internals = T)
dtw_standard <- dtw::dtw(ts_1, ts_2, step.pattern = symmetric1, keep.internals = T)


dtwPlotTwoWay(d = dtw_proper, ts_1, ts_2, ylab = "")
dtwPlotTwoWay(d = dtw_standard, ts_1, ts_2, ylab = "")

dtwPlotThreeWay(d = dtw_proper, ylab = "")
dtwPlotDensity(dtw_proper)


# Plot 37 - Accumulated cost matrix visualization
require(pheatmap)

ts_1 <- sample(1:10,10, replace = T)
ts_2 <- sample(1:10,10, replace = T)

g_matrix <- proxy::dist(ts_1, ts_2)
G_matrix <- RcppShapeDTW::RcppAccumulatedCostMatrix(g_matrix)
a <- dtw::dtw(ts_1, ts_2, step.pattern = symmetric1, keep.internals = T, 
              window.type = "sakoechiba", window.size = 2)

pheatmap(G_matrix[10:1,], display_numbers = T, color = colorRampPalette(c('white','red'))(100), 
         cluster_rows = F, cluster_cols = F, fontsize_number = 15,
         xlab = "a")
a$distance

dtwPlotDensity(a)
pheatmap(a$costMatrix[10:1,], display_numbers = T, color = colorRampPalette(c('white','red'))(100), 
         cluster_rows = F, cluster_cols = F, fontsize_number = 15,
         xlab = "a")

a$index1
a$index2

a <- dtw::dtw(ts_1, ts_2, step.pattern = symmetric1, keep.internals = T, 
              window.type = "sakoechiba", window.size = 2)
b <- dtw::dtw(ts_1, ts_2, step.pattern = symmetric1, keep.internals = T, 
              window.type = "sakoechiba", window.size = 100)

dtwPlotTwoWay(a, xts = ts_1, yts = ts_2 +10, ylab = "")
dtwPlotTwoWay(b, xts = ts_1, yts = ts_2 +10, ylab = "")

# Plot 38 Shape Warping and normal warping
SDP_traditional <- new("ShapeDescriptorParams")
SDP_compound <- new("ShapeDescriptorParams",
                    Type = "compound",
                    Descriptors = c("PAADescriptor",
                                    "slopeDescriptor"),
                    Additional_params = list(
                      Weights = c(1, 10),
                      PAAWindow = 3L,
                      slopeWindow = 3L
                    ))

set.seed(38)
ts_ref <- cumsum(rnorm(1000, 0, 1))

ts_1 <- ts_ref[1:150]
ts_2 <- ts_ref[21:170] + rnorm(150, 0, 1)

dtw_standard <- dtw(ts_1, ts_2, step.pattern = symmetric1, keep.internals = T)
dtw_new_indices <- dtw(ts_1, ts_2, step.pattern = symmetric1, keep.internals = T)
dtw_shape <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = t(t(ts_1)), 
                                          testSeries = t(t(ts_2)), 
                                          forecastHorizon = 0, 
                                          subsequenceWidth = 40, 
                                          subsequenceBreaks = 1, 
                                          shapeDescriptorParams = SDP_compound, 
                                          normalizationType = "Zscore", 
                                          distanceType = "Dependent")

dtw_new_indices$index1 <- dtw_shape$ShapeDescriptorsDistanceResults$WarpingPaths$WarpingPaths_0[,1]
dtw_new_indices$index2 <- dtw_shape$ShapeDescriptorsDistanceResults$WarpingPaths$WarpingPaths_0[,2]

dtwPlotTwoWay(dtw_standard, ts_1, ts_2 + 30, ylab = "")
dtwPlotTwoWay(dtw_new_indices, ts_1, ts_2 + 30, ylab = "")

##################################################################################
###                                   CHAPTER 5                                ###
##################################################################################

# Plot 1 Simple Rcpp vs R comparison

require(Rcpp)

Rcpp::cppFunction(
  code = 
    "NumericMatrix RcppDistanceMatrix(NumericVector x, NumericVector y){
      int len_x = x.length();
      int len_y = y.length();
      
      NumericMatrix res(len_x, len_y);
      
      for(int i = 0; i < len_x; i++){
        for(int j = 0; j < len_y; j++){
          res(i, j) = pow(pow(x(i) - y(j), 2), 0.5);
        }
      }
      
      return(res);
    }"
)


RDistMatrix <- function(x, y){
  
  len_x <- length(x)
  len_y <- length(y)
  
  res <- matrix(0, nrow = len_x, ncol = len_y)
  
  for(i in 1:len_x){
    for(j in 1:len_y){
      res[i, j] <- sqrt((x[i]-y[j])^2)
    }
  }
  
  res
}

ts_example_1 <- purrr::map(2:100, rnorm)
ts_example_2 <- purrr::map(2:100, rnorm)

time_Rcpp <- numeric(99)
time_R <- numeric(99)

for(i in 1:99){
  cat("Proceeding ts number ", i, "\n")
  
  mcb <- microbenchmark::microbenchmark(RcppDistanceMatrix(ts_example_1[[i]], ts_example_2[[i]]),
                                        RDistMatrix(ts_example_1[[i]], ts_example_2[[i]]))
  
  median_runs <- aggregate(mcb$time ~ mcb$expr, FUN = median)
  
  time_Rcpp[i] <- median_runs$`mcb$time`[1]
  time_R[i] <- median_runs$`mcb$time`[2]
}



df_to_plot <- data.frame(
  index = 2:100,
  time_rcpp = time_Rcpp / 100000,
  time_r = time_R / 100000
)

ggplot(df_to_plot, aes(index)) + 
  geom_line(aes(y = time_r, colour = "Standard R implementation")) + 
  geom_line(aes(y = time_rcpp, colour = "Rcpp implementation")) +
  ggplot2::ylab("Time in microseconds") +
  ggplot2::xlab("Time series length") +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Distance matrix calculation time")

# Plot 2 comparison of different methods time calculation

SDP_traditional <- new("ShapeDescriptorParams")
SDP_compound <- new("ShapeDescriptorParams",
                    Type = "compound",
                    Descriptors = c("PAADescriptor",
                                    "slopeDescriptor"),
                    Additional_params = list(
                      Weights = c(1, 10),
                      PAAWindow = 3L,
                      slopeWindow = 3L
                    ))

euclidDistance <- function(x, y){
  sqrt(sum((x-y)^2))
}

corMeasure <- function(x, y){
  cor(x, y)
}

simpleDTWMeasure <- function(x, y){
  dtw_shape <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = x, 
                                            testSeries = y, 
                                            forecastHorizon = 0, 
                                            subsequenceWidth = 0, 
                                            subsequenceBreaks = 1, 
                                            shapeDescriptorParams = SDP_traditional, 
                                            normalizationType = "Unitarization", 
                                            distanceType = "Dependent")
  dtw_shape
}

shapeDTWMeasure <- function(x, y){
  dtw_shape <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = x, 
                                            testSeries = y, 
                                            forecastHorizon = 0, 
                                            subsequenceWidth = 4, 
                                            subsequenceBreaks = 1, 
                                            shapeDescriptorParams = SDP_compound, 
                                            normalizationType = "Unitarization", 
                                            distanceType = "Dependent")
  dtw_shape
}

shapeDTWmultidim <- function(x, y){
  dtw_shape <- RcppShapeDTW::kNNShapeDTWCpp(referenceSeries = x, 
                                            testSeries = y, 
                                            forecastHorizon = 0, 
                                            subsequenceWidth = 4, 
                                            subsequenceBreaks = 1, 
                                            shapeDescriptorParams = SDP_compound, 
                                            normalizationType = "Unitarization", 
                                            distanceType = "Dependent")
  dtw_shape
}

time_series_test_1 <- purrr::map(10:100, function(x){
  m <- matrix(rnorm(2*x), ncol = 2)
  m[,1] <- Unitarization(m[,1])
  m[,2] <- Unitarization(m[,2])
  m
})

time_series_test_2 <- purrr::map(10:100, function(x){
  m <- matrix(rnorm(2*x), ncol = 2)
  m[,1] <- Unitarization(m[,1])
  m[,2] <- Unitarization(m[,2])
  m
})

time_series_for_DTW_1 <- purrr::map(time_series_test_1,
                                    function(x){
                                      t(t(x[,1]))
                                    })

time_series_for_DTW_2 <- purrr::map(time_series_test_2,
                                    function(x){
                                      t(t(x[,1]))
                                    })


euclid_time <- numeric(length(time_series_test_1))
cor_time <- numeric(length(time_series_test_1))
simpleDTW_time <- numeric(length(time_series_test_1))
shapeDTW_time <- numeric(length(time_series_test_1))
MTDshapeDTW_time <- numeric(length(time_series_test_1))

for(i in 1:length(time_series_test_1)){
  cat("Proceeding ts number ", i, "\n")
  
  mcb <- microbenchmark::microbenchmark(euclidDistance(time_series_for_DTW_1[[i]], time_series_for_DTW_2[[i]]),
                                        corMeasure(time_series_for_DTW_1[[i]], time_series_for_DTW_2[[i]]),
                                        simpleDTWMeasure(time_series_for_DTW_1[[i]], time_series_for_DTW_2[[i]]),
                                        shapeDTWMeasure(time_series_for_DTW_1[[i]], time_series_for_DTW_2[[i]]),
                                        shapeDTWmultidim(time_series_test_1[[i]], time_series_test_2[[i]]))
  
  median_runs <- aggregate(mcb$time ~ mcb$expr, FUN = median)
  
  time_Rcpp[i] <- median_runs$`mcb$time`[1]
  time_R[i] <- median_runs$`mcb$time`[2]
  
  euclid_time[i] <- median_runs$`mcb$time`[1]
  cor_time[i] <- median_runs$`mcb$time`[2]
  simpleDTW_time[i] <- median_runs$`mcb$time`[3]
  shapeDTW_time[i] <- median_runs$`mcb$time`[4]
  MTDshapeDTW_time[i] <- median_runs$`mcb$time`[5]
}

df_to_plot <- data.frame(
  index = 10:100,
  euclid_time = euclid_time / 10000,
  cor_time = cor_time / 10000,
  simpleDTW_time = simpleDTW_time / 10000,
  shapeDTW_time = shapeDTW_time / 10000,
  MTDshapeDTW_time = MTDshapeDTW_time /10000
)

ggplot(df_to_plot, aes(index)) + 
  geom_line(aes(y = euclid_time, colour = "Euclidean distance")) + 
  geom_line(aes(y = cor_time, colour = "Correlation distance")) +
  geom_line(aes(y = simpleDTW_time, colour = "Standard DTW distance")) +
  geom_line(aes(y = shapeDTW_time, colour = "shapeDTW distance")) +
  geom_line(aes(y = MTDshapeDTW_time, colour = "Multidimensional shapeDTW distance")) +
  ggplot2::ylab("Time in microseconds") +
  ggplot2::xlab("Time series length") +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Distance measures - calculation time")


# Plot 3 descriptors examples
require(dplyr)
require(ggplot2)

MBANK_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/MBANK.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

mbank_close_prices <- MBANK_data$X.CLOSE.
mbank_subsequence <- mbank_close_prices[6000:6100]

mbank_subsequence_matrix <- RcppShapeDTW::RcppsubsequencesMatrix(
  mbank_subsequence, subsequenceWidth = 5
)

SDP_slope <- new("ShapeDescriptorParams",
                 Type = "simple",
                 Descriptors = c("slopeDescriptor"),
                 Additional_params = list(
                   slopeWindow = 3L
                 ))
SDP_PAA <- new("ShapeDescriptorParams",
               Type = "simple",
               Descriptors = c("PAADescriptor"),
               Additional_params = list(
                 PAAWindow = 3L
               ))
SDP_der <- new("ShapeDescriptorParams",
               Type = "simple",
               Descriptors = c("derivativeDescriptor"))

slopeDescriptorsMatrix <- RcppShapeDTW::RcppasShapeDescriptor(
  mbank_subsequence_matrix,
  SDP_slope
)

PAADescriptorsMatrix <- RcppShapeDTW::RcppasShapeDescriptor(
  mbank_subsequence_matrix,
  SDP_PAA
)

derivativeDescriptorsMatrix <- RcppShapeDTW::RcppasShapeDescriptor(
  mbank_subsequence_matrix,
  SDP_der
)

par(mfrow = c(1, 4))
plot(mbank_subsequence_matrix[50,], type = "l")
plot(slopeDescriptorsMatrix[50,], type = "l")
plot(PAADescriptorsMatrix[50,], type = "l")
plot(derivativeDescriptorsMatrix[50,], type = "l")

df_ind_11 <- data.frame(index = 1:11)
df_ind_9 <- data.frame(index = 1:9)

real_series_plot <- ggplot(df_ind_11, aes(index)) + 
  geom_line(aes(y = mbank_subsequence_matrix[50,])) +
  ggplot2::theme_minimal() + 
  ggplot2::ylab("Price") +
  ggplot2::ggtitle("Close prices")

slope_desc_plot <- ggplot(df_ind_9, aes(index)) + 
  geom_line(aes(y = slopeDescriptorsMatrix[50,])) +
  ggplot2::theme_minimal() + 
  ggplot2::ylab("value") +
  ggplot2::ggtitle("Slope descriptor")

paa_desc_plot <- ggplot(df_ind_9, aes(index)) + 
  geom_line(aes(y = PAADescriptorsMatrix[50,])) +
  ggplot2::theme_minimal() + 
  ggplot2::ylab("value") +
  ggplot2::ggtitle("PAA descriptor")

der_desc_plot <- ggplot(df_ind_11, aes(index)) + 
  geom_line(aes(y = derivativeDescriptorsMatrix[50,])) +
  ggplot2::theme_minimal() + 
  ggplot2::ylab("value") +
  ggplot2::ggtitle("Derivative descriptor")

gridExtra::grid.arrange(grobs = list(real_series_plot, 
                                     slope_desc_plot,
                                     paa_desc_plot,
                                     der_desc_plot), nrow = 2)

# Plot 4 time series and trigonometric transform

JSW_data <- read.table(
  file = "../Magisterka tekst/Ilustracje/Dane/JSW.mst", 
  header = T,
  sep = ",",
  stringsAsFactors = F
)

JSW_d <- JSW_data %>%
  mutate(date = as.POSIXct(strptime(X.DTYYYYMMDD., format = "%Y%m%d")),
         closePrice = X.CLOSE., volume = X.VOL.) %>%
  filter(date > as.Date("2018-04-25"), date < as.Date("2018-12-30"))%>%
  dplyr::select(date, closePrice, volume) %>%
  mutate(TTR_cos_volume = RcppShapeDTW::RcpptrigonometicTransform(volume, "cosinus"))

real_series_plot <- ggplot2::ggplot(JSW_d, aes(date)) + 
  geom_line(aes(y = closePrice)) +
    ggplot2::theme_light() + 
  ggplot2::ylab("Price") +
  ggplot2::ggtitle("Close prices")

volume_plot <- ggplot2::ggplot(JSW_d, aes(date)) + 
  geom_line(aes(y = volume)) +
  ggplot2::theme_light() + 
  ggplot2::ylab("Volume") +
  ggplot2::ggtitle("Volume")

TTR_volume_plot <- ggplot2::ggplot(JSW_d, aes(1:nrow(JSW_d))) + 
  geom_line(aes(y = TTR_cos_volume)) +
  ggplot2::theme_light() + 
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Volume's cosine transform") +
  ggplot2::xlab("")

gridExtra::grid.arrange(grobs = list(real_series_plot, 
                                     volume_plot,
                                     TTR_volume_plot), nrow = 3)

# Plot 5 time series, nearest neighbour and forecast
GPW_tick_d30min <- readRDS("../Magisterka tekst/SeriesProcessed/GPW_tick_d30min.rds")
GPW_30_min_results <- readRDS("../Magisterka tekst/Wyniki/DaneRDS/ResultsEuclidSingleStockTimeFixed/GPW_tick_d30min_results_ref_100.rds")
GPW_30_min_results$PGNIG$dtw_type_Dependent.shape_desc_type_compound.dims1



DTW_results_to_plot <- RknnShapeDTWParallel(refSeries = GPW_tick_d30min_filtered$KGHM,
                                            learnSeries = GPW_tick_d30min_filtered[4], 
                                            refSeriesStart = 9062, 
                                            shapeDTWParams = SDP_compound, 
                                            targetDistance = "r", 
                                            distanceType = "D", 
                                            normalizationType = "Z", 
                                            refSeriesLength = 100, 
                                            forecastHorizon = 100, 
                                            subsequenceWidth = 4, 
                                            subsequenceBreaks = 1)

plot(DTW_results_to_plot, DTW_results_to_plot, lift = 0, includeFrcstPart = T, add_wp = F)

sign(GPW_30_min_results$KGHM$dtw_type_Dependent.shape_desc_type_compound.dims1_2$target_series_100_returns)==
sign(GPW_30_min_results$KGHM$dtw_type_Dependent.shape_desc_type_compound.dims1_2$learn_series_100_returns)
GPW_30_min_results$KGHM$dtw_type_Dependent.shape_desc_type_compound.dims1_2$best_series_ind.Idx

str(DTW_results_to_plot)
seq(from = 9062, by = 10, length.out = 100)[97]

DTW_results_to_plot_missed <- RknnShapeDTWParallel(refSeries = GPW_tick_d30min_filtered$KGHM,
                                            learnSeries = GPW_tick_d30min_filtered[4], 
                                            refSeriesStart = 10022, 
                                            shapeDTWParams = SDP_compound, 
                                            targetDistance = "r", 
                                            distanceType = "D", 
                                            normalizationType = "Z", 
                                            refSeriesLength = 100, 
                                            forecastHorizon = 100, 
                                            subsequenceWidth = 4, 
                                            subsequenceBreaks = 1)

plot(DTW_results_to_plot_missed, DTW_results_to_plot, lift = 0, includeFrcstPart = T, add_wp = F)


DTW_results_to_plot_showing_matching <- RknnShapeDTWParallel(refSeries = GPW_tick_d30min_filtered$KGHM,
                                                   learnSeries = GPW_tick_d30min_filtered[4], 
                                                   refSeriesStart = 9500, 
                                                   shapeDTWParams = SDP_compound, 
                                                   targetDistance = "r", 
                                                   distanceType = "D", 
                                                   normalizationType = "Z", 
                                                   refSeriesLength = 100, 
                                                   forecastHorizon = 100, 
                                                   subsequenceWidth = 4, 
                                                   subsequenceBreaks = 1)

plot(DTW_results_to_plot_showing_matching, lift = 3, includeFrcstPart = F, add_wp = T)

DTW_results_to_plot_showing_I_matching <- RknnShapeDTWParallel(refSeries = GPW_tick_d30min_filtered$KGHM,
                                                             learnSeries = GPW_tick_d30min_filtered[4], 
                                                             refSeriesStart = 9500, 
                                                             shapeDTWParams = SDP_compound, 
                                                             targetDistance = "r", 
                                                             distanceType = "I", 
                                                             normalizationType = "Z", 
                                                             refSeriesLength = 100, 
                                                             forecastHorizon = 100, 
                                                             subsequenceWidth = 4, 
                                                             subsequenceBreaks = 1)

plot(DTW_results_to_plot_showing_I_matching, lift = 0, includeFrcstPart = T, add_wp = F)

# Plot 6 learning and testing part division

GPW_tick_d30min_filtered$PKOBP

PKO_data_to_plot <- as.data.frame(GPW_tick_d30min_filtered$PKOBP) %>%
  mutate(date = as.Date(time(GPW_tick_d30min_filtered$PKOBP)))

ggplot2::ggplot(PKO_data_to_plot, aes(date)) +
  ggplot2::geom_line(aes(y = ClosePrice)) +
  labs(title = "PKO", 
       subtitle  = paste(
         strftime(min(PKO_data_to_plot$date), "%d.%m.%Y"), " - ", 
         strftime(max(PKO_data_to_plot$date), "%d.%m.%Y"))) +
  geom_vline(xintercept = as.Date("2019-06-03"), size = 1, col = "red")


