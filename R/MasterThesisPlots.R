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

