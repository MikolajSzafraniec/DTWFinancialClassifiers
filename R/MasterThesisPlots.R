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
  dplyr::filter(date < as.Date("2002-01-01"))

