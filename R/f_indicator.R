#' Lag function
#' @param x A vector to perform lag on
#' @param n number of elements to shift. Default is 1.
#' @export
#' @examples
#' f_lag(x = 1:10, n = 1)
#'

f_lag <- function(x, n = 1) {

  if (n == 0) {
    return(x)
  }

  id <- 1:n
  x <- c(x[-id], rep(NA, n))
  return(x)
}

#' This function calculates the slope of a moving average
#' @param data data.table. data should be a obj an outcome from f_indicators to ensure MA is present in data.
#' @return The slope of the moving average
#' @import data.table
#' @importFrom stats coef lm na.omit

f_slope <- function(data) {

  id <- NULL

  data[, id := 1:.N]
  subdata <- data[, c("MA", "id")]
  subdata <- na.omit(subdata)
  if (nrow(subdata) < 5) {
    return(NA)
  }
  m <- lm(MA ~ id, subdata)
  as.numeric(coef(m)[2])
}

#' This function calculates the slope of a moving average based on n data points
#' @param data data.table. data should be a obj an outcome from f_indicators to ensure MA is present in data.
#' @param n number of days used to calculate the slope
#' @return data.table object with moving average slope for each day
#' @import data.table

f_ma_slope <- function(data, n = 10) {

  m <- nrow(data)

  start_day <- seq(from = 1, to = m, by = n)
  end_day <- seq(from = n, to = m, by = n)

  k <- min(length(start_day), length(end_day))

  start_day <- start_day[1:k]
  end_day <- end_day[1:k]
  h <- floor(n / 2)
  id <- which(end_day - start_day > h)
  start_day <- start_day[id]
  end_day <- end_day[id]
  l <- length(id)

  data$MA_slope <- rep(NA, nrow(data))

  for (i in i:l) {
    time_period <- start_day[i]:end_day[i]
    loopdata <- data[time_period, ]

    data$MA_slope[time_period] <- f_slope(data = loopdata)

  }

  return(data$MA_slope)

}

#' This function calculates common indicators such as moving averages and RSI
#' @param data a data.table obtained from f_load
#' @param n number of days used to calculate the slope of moving average. Default is 10
#' @param m number of days used to calculate moving average. Default is 5
#' @return A data.table
#' @export
#' @import data.table

f_indicator <- function(data, n = 10, m = 5) {

  Change <- MACD <- NULL

  data <- data[!is.na(Change), ]

  ## OSCILLATORS
  ## Relative Strength Index
  data$RSI <- TTR::RSI(data$Close)

  ## Commodity Channel Index (20)
  data$CCI <- TTR::CCI(HLC = data[, c("High", "Low", "Close")], n = 20)

  ## Average Directional Index (14)
  df_adi <- TTR::ADX(HLC = data[, c("High", "Low", "Close")], n = 14)
  df_adi <- data.frame(df_adi)
  data$ADI <- df_adi$ADX

  ## Awesome Oscillator

  ## Momentum (10)
  data$momentum <- TTR::momentum(x = data$Close, n = 10)

  ## MACD Level (12, 26)
  MACD_res <- data.frame(TTR::MACD(data$Close,Fast = 12, nSlow = 26))
  data$MACD <- MACD_res$macd

  ## Stochastic RSI Fast (3, 3, 14, 14)

  ## Williams Percent Range (14)
  data$Williams_pct <- TTR::WPR(HLC = data[, c("High", "Low", "Close")], n = 14)

  ## Bull Bear Power

  ## Ultimate Oscillator (7, 14, 28)
  data$ultimateOscillator <- TTR::ultimateOscillator(HLC = data[, c("High", "Low", "Close")], n = c(7, 14, 28))

  ## MOVING AVERAGES
  ## Exponential Moving Average (5)
  data$EMA5 <- TTR::EMA(x = data$Close, n = 5)

  ## Simple Moving Average (5)
  data$MA5 <- TTR::SMA(x = data$Close, n = 5)

  ## Exponential Moving Average (10)
  data$EMA10 <- TTR::EMA(x = data$Close, n = 10)

  ## Simple Moving Average (10)
  data$MA10 <- TTR::SMA(x = data$Close, n = 10)

  ## Exponential Moving Average (20)
  data$EMA20 <- TTR::EMA(x = data$Close, n = 20)

  ## Simple Moving Average (20)
  data$MA20 <- TTR::SMA(x = data$Close, n = 20)

  ## Exponential Moving Average (30)
  data$EMA30 <- TTR::EMA(x = data$Close, n = 30)

  ## Simple Moving Average (30)
  data$MA30 <- TTR::SMA(x = data$Close, n = 30)

  ## Exponential Moving Average (50)
  data$EMA50 <- TTR::EMA(x = data$Close, n = 50)

  ## Simple Moving Average (50)
  data$MA50 <- TTR::SMA(x = data$Close, n = 50)

  ## Exponential Moving Average (100)
  data$EMA100 <- TTR::EMA(x = data$Close, n = 100)

  ## Simple Moving Average (100)
  data$MA100 <- TTR::SMA(x = data$Close, n = 100)

  ## Exponential Moving Average (200)
  data$EMA200 <- TTR::EMA(x = data$Close, n = 200)

  ## Simple Moving Average (200)
  data$MA200 <- TTR::SMA(x = data$Close, n = 200)

  ## Ichimoku Cloud Base Line (9, 26, 52, 26)

  ## Volume Weighted Moving Average (20)
  data$WMA <- TTR::WMA(x = data$Close, wts = data$Volume, n = 20)

  ## Hull Moving Average (9)
  data$HMA <- TTR::HMA(x = data$Close, n = 9)

  ## Signal
  data$Signal <- MACD_res$signal

  ## MACD Signal difference
  data$MACD_Signal_Diff <- data$MACD - data$Signal

  ## MA
  data$MA <- TTR::SMA(data$Close, n = m)

  ## MA slopes
  data$MA5_slope <- f_ma_slope(data, n = 5)
  data$MA10_slope <- f_ma_slope(data, n = 10)
  data$MA20_slope <- f_ma_slope(data, n = 20)
  data$MA50_slope <- f_ma_slope(data, n = 50)
  data$MA100_slope <- f_ma_slope(data, n = 100)
  data$MA200_slope <- f_ma_slope(data, n = 200)

  ## Bollinger bands:

  bb <- TTR::BBands(HLC = data[, c("High", "Low", "Close")])

  data$lower_bb <- bb[, "dn"]
  data$upper_bb <- bb[, "up"]
  data$ma_bb <- bb[, "mavg"]
  data$pct_bb <- bb[, "pctB"]

  data[, pct_close_lower_bb := (Close - lower_bb) / Close]
  data[, pct_upper_bb_close := (upper_bb - Close) / Close]
  data[, bb_dif := upper_bb - lower_bb]

  return(data)

}
