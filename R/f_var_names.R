#' Variable names
#' @param data data input is used to only return variables present in data
#' @param var_names variable names. Default is NULL, which means all predictor names from f_indicators that are present in data are returned. If a vector of variable names are supplied, f_var_names will return the vector
#' @export
#'
f_var_names <- function(data, var_names = NULL) {
  if (is.null(var_names)) {
    var_names <- c("Volume", "RSI", "CCI", "ADI", "momentum", "MACD", "Williams_pct",
      "ultimateOscillator", "EMA5", "MA5", "EMA10", "MA10", "EMA20", "MA20",
      "EMA30", "MA30", "EMA50", "MA50", "EMA100", "MA100", "EMA200", "MA200",
      "WMA", "Diff", "MA5_slope", "MA10_slope", "MA20_slope", "MA50_slope",
      "MA100_slope", "MA200_slope",
      "lower_bb", "ma_bb", "Close", "upper_bb", "pct_bb",
      "pct_close_lower_bb", "pct_upper_bb_close", "bb_dif",
      "treasury_10y", "unemployment_claims", "inflation_10y",
      "mortgage_rate_30y_us", "brent_oil", "gold", "bitcoin",
      "NOVO", "Pandora", "aluminum")
    var_names <- var_names[var_names %in% names(data)]
  }
  return(var_names)
}
