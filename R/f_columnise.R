#' Columnise data
#' @param data data.table from any of the f_load functions
#' @export
f_columnise <- function(data) {

  f_ticker_name <- function(x) {
    x <- gsub("\\.|-", "_", x)
    gsub("\\^", "", x)
  }

  str_tickers <- data$Ticker %>% unique()
  n_tickers <- length(str_tickers)

  if (n_tickers < 2) {
    stop("Number of tickers should be at least 2")
  }

  df_out <- data[Ticker == str_tickers[1], c("Date", "Close")]
  setnames(df_out, old = "Close", new = f_ticker_name(str_tickers[1]))

  for (i in 2:n_tickers) {

    df_tmp <- data[Ticker == str_tickers[i], c("Date", "Close")]
    setnames(df_tmp, old = "Close", new = f_ticker_name(str_tickers[i]))
    df_out <- df_out %>%
      dplyr::inner_join(df_tmp, by = "Date")

  }

  return(df_out)

}
