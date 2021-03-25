#' Update data
#' @param ticker ticker name
#' @export

f_update_data <- function(ticker = "all") {

  if (ticker == "all") {
    ticker <- c("DGS10", "ICSA", "T10YIE", "MORTGAGE30US", "DCOILBRENTEU",
                 "GOLDAMGBD228NLBM", "CBBTCUSD", "PALUMUSDM", "USRECDM")
  }

  n <- length(ticker)
  dt_all <- f_load_fred(symbol = ticker[1])
  flush.console()
  cat("\r", 1, "of", n)

  for (i in 2:n) {
    dt <- f_load_fred(symbol = ticker[i])
    if (ticker[i] == "ICSA") {
      dt[, Date := as.character(as.Date(Date) - 1)]
    }
    dt_all <- dt_all %>%
      dplyr::full_join(dt, by = "Date")

    flush.console()
    cat("\r", i, "of", n)

  }

  dt_all <- data.table::data.table(dt_all)

  setnames(dt_all,
           old = c("DGS10", "ICSA", "T10YIE", "MORTGAGE30US", "DCOILBRENTEU",
                   "GOLDAMGBD228NLBM", "CBBTCUSD", "PALUMUSDM", "USRECDM"),
           new = c("treasury_10y", "unemployment_claims", "inflation_10y",
                   "mortgage_rate_30y_us", "brent_oil", "gold", "bitcoin", "aluminum",
                   "flag_us_recession"))

  return(dt_all)

}
