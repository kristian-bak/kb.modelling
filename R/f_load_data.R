#' Load data
#' @param ticker ticker name
#' @param from_date load data since from date
#' @param offline logical. If TRUE, data will be loaded from data folder.
#' @export
f_load_data <- function(ticker, from_date = "2016-01-01", offline = FALSE) {

  str_ticker <- gsub("\\.", "_", ticker)
  file_name <- list.files("data/tickers", pattern = str_ticker, full.names = TRUE)

  if (length(file_name) > 1) {
    stop("Ticker matches multiple RDS files")
  } else if (length(file_name) == 1) {
    file_info <- file.info(file_name)
    last_run <- substring(file_info$mtime, 1, 10)
  } else {
    last_run <- "1900-01-01"
  }

  if (last_run == Sys.Date()) {
    data <- readRDS(file = file_name)
    return(data)
  }

  data <- f_load(ticker = ticker, from_date = from_date, offline = offline)

  data[, close_day1 := f_lag(Close, n = 1)]
  data[, close_day5 := f_lag(Close, n = 5)]
  data[, close_day10 := f_lag(Close, n = 10)]
  data[, close_day20 := f_lag(Close, n = 20)]
  data[, close_day50 := f_lag(Close, n = 50)]

  data[, target_day1 := 1 * (close_day1 >= Close)]
  data[, target_day5 := 1 * (close_day5 >= Close)]
  data[, target_day10 := 1 * (close_day10 >= Close)]
  data[, target_day20 := 1 * (close_day20 >= Close)]
  data[, target_day50 := 1 * (close_day50 >= Close)]

  data[, close_day1 := NULL]
  data[, close_day5 := NULL]
  data[, close_day10 := NULL]
  data[, close_day20 := NULL]
  data[, close_day50 := NULL]

  data[, open_tomorrow := f_lag(Open, n = 1)]
  data <- f_indicator(data = data)

  if (offline) {
    dt <- readRDS(system.file("data", "fred.RDS", package = "kb.modelling"))

    data <- data %>%
      dplyr::left_join(dt, by = "Date")

  } else {
    dt <- f_load_fred(symbol = "DGS10")
    dt <- dt[Date >= from_date, ]

    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "ICSA")
    dt[, Date := as.character(as.Date(Date) - 1)]
    dt <- dt[Date >= from_date, ]

    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "T10YIE")
    dt <- dt[Date >= from_date, ]

    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "MORTGAGE30US")
    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "DCOILBRENTEU")
    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "GOLDAMGBD228NLBM")
    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "CBBTCUSD")
    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load_fred(symbol = "PALUMUSDM")
    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    ## Stocks

    dt <- f_load(ticker = "NOVO-B.CO", from_date = from_date)
    dt <- dt[, c("Date", "Change")]
    setnames(dt, old = "Change", new = "NOVO")

    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    dt <- f_load(ticker = "PNDORA.CO", from_date = from_date)
    dt <- dt[, c("Date", "Change")]
    setnames(dt, old = "Change", new = "Pandora")

    data <- data %>%
      dplyr::left_join(dt, by = "Date")

    setnames(data,
             old = c("DGS10", "ICSA", "T10YIE", "MORTGAGE30US", "DCOILBRENTEU",
                     "GOLDAMGBD228NLBM", "CBBTCUSD", "PALUMUSDM"),
             new = c("treasury_10y", "unemployment_claims", "inflation_10y",
                     "mortgage_rate_30y_us", "brent_oil", "gold", "bitcoin", "aluminum"))

    saveRDS(object = data, file = paste0("./data/tickers/", str_ticker, ".RDS"))

  }

  return(data)

}
