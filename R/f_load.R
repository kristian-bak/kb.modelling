#' This function is an extension of tryCatch
#' @param expr expression to evalute
#'
#' @return A list with value, warning and error message
#' @export

f_try_catch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value = value, warning = warn, error = err)
}

#' This function is used to calculate price change in %
#' @param x stock price
#' @param digits number of digits used when rounding the price change
#' @param from_start logical. If TRUE, change will be calculated since day 1
#' @return A vector with prince changes in %
#' @export

f_change <- function(x, from_start = FALSE, digits = 5) {
  x_lag <- c(NA, x)
  x <- c(x, NA)
  if (from_start) {
    y <- round((x - x[1]) / x[1], digits)
  } else {
    y <- round((x - x_lag) / x, digits)
  }
  y <- 100 * y[-length(y)]
  return(y)
}

#' This function loads one stock from Yahoo.
#' @description Get stock data from Yahoo by inputting ticker name.
#' @param ticker ticker code from Yahoo. Use f_load to multiple stocks.
#' @param from_date loads data from the date untill today. Date format is yyyy-mm-dd.
#' @param offline logical. If TRUE, data will be loaded from data folder
#' @param indicators logical. If TRUE, technical indicators will be calculated as well. Default is TRUE
#' @return A data.table
#' @export
#' @import data.table
#' @import quantmod

f_load_one <- function(ticker, from_date = "2014-01-01",
                       offline = FALSE, indicators = TRUE) {

  Change <- Close <- Ticker <- NULL

  if (offline) {
    str_ticker <- gsub("\\.", "_", ticker)
    file_name <- paste0(str_ticker, ".RDS")
    full_file_name <- system.file("data/stocks", file_name, package = "kb.modelling")
    data <- f_try_catch(readRDS(full_file_name))
    if (!is.null(data$value)) {
      return(data$value)
    }
  } else {
    data <- f_try_catch(quantmod::getSymbols(ticker, auto.assign = FALSE,
                                             from = from_date, src = 'yahoo'))
  }

  str_stock <- f_get_stock(ticker = ticker)

  if (is.null(data$value)) {
    dt <- data.table::data.table("Company" = str_stock,
                                 "Ticker" = ticker,
                                 "Date" = NA, "Open" = NA,
                                 "Low" = NA, "High" = NA,
                                 "Close" = NA, "Adjusted" = NA,
                                 "Change" = NA, "Volume" = NA)
    return(dt)
  }

  data <- data$value
  data <- data.frame(data)
  data$Date <- rownames(data)
  data <- data.table::data.table(data)
  var_rename <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date")

  data.table::setnames(data, old = names(data), new = var_rename)

  str_company <- f_get_stock(ticker = ticker)

  if (length(str_company) > 1) {
    warning("Multiple companies matched the ticker code. The first name was selected")
  }

  data$Company <- str_company[1]
  data[, Change := f_change(Close)][]
  data[, Ticker := ticker]

  data.table::setcolorder(data, neworder = c("Company", "Ticker", "Date", "Open", "Low", "High",
                                             "Close", "Adjusted", "Change", "Volume"))

  flag_change_tomorrow <- NULL

  if (indicators) {
    data <- f_data_prep(data)
  }

  return(data)

}

#' This function loads data from Yahoo.
#' @param ticker ticker code from Yahoo. Provide a vector of ticker codes to get multiple stocks.
#' @param from_date loads data from the date untill today. Date format is yyyy-mm-dd.
#' @param indicators logical. If TRUE, technical indicators will be calculated.
#' @param offline logical. If TRUE data will be loaded from data folder
#' @return A data.table
#' @export

f_load <- function(ticker, from_date = "2014-01-01", indicators = TRUE, offline = FALSE) {

  df_list <- lapply(X = ticker, FUN = f_load_one,
                    from_date = from_date,
                    indicators = indicators,
                    offline = offline)

  df_out <- do.call("rbind", df_list)

  return(df_out)

}

#' Loading data from Federal Reserve bank (FRED)
#' @description f_load_fred takes character value and loads data from Federal Reserve bank (FRED)
#' @param symbol symbol to load
#' @importFrom quantmod getSymbols
#'
#' @return A data.table
#' @export

f_load_fred <- function(symbol) {
  df <- quantmod::getSymbols(Symbols = symbol, src = "FRED")
  df <- data.frame(mget(df))
  df$Date <- rownames(df)
  dt <- data.table::data.table(df)
  dt$tmp <- f_change(dt[[symbol]])
  dt[, (symbol) := NULL]
  setnames(dt, old = "tmp", new = symbol)
  return(dt)
}
