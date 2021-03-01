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
#'
#' @return A vector with prince changes in %
#' @export

f_change <- function(x, digits = 5) {
  x_lag <- c(NA, x)
  x <- c(x, NA)
  y <- round((x - x_lag) / x, digits)
  y <- 100 * y[-length(y)]
  return(y)
}

#' This function loads one stock from Yahoo.
#' @param ticker ticker code from Yahoo. Use f_load to multiple stocks. 
#' @param from_date loads data from the date untill today. Date format is yyyy-mm-dd. 
#'
#' @return A data.table
#' @export
#' @import data.table 
#' @import quantmod

f_load_one <- function(ticker, from_date = "2014-01-01") {
  
  data <- f_try_catch(quantmod::getSymbols(ticker, auto.assign = FALSE, 
                                           from = from_date, src = 'yahoo'))
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
  
  return(data)
  
}

#' This function loads data from Yahoo.
#' @param ticker ticker code from Yahoo. Provide a vector of ticker codes to get multiple stocks. 
#' @param from_date loads data from the date untill today. Date format is yyyy-mm-dd. 
#'
#' @return A data.table
#' @export

f_load <- function(ticker, from_date = "2014-01-01") {
  
  df_list <- lapply(X = ticker, FUN = f_load_one, from_date = from_date)
  
  df_out <- do.call("rbind", df_list)
  
  return(df_out)
  
}
