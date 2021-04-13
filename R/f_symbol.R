
#' This function update the lists of symbols available from a given exchange/in an index.
#' @param str_exchange_index A string specifying which exhange/index to look at. Possible values: NASDAQ, GDAXIOMX, OMX, OMXC25 and OMXCXC20GI.
#' @importFrom utils data
#' @importFrom utils write.table
#' @importFrom magrittr "%>%"
#' @return Nothing is returned. The function saves the symbols as .txt in ./data/symbols folder

f_update_symbol <- function(str_exchange_index = "NASDAQ") {

  Change <- Open <- symbols <- NULL

  if (str_exchange_index == "NASDAQ") {

    df_symbol <- TTR::stockSymbols(quiet = TRUE)
    df_symbol$Volume <- NA
    df_symbol$Exchange_index <- df_symbol$Exchange
    df_symbol$Company_Name <- df_symbol$Name

  } else {

    url <- paste0("https://finance.yahoo.com/quote/%5E", str_exchange_index, "/components?p=%5E", str_exchange_index)

    webpage <- xml2::read_html(url)

    tabs <- webpage %>%
      rvest::html_table()

    df_symbol <- tabs[[1]]
    df_symbol$Exchange_index <- str_exchange_index
    names(df_symbol) <- gsub(" ", "_", names(df_symbol))

  }

  df_symbol$Company_Name <- gsub(";|%|'", "", df_symbol$Company_Name)
  df_symbol$Company_Symbol <- paste0(df_symbol$Company_Name, " (", df_symbol$Symbol, ")")
  df_symbol <- df_symbol[, c("Exchange_index", "Company_Name", "Symbol", "Company_Symbol", "Volume")]
  write.table(x = df_symbol, file = paste0("data/symbols/", str_exchange_index, ".txt"),
              quote = FALSE, row.names = FALSE, sep = ";")

  return("Symbols have been saved to ./data/symbols folder")

}

#' This function returns ticker code for a stock
#' @param stock Stock name. Full stock name not needed as ticker codes for stock names like parameter input is returned.
#' @param index Index name. Supply either stock name or index, not both.
#' @param output string indicating if output should be a vector or data.frame. Default is "vector".
#'
#' @return A vector with ticker codes matching the input name(stock or index). If the output parameter is not "vector", then a data.frame with ticker codes and stock names matching input stock name is returned.
#' @export

f_get_symbol <- function(stock = "NOVO", index = NULL, output = "vector") {

  df_symbols <- NULL

  if (!is.null(index)) {

    if (!any(c("GDAXI", "OMX", "OMXC25", "OMXCXC20GI") %in% index)) {
      stop("index should be either GDAXI, OMX, OMXC25, OMXCXC20GI or a vector combination of these")
    }

  }

  # change symbols to df_symbols
  data("df_symbols", package = "kb.modelling", envir = environment())
  stock <- tolower(stock)

  if (!is.null(index)) {
    df_out <- df_symbols[df_symbols$Exchange_index %in% index, c("Company_Name", "Symbol")]
  } else {
    df_out <- df_symbols[grep(stock, df_symbols$Company_Symbol_lower), c("Company_Name", "Symbol")]
  }

  if (nrow(df_out) == 0) {
    return(paste0("Ticker code not found for ", stock))
  }

  rownames(df_out) <- 1:nrow(df_out)
  if (output == "vector") {
    return(df_out$Symbol)
  } else {
    return(df_out)
  }
}

#' This function returns company name code for a given ticker code
#' @param ticker ticker code
#' @param index Index name. Supply either ticker code or index, not both. Index can be a vector.
#'
#' @return A vector with company names matching the input name(ticker or index).
#' @export

f_get_stock <- function(ticker, index = NULL) {

  df_symbols <- NULL

  # change symbols to df_symbols
  data("df_symbols", package = "kb.modelling", envir = environment())

  if (!is.null(index)) {
    if (!any(c("GDAXI", "OMX", "OMXC25", "OMXCXC20GI") %in% index)) {
      stop("index should be either GDAXI, OMX, OMXC25, OMXCXC20GI or a vector combination of these")
    }
    company <- df_symbols$Company_Name[df_symbols$Exchange_index %in% index]
    str_ticker <- index
  } else {
    company <- df_symbols$Company_Name[df_symbols$Symbol == ticker]
    str_ticker <- ticker
  }

  if (length(company) == 0) {
    return(paste0("No company found for ticker code/index ", str_ticker))
  }

  return(company)

}
