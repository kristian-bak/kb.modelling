#' Stock trading based on xgboost predictions
#' @param test_data data. Should be a data.table object with predictions from f_predict
#' @param buy_cut cut off determining when to buy. Should be a number between 0 and 1
#' @param sell_cut cut off determining when to sell. Should be a number between 0 and 1
#' @importFrom data.table data.table
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @export
#' @return list with trades made and earnings

f_trade <- function(test_data, buy_cut = 0.6, sell_cut = 0.4) {

  # Solution to no visible binding for global variable
  Date <- open_tomorrow <- Close <- action <- close_today <- earning <- yield <- NULL

  n <- nrow(test_data)
  status <- rep("out", n)
  p <- test_data[, p]
  buy <- ifelse(p >= buy_cut, 1, 0)
  sell <- ifelse(p < sell_cut, 1, 0)
  hold <- ifelse(p <= buy_cut & p >= sell_cut, 1, 0)
  dt <- data.table::data.table(date = test_data[, Date],
                               open_tomorrow = test_data[, open_tomorrow],
                               close_today = test_data[, Close], p, buy, hold, sell)
  ## antager buy eksisterer
  first_buy <- dt[, which(buy == 1)][1]

  if (is.na(first_buy)) {
    dt_out <- data.table::data.table(buy_date = NA, sell_date = NA, buy_value = NA, sell_value = NA)
    dt_stats <- data.table::data.table(total_buy_value = NA, total_earning = NA, total_yield = NA)
    return(list("transactions" = dt_out, "stats" = dt_stats))
  }

  dt[1:first_buy, hold := 0]
  dt[1:first_buy, sell := 0]

  status <- "buy"
  dt[, action := "undefined"]
  dt[1:(first_buy - 1), action := "stay_away"]
  dt[first_buy, action := "buy"]

  for (i in (first_buy + 1):n) {
    if (status == "buy") {
      status <- "hold"
      dt$action[i] <- "hold"
    } else if (status == "hold" & dt[i, sell] == 0) {
      status <- "hold"
      dt$action[i] <- "hold"
    } else if (status == "hold" & dt[i, sell] == 1) {
      status <- "sell"
      dt$action[i] <- "sell"
    } else if (status == "sell") {
      status <- "stay_away"
      dt$action[i] <- "stay_away"
    } else if (status == "stay_away" & dt[i, buy] == 1) {
      status <- "buy"
      dt$action[i] <- "buy"
    } else if (status == "stay_away" & dt[i, buy] == 0) {
      status <- "stay_away"
      dt$action[i] <- "stay_away"
    }
  }

  sell_dates <- dt[action == "sell", date]

  if (length(sell_dates) > 0) {

    dt_list <- list()

    for (i in 1:length(sell_dates)) {
      sell_date <- sell_dates[i]
      dt_sell <- dt[date == sell_date, ]
      buy_date <- dt[date <= sell_date & action == "buy", max(date)]
      dt_buy <- dt[date == buy_date, ]
      buy_value <- dt_buy[, open_tomorrow]
      sell_value <- dt_sell[, open_tomorrow]
      sell_value_yesterday <- dt[.N, close_today]
      sell_value <- ifelse(!is.na(sell_value), sell_value, sell_value_yesterday)
      dt_status <- data.table::data.table(buy_date, sell_date, buy_value, sell_value)
      dt_status[, earning := sell_value - buy_value]
      dt_status[, yield := round(100 * (earning / buy_value), 3)]
      dt_list[[i]] <- dt_status
    }

    dt_out <- do.call("rbind", dt_list)
    total_buy_value <- dt_out[, max(buy_value)]
    total_earning <- dt_out[, sum(earning)] * 0.998 ## kurtage
    total_yield <- round(100 * (total_earning / total_buy_value), 3)

    dt_stats <- data.table::data.table(total_buy_value, total_earning, total_yield)

  } else {

    buy_date <- dt[, min(date)]
    buy_value <- dt[date == buy_date, open_tomorrow]
    today_value <- dt[.N - 1, open_tomorrow]

    dt_status <- data.table(buy_date, sell_date = NA, buy_value, sell_value = NA)
    dt_status[, earning := today_value - buy_value]
    dt_status[, yield := round(100 * (earning / buy_value), 3)]

    dt_out <- dt_status

    dt_stats <- data.table::data.table(total_buy_value = buy_value,
                           total_earning = dt_status[, earning],
                           total_yield = dt_status[, yield])

  }

  return(list("transactions" = dt_out, "stats" = dt_stats))

}
