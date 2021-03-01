#' Preparing data for training xgboost
#' @param data data from f_load. Should be a data.table object
#' @export
#' @return data with target and indicators


f_data_prep <- function(data) {

  Change <- Open <- change_tomorrow <- flag_change_tomorrow <- open_tomorrow <- NULL

  data[, change_tomorrow := f_lag(Change, n = 1)]
  data[, flag_change_tomorrow := 1 * (change_tomorrow >= 0)]
  data[, open_tomorrow := f_lag(Open, n = 1)]
  data <- f_indicator(data = data)
  return(data)

}
