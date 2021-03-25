#' Report from xgboost predictions
#' @param ticker ticker name
#' @param target_name target name. See options in `f_target_names()`. Default is target_day1
#' @param from date when loading data
#' @param var_names predictors used to train xgboost model. Default is NULL, meaning all predictors from f_indicators will be selected.
#' @param buy_cut cut off for when to buy based on predictions from xgboost. Should be between 0 and 1.
#' @param sell_cut cut off for when to sell based on predictions from xgboost. Should be between 0 and 1.
#' @param importance_plot logical. If TRUE, importance plot will be calculated. Default is FALSE.
#' @param shap_plot logical. If TRUE, shap plot will be calculated. Default is FALSE
#' @param top_n top n variables to show in importance plot.
#' @param offline logical. If TRUE, data will be loaded from data folder
#' @param mode plot mode. Should be "candlesticks" or "lines". Default is "candlesticks".
#' @importFrom xgboost xgb.ggplot.importance
#' @export
#'
f_report <- function(ticker, target_name = "target_day1", from_date = "2016-01-01",
                     var_names = NULL, buy_cut = 0.6, sell_cut = 0.4,
                     importance_plot = FALSE, shap_plot = FALSE, top_n = 7,
                     offline = FALSE, mode = "candlesticks") {

  start_time <- Sys.time()

  ## data loading
  data <- f_load_data(ticker = ticker, from_date = from_date, offline = offline)

  ## Filter out NA's in target variable
  data <- data[!is.na(get(target_name)), ]

  ## data splitting
  outcome <- f_split_train_test(data = data)
  train_data <- outcome$train_data
  test_in_data <- outcome$test_in_data
  test_out_data <- outcome$test_out_data

  var_names <- f_var_names(data = data, var_names = var_names)

  n <- nrow(train_data)
  weight <- f_weight_observations(n = n)

  m_xgboost <- f_train(df_train = train_data,
                       target = target_name,
                       var = var_names,
                       nrounds = 250,
                       max.depth = 8,
                       eta = 0.1,
                       min_child_weight = 20,
                       early_stopping_rounds = 25,
                       subsample = 1,
                       colsample_bytree = 0.7,
                       gamma = 1,
                       weight = weight)
  data$prediction <- f_predict(model = m_xgboost, data = data)

  if (importance_plot) {
    importance_matrix <- xgboost::xgb.importance(model = m_xgboost)
    importance_plot <- xgboost::xgb.ggplot.importance(importance_matrix)
  } else {
    importance_plot <- NULL
  }

  if (shap_plot) {
    mat_train <- as.matrix(train_data[, var_names, with = FALSE])
    shap_plot <- plot_shap(xgb_model = m_xgboost, mat_train = mat_train, top_n = top_n)
  } else {
    shap_plot <- NULL
  }

  train_data[, p := f_predict(model = m_xgboost, data = train_data)]
  test_in_data[, p := f_predict(model = m_xgboost, data = test_in_data)]
  test_out_data[, p := f_predict(model = m_xgboost, data = test_out_data)]

  train_auc <- Hmisc::somers2(x = train_data[, p], y = train_data[[target_name]])[1]
  test_auc <- Hmisc::somers2(x = test_in_data[, p], y = test_in_data[[target_name]])[1]
  out_auc <- Hmisc::somers2(x = test_out_data[, p], y = test_out_data[[target_name]])[1]
  auc_list <- list("train_auc" = train_auc, "test_auc" = test_auc, "out_auc" = out_auc)

  dt_transactions <- f_trade(test_out_data, buy_cut = buy_cut, sell_cut = sell_cut)

  p <- f_plot_trade(test_out_data, dt_transactions, mode = mode)

  f_cor <- function(data, target_name, x) {
    subdata <- na.omit(data[, c(target_name, x), with = FALSE])
    subdata[, cor(get(target_name), get(x))]
  }

  cor_res <- sapply(var_names, f_cor, data = data, target_name = target_name)
  names(cor_res) <- NULL
  df_cor <- data.frame("var" = var_names, "cor" = cor_res)
  df_cor <- df_cor %>%
    dplyr::arrange(cor)

  end_time <- Sys.time()

  comp_time <- end_time - start_time

  return(list(
    "data" = data,
    "nrow" = list("train_data" = nrow(train_data),
                  "test_in_data" = nrow(test_in_data),
                  "test_out_data" = nrow(test_out_data)),
    "importance_plot" = importance_plot,
    "shap_plot" = shap_plot,
    "auc" = auc_list,
    "trade_plot" = p,
    "cor" = df_cor,
    "comp_time" = comp_time))
}
