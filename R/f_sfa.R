#' Single feature analysis
#' @param ticker ticker name passed as string
#' @param from_date load data since from_date. Default is 2016-01-01.
#' @param offline logical. If TRUE, data will be loaded from data folder.
#' @param target_name target_name
#' @param var_names variables to loop through xgboost model. Default is NULL which means all variables from f_indicators will be used.
#' @param b number of bootstrap iterations
#' @export
#'
f_sfa <- function(ticker, from_date = "2016-01-01", offline = FALSE, target_name, var_names = NULL, b) {

  data <- f_load_data(ticker = ticker, from_date = from_date, offline = offline)

  outcome <- f_split_train_test(data = data)
  train_data <- outcome$train_data
  test_in_data <- outcome$test_in_data
  ## test_out_of_data will not be used in sfa, otherwise out of sample auc will be falsely boosted

  var_names <- f_var_names(data = data, var_names = var_names)
  p <- length(var_names)

  n <- floor(0.8 * nrow(train_data))
  weight <- f_weight_observations(n)

  test_auc <- matrix(NA, nrow = p, ncol = b)

  for (i in 1:p) {

    for (j in 1:b) {

      train_id <- sample(x = 1:nrow(train_data),
                         size = floor(0.8 * nrow(train_data)),
                         replace = FALSE)
      sub_train_data <- train_data[train_id, ]
      m_xgboost <- f_train(df_train = sub_train_data,
                           target_name = target_name,
                           var = var_names[i],
                           nrounds = 250,
                           max.depth = 8,
                           eta = 0.1,
                           min_child_weight = 20,
                           early_stopping_rounds = 25,
                           subsample = 1,
                           colsample_bytree = 0.7,
                           gamma = 1,
                           weight = weight)
      pred <- f_predict(model = m_xgboost, data = test_in_data)
      test_auc[i, j] <- Hmisc::somers2(x = pred, y = test_in_data[[target_name]])[1]
      flush.console()
      cat("\r", i, ".", j, "of", p, ".", b, "\r")
    }

  }

  rownames(test_auc) <- var_names
  return(test_auc)

}
