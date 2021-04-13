#' Stepwise xgboost
#' @param data data.table or data.frame
#' @param target_name response variable name. See options in f_target_names()
#' @param init_var initial variables that always will be selected. Default is missing.
#' @param var_names feature names
#' @param n_max_steps maximum number of steps. Default is 10
#' @export
#'

f_step <- function(data, target_name, init_var = NULL, var_names = NULL, n_max_steps = 10) {

  ## Filter out NA's in target variable
  data <- data[!is.na(get(target_name)), ]

  ## data splitting
  outcome <- f_split_train_test(data = data)
  train_data <- outcome$train_data
  test_in_data <- outcome$test_in_data
  test_out_data <- outcome$test_out_data

  var_names <- f_var_names(data = data)
  n <- nrow(train_data)
  weight <- f_weight_observations(n = n)

  go <- TRUE
  i <- 1
  added_var <- NULL
  p <- length(var_names)
  potental_var <- var_names
  test_auc <- matrix(NA, nrow = n_max_steps, ncol = p)
  auc_now <- 0

  while (i < n_max_steps & go) {

    for (j in 1:p) {

      predictors <- c(init_var, added_var, potental_var[j])
      m_xgboost <- f_train(df_train = train_data,
                           target = target_name,
                           var = predictors,
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
      cat("\r", i, ".", j, "of", n_max_steps, ".", p, "\r")

    }

    auc_new <- max(test_auc[i, ], na.rm = TRUE)

    if (auc_new <= auc_now) {
      go <- FALSE
    } else {
      auc_now <- auc_new
      add_var_id <- which.max(test_auc[i, ])
      added_var <- c(added_var, potental_var[add_var_id])
      potental_var <- potental_var[!potental_var %in% added_var]
    }

    p <- length(potental_var)
    i <- i + 1

  }

  set.seed(1)
  m_xgboost <- f_train(df_train = train_data,
                       target = target_name,
                       var = c(init_var, added_var),
                       nrounds = 250,
                       max.depth = 8,
                       eta = 0.1,
                       min_child_weight = 20,
                       early_stopping_rounds = 25,
                       subsample = 1,
                       colsample_bytree = 0.7,
                       gamma = 1,
                       weight = weight)
  pred_train <- f_predict(model = m_xgboost, data = train_data)
  pred_test <- f_predict(model = m_xgboost, data = test_in_data)
  pred_out <- f_predict(model = m_xgboost, data = test_out_data)
  auc_train <- Hmisc::somers2(x = pred_train, y = train_data[[target_name]])[1]
  auc_test <- Hmisc::somers2(x = pred_test, y = test_in_data[[target_name]])[1]
  auc_out <- Hmisc::somers2(x = pred_out, y = test_out_data[[target_name]])[1]
  auc_list <- list("auc_train" = auc_train,
                   "auc_test" = auc_test,
                   "auc_out" = auc_out)

  return(list("variables" = c(init_var, added_var),
              "step_test_auc" = auc_now,
              "auc_list" = auc_list))
}
