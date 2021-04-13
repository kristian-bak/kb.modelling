#' Training xgboost models
#' @param df_train train data. Should be data.table object
#' @param target_name target variable. Should be a string with a variable present in train data.
#' @param var variables used to train the model. Should be a character vector. Default value is NULL, indicating that all variables available from f_indicators will be used.
#' @param nrounds max number of iterations. Default is 32
#' @param max.depth Max depth of the trees, i.e. the number of splits made.
#' @param eta step size of each boosting step. Large eta may lead to unstable results. Default is 1.
#' @param min_child_weight minimum counts in a child. The algorithm stops if splitting leads to leaf node with fewer than min_child_weight instances.
#' @param early_stopping_rounds stopping criteria. If performance is not improved after k(= early_stopping_rounds) iterations, the algorithm stops.
#' @param subsample train proportion for each fold in cross validation. Should be a number between 0 and 1.
#' @param colsample_bytree subsample proportion for each tree. Should be a number between 0 and 1.
#' @param gamma  minimum loss reduction required to make a further partition on a leaf node of the tree. The larger, the more conservative the algorithm will be.
#' @param ... additional parameters passed to xgboost.
#' @export
#' @return xgboost object

f_train <- function(df_train,
                    target_name,
                    var = NULL,
                    nrounds = 32,
                    max.depth = 4,
                    eta = 1,
                    min_child_weight = 50,
                    early_stopping_rounds = 25,
                    subsample = 0.9,
                    colsample_bytree = 0.9,
                    gamma = 1, ...) {

  if (is.null(var)) {
    var <- names(df_train)[!names(df_train) %in% c("Company", "Ticker", "Date", "Low", "High",
                                                   "Close", "Adjusted", "Change",
                                                   "change_tomorrow", "flag_change_tomorrow",
                                                   "open_tomorrow")]
  }

  mat_train <- as.matrix(df_train[, var, with = FALSE])
  m <- xgboost::xgboost(data = mat_train,
               label = df_train[[target_name]],
               nrounds = nrounds,
               max.depth = max.depth,
               eta = eta,
               early_stopping_rounds = early_stopping_rounds,
               subsample = subsample,
               colsample_bytree = colsample_bytree,
               gamma = gamma,
               objective = "binary:logistic",
               eval_metric = "auc",
               min_child_weight = min_child_weight,
               verbose = 0, ...)

  return(m)

}

#' Predicting from xgboost models
#' @param model xgboost object obtained from f_train
#' @param data data to calculate predictions on. Should be a data.table object
#' @export
#' @importFrom stats predict
#' @return a numeric vector with predictions from the model

f_predict <- function(model, data) {
  var <- model$feature_names
  mat_data <- as.matrix(data[, var, with = FALSE])
  predict(model, newdata = mat_data)
}
