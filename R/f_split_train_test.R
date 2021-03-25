#' Train test splitting data
#' @param data data
#' @param split_pct split percent. A number between 0 and 1. Default is a 80-20 split. An out of sample period is provided as well, so data is split into 80 training/test and 20 out of sample. The 80 training/tet is split into 80-20 training and 20 test
#' @export
#'
f_split_train_test <- function(data, split_pct = 0.8) {
  set.seed(1)
  m <- floor(split_pct * nrow(data))
  id <- 1:m
  train_id <- sample(x = id, size = floor(split_pct * m), replace = FALSE)
  test_id <- id[!id %in% train_id]
  train_data <- data[train_id, ]
  test_in_data <- data[test_id, ]
  test_out_data <- data[(m + 1):.N, ]
  outcome <- list("train_data" = train_data,
                  "test_in_data" = test_in_data,
                  "test_out_data" = test_out_data)
  return(outcome)
}
