#' Accuracy
#' @param x vector with predictions
#' @param y vector with binary outcome
#' @param c cutoff. Should be between 0 and 1. Default is 0.5
#' @export
#'
f_accuracy <- function(x, y, c = 0.5) {

  if (length(x) != length(y)) {
    stop("Length of x and y should match")
  }

  tp <- sum(x >= c & y == 1)
  tn <- sum(x < c & y == 0)
  n <- length(x)
  accuracy <- (tp + tn) / n

  return(accuracy)

}
