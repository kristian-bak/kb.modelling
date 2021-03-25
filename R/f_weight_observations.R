#' Weight function
#' @param x vector
#' @export
#'
f_weight <- function(x) {
  n <- length(x)
  exp(-x / n)
}

#' Weight observations with exponential decay
#' @param n number of rows in data
#' @export
#'
f_weight_observations <- function(n) {
  base_weight <- rev(f_weight(x = 1:n))
  m <- sum(base_weight)
  d <- n - m
  weight <- (d / n) + base_weight
  return(weight)
}
