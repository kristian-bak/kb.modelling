#' Target names
#' @description `f_target_names` returns a character vector with possible target names.
#' The number indicates stock increase(= 1) and decrease (= 0) x days in future. So, target_day1 is a flag for whether or not the stock increased on day i + 1 looking at day i.
#' @export
#'
f_target_names <- function() {
  return(c("target_day1", "target_day5", "target_day10", "target_day20", "target_day50"))
}
