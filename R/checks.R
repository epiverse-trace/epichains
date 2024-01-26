#' Check if constructed random number generator for offspring exists
#' and has an `n` argument.
#'
#' @inheritParams simulate_chains
#' @keywords internal
.check_offspring_func_valid <- function(offspring_dist) {
  checkmate::assert_function(offspring_dist, args = "n")
}


#' Check if the generation_time argument is specified as a function
#'
#' @inheritParams simulate_chains
#'
#' @keywords internal
.check_generation_time_valid <- function(generation_time) {
  checkmate::assert_function(generation_time, nargs = 1)
  x <- generation_time(10)
  checkmate::assert_numeric(x, len = 10)
}
