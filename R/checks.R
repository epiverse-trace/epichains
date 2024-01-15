#' Check if constructed random number generator for offspring exists
#'
#' @param roffspring_name Constructed random offspring sampler: a character
#' string corresponding to the R distribution function (e.g., "rpois" for
#' Poisson.
#' @keywords internal
check_offspring_func_valid <- function(roffspring_name) {
  if (!(exists(roffspring_name)) ||
        !checkmate::test_function(get(roffspring_name))) {
    stop("Function ", roffspring_name, " does not exist.")
  }
}


#' Check if the generation_time argument is specified as a function
#'
#' @inheritParams simulate_tree
#'
#' @keywords internal
check_generation_time_valid <- function(generation_time) {
  checkmate::assert_function(generation_time, nargs = 1)
  x <- generation_time(10)
  checkmate::assert_numeric(x, len = 10)
}
