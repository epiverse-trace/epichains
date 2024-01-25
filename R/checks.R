#' Check if constructed random number generator for offspring exists
#'
#' @param roffspring_name Constructed random offspring sampler: a character
#' string corresponding to the R distribution function (e.g., "rpois" for
#' Poisson.
#' @keywords internal
check_offspring_func_valid <- function(roffspring_name) {
  checkmate::assert(
    exists(roffspring_name) ||
      checkmate::assert_function(get(roffspring_name)),
    .var.name = "offspring_dist"
  )
}


#' Check if the generation_time argument is specified as a function
#'
#' @inheritParams simulate_chains
#'
#' @keywords internal
check_generation_time_valid <- function(generation_time) {
  checkmate::assert_function(generation_time, nargs = 1)
  x <- generation_time(10)
  checkmate::assert_numeric(x, len = 10)
}
