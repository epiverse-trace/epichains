#' Check if offspring argument is specified as a character string
#'
#' @param offspring_dist Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers).
#' @keywords internal
check_offspring_valid <- function(offspring_dist) {
  if (!checkmate::test_string(offspring_dist)) {
    stop(sprintf(
      "%s %s",
      "'offspring_dist' must be specified as a character string.",
      "Did you forget to enclose it in quotes?"
    ))
  }
}


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
#' @param generation_time The generation interval function; the name of a
#' user-defined named or anonymous function with only one argument `n`,
#' representing the number of generation intervals to sample.
#'
#' @keywords internal
check_generation_time_valid <- function(generation_time) {
  if (!checkmate::test_function(generation_time, nargs = 1)) {
    stop(sprintf(
      "%s %s",
      "The `generation_time` argument must be a function",
      "(see details in ?simulate_tree)."
    ))
  }
  x <- generation_time(10)
  if (!checkmate::test_numeric(x, len = 10)) {
    stop(
      "The return values of `serials_dist` must be a numeric vector of length ",
      "`n`."
    )
  }
}


#' Check that `ntrees` is greater than 0 and not infinity
#'
#' @param ntrees Number of trees to simulate.
#'
#' @keywords internal
check_ntrees_valid <- function(ntrees) {
  if (!checkmate::test_count(ntrees, positive = TRUE)) {
    stop("`ntrees` must be > 0 but less than `Inf`")
  }
}
