#' Check if offspring argument is specified as a character string
#'
#' @param offspring_sampler Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers).
#' @keywords internal
check_offspring_valid <- function(offspring) {
  if (!is.character(offspring)) {
    stop(sprintf(
      "%s %s",
      "'offspring' must be specified as a character string.",
      "Did you forget to enclose it in quotes?"
    ))
  }
}


#' Check if constructed random number generator for offspring exists
#'
#' @param roffspring_name
#' @keywords internal
check_offspring_func_valid <- function(roffspring_name) {
  if (!(exists(roffspring_name)) || !is.function(get(roffspring_name))) {
    stop("Function ", roffspring_name, " does not exist.")
  }
}


#' Check if the serials_sampler argument is specified as a function
#'
#' @param serials_sampler
#'
#' @keywords internal
check_serial_valid <- function(serials_sampler) {
  if (!is.function(serials_sampler)) {
    stop(sprintf(
      "%s %s",
      "The `serials_sampler` argument must be a function",
      "(see details in ?sim_chain_tree)."
    ))
  }
}


#' Check that nchains is greater than 0 and not infinite
#'
#' @param nchains
#'
#' @keywords internal
check_nchains_valid <- function(nchains) {
  if (nchains < 1 || is.infinite(nchains)) {
    stop("`nchains` must be > 0 but less than `Inf`")
  }
}
