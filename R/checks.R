#' Check if offspring argument is specified as a character string
#'
#' @param offspring
#'
#' @return
#' @export
#' @keywords internal
#' @examples
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
#'
#' @return
#' @export
#'
#' @examples check_offspring_exists("rpois")
check_offspring_func_valid <- function(roffspring_name) {
  if (!(exists(roffspring_name)) || !is.function(get(roffspring_name))) {
    stop("Function ", roffspring_name, " does not exist.")
  }
}


#' Check if the serials_sampler argument is specified as a function
#'
#' @param serials_sampler
#'
#' @return
#' @export
#' @keywords internal
#' @examples
check_serial_valid <- function(serials_sampler) {
  if (!is.function(serials_sampler)) {
    stop(sprintf(
      "%s %s",
      "The `serials_sampler` argument must be a function",
      "(see details in ?sim_chain_tree)."
    ))
  }
}


check_nchains_valid <- function(nchains) {
  if (nchains < 1 || is.infinite(nchains)) {
    stop("`nchains` must be > 0 but less than `Inf`")
  }
}
