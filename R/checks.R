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

#' Check inputs to `simulate_chains()` and `simulate_summary()`
#'
#' @inheritParams simulate_chains
#' @return NULL; called for side effects
#' @keywords internal
.check_sim_args <- function(
    n_chains,
    statistic,
    offspring_dist,
    stat_threshold,
    pop,
    percent_immune) {
  # Input checking
  checkmate::assert_count(
    n_chains,
    positive = TRUE
  )
  # check that offspring is a function with argument "n"
  .check_offspring_func_valid(offspring_dist)
  # check that arguments related to the statistic are valid
  .check_statistic_args(
    statistic,
    stat_threshold
  )
  checkmate::assert(
    is.infinite(pop) ||
      checkmate::assert_integerish(pop, lower = 1)
  )
  checkmate::assert_number(
    percent_immune,
    lower = 0, upper = 1
  )
  invisible(NULL)
}

#' Check that the `statistic` and `stat_threshold` arguments are valid
#'
#' @inheritParams simulate_chains
#' @description
#' The function treats these two arguments as related and checks
#' them in one place to remove repeated checks in several places in the
#' package.
#' @return NULL; called for side effects
#' @keywords internal
.check_statistic_args <- function(statistic,
                                  stat_threshold) {
  checkmate::assert_choice(
    statistic,
    choices = c("size", "length")
  )
  # check that stat_threshold is an integer or Inf.
  checkmate::assert(
    checkmate::anyInfinite(stat_threshold),
      checkmate::check_integerish(
        stat_threshold,
        lower = 1
    ),
    combine = "or"
  )
}

#' Check inputs that control time events
#'
#' @description
#' This function checks the time-related inputs, i.e., start time of each chain,
#' `t0`, the end time of the simulation, `tf`, and the generation time,
#' generation_time. It also checks that the generation_time argument is
#' specified if `tf` is specified as these go hand-in-hand.
#'
#' @param tf_specified <logical>; Whether the `tf` argument is specified. Only
#' makes sense in the context where this function is called, i.e., in
#' [simulate_chains()]. If `tf` is specified, generation_time must be specified.
#' @inheritParams simulate_chains
#' @return NULL; called for side effects
#' @keywords internal
.check_time_args <- function(tf_specified,
                             tf,
                             generation_time,
                             t0) {
  # if tf is specified, generation_time must be specified too
  if (!is.null(generation_time)) {
    .check_generation_time_valid(generation_time)
  } else if (tf_specified) {
      stop("If `tf` is specified, `generation_time` must be specified too.")
  }
  checkmate::assert_number(
    tf,
    lower = 0
  )
  checkmate::assert_numeric(
    t0,
    lower = 0,
    finite = TRUE
  )
}
