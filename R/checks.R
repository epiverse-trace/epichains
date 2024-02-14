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
#' @param sim_func <character>; The simulation function to check
#'
#' @inheritParams simulate_chains
#' @return NULL; called for side effects
#' @keywords internal
.check_sim_args <- function(
    func_name = c("simulate_chains", "simulate_summary"),
    index_cases,
    statistic,
    offspring_dist,
    stat_max,
    pop,
    percent_immune,
    generation_time = NULL,
    t0 = NULL,
    tf = NULL) {
  # Get the function name
  func_name <- match.arg(func_name)
  # Input checking
  checkmate::assert_count(
    index_cases,
    positive = TRUE
  )
  # check that offspring is a function with argument "n"
  .check_offspring_func_valid(offspring_dist)
  # check that arguments related to the statistic are valid
  .check_statistic_args(
    statistic,
    stat_max
  )
  checkmate::assert(
    is.infinite(pop) ||
      checkmate::assert_integerish(pop, lower = 1)
  )
  checkmate::assert_number(
    percent_immune,
    lower = 0, upper = 1
  )

  if (func_name == "simulate_chains") {
    # Check generation time is properly specified and if tf
    # is specified, generation_time is also specified
    if (!missing(generation_time)) {
      .check_generation_time_valid(generation_time)
    } else if (!missing(tf)) {
      stop("If `tf` is specified, `generation_time` must be specified too.")
    }
    checkmate::assert_numeric(
      t0,
      lower = 0, finite = TRUE
    )
    checkmate::assert_number(
      tf,
      lower = 0
    )
  }
  invisible(NULL)
}

#' Check that the statistic and stat_max arguments are valid
#'
#' @inheritParams simulate_chains
#' @description
#' The function treats these two arguments as related and checks
#' them in one place to remove repeated checks in several places in the
#' package.
#'
#'
#' @return NULL; called for side effects
#' @keywords internal
.check_statistic_args <- function(statistic,
                                  stat_max){
  checkmate::assert_choice(
    statistic,
    choices = c("size", "length")
  )
  checkmate::assert(
    is.infinite(stat_max) ||
      checkmate::assert_integerish(
        stat_max,
        lower = 0,
        null.ok = FALSE
    )
  )
}
