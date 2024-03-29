#' Construct an `<epichains>` object
#'
#' @description
#' `new_epichains()` constructs an `<epichains>` object from a
#' supplied `<data.frame>` and extra attributes passed as individual arguments.
#' It is meant to be lazy and performant, by creating the object without
#' checking the arguments for correctness. It is not safe to call
#' `new_epichains()` on its own as is called within `epichains()`
#' after the arguments have been checked. To create an `<epichains>`
#' object, use `epichains()`.
#' @param tree_df a `<data.frame>` containing at least columns for
#' "infectee_id", "infector_id", and "generation". Also has optional columns
#' for "time", and "chain_id".
#' @param track_pop Was the susceptible population tracked? Logical.
#' @inheritParams .epichains
#' @author James M. Azam
#' @keywords internal
.new_epichains <- function(tree_df,
                               index_cases,
                               statistic,
                               offspring_dist,
                               stat_max,
                               track_pop) {
  # Assemble the elements of the object
  obj <- tree_df
  class(obj) <- c("epichains", class(obj))
  attr(obj, "index_cases") <- index_cases
  attr(obj, "statistic") <- statistic
  attr(obj, "offspring_dist") <- offspring_dist
  attr(obj, "stat_max") <- stat_max
  attr(obj, "track_pop") <- track_pop
  return(obj)
}

#' Create an `<epichains>` object
#'
#' @description
#' `epichains()` constructs an `<epichains>` object, which is
#' inherently an `<data.frame>` object that stores some of the inputs
#' passed to the `simulate_tree()` and `simulate_tree_from_pop()` and the
#' simulated output. The stored attributes are useful for downstream
#' analyses and reproducibility. This function checks the validity of the
#' object created to ensure it has the right columns and column types.
#'
#' An `<epichains>` object contains a `<data.frame>` of the simulated
#' outbreak tree with ids for each infector and infectee, generation, and
#' optionally, time, the number of initial cases used for the simulation,
#' the statistic that was tracked, and whether the susceptible population was
#' tracked.
#'
#' @inheritParams simulate_chains
#' @inheritParams .new_epichains
#'
#' @return An `<epichains>` object.
#' @author James M. Azam
#' @keywords internal
.epichains <- function(tree_df,
                      index_cases,
                      offspring_dist,
                      track_pop,
                      statistic = c("size", "length"),
                      stat_max = Inf) {
  # Check that inputs are well specified
  checkmate::assert_data_frame(tree_df, min.cols = 3, min.rows = index_cases)
  checkmate::assert_integerish(
    index_cases,
    any.missing = FALSE,
    len = 1L,
    lower = 1L,
    upper = max(tree_df$infectee_id, na.rm = TRUE)
  )
  checkmate::assert_string(statistic)
  statistic <- match.arg(statistic, choices = c("size", "length"))
  .check_offspring_func_valid(offspring_dist)
  checkmate::assert_logical(track_pop, len = 1L)
  checkmate::assert(
    is.infinite(stat_max),
    checkmate::check_integerish(stat_max, lower = 1L)
  )
  # Create <epichains> object
  epichains <- .new_epichains(
    tree_df = tree_df,
    index_cases = index_cases,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    track_pop = track_pop
  )

  # Validate the created object
  .validate_epichains(epichains)

  return(epichains)
}

#' Construct a `<epichains_summary>` object
#'
#' @description
#' `new_epichains_summary()` constructs an `<epichains_summary>` object from a
#' supplied `<vector>` of chain sizes or lengths. It also stores extra
#' attributes passed as individual arguments.
#'
#' `new_epichains_summary()` is meant to be lazy and performant, by creating
#' the object without checking the arguments for correctness. It is not safe
#' to call `new_epichains_summary()` on its own as is called within
#' `epichains_summary()` after the arguments have been checked. To create a
#' new `<epichains_summary>` object safely, use `epichains_summary()`.
#'
#' @param chains_summary A numeric `<vector>` of chain sizes and lengths.
#' @inheritParams .new_epichains
#' @inheritParams .simulate_chains
#' @author James M. Azam
#' @keywords internal
.new_epichains_summary <- function(chains_summary,
                                  index_cases,
                                  statistic,
                                  offspring_dist,
                                  stat_max) {
  # Assemble the elements of the object
  obj <- chains_summary
  class(obj) <- c("epichains_summary", class(chains_summary))
  attr(obj, "index_cases") <- index_cases
  attr(obj, "statistic") <- statistic
  attr(obj, "offspring_dist") <- offspring_dist
  attr(obj, "stat_max") <- stat_max
  return(obj)
}

#' Create an `<epichains_summary>` object
#'
#' @description
#' `epichains_summary()` constructs an `<epichains_summary>` object.
#'
#' An `<epichains_summary>` object is a `<vector>` of the simulated
#' tree sizes or lengths. It also stores information on the number of initial
#' cases used for the simulation, and the statistic that was tracked,
#' the intervention level.
#'
#' @inheritParams .new_epichains_summary
#'
#' @return An `<epichains_summary>` object.
#' @author James M. Azam
#' @keywords internal
.epichains_summary <- function(chains_summary,
                              index_cases,
                              offspring_dist,
                              statistic = c("size", "length"),
                              stat_max = Inf) {
  # chain_summary can sometimes contain infinite values, so check
  # that finite elements are integerish.
  checkmate::check_integerish(
    chains_summary[is.finite(chains_summary)],
    lower = 0,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    index_cases,
    any.missing = FALSE,
    lower = 1L,
    len = 1L
  )
  checkmate::assert_string(statistic)
  statistic <- match.arg(statistic, c("size", "length"))
  .check_offspring_func_valid(offspring_dist)
  checkmate::assert(
    is.infinite(stat_max),
    checkmate::check_integerish(stat_max, lower = 1L)
  )

  # Create <epichains_summary> object
  epichains_summary <- .new_epichains_summary(
    chains_summary,
    index_cases = index_cases,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max
  )

  # Validate the created object
  .validate_epichains_summary(epichains_summary)

  return(epichains_summary)
}

#' Print an `<epichains>` object
#'
#' @param x An `<epichains>` object.
#' @param ... Other parameters passed to `print()`.
#' @return Invisibly returns an `<epichains>`. Called for
#' side-effects.
#' @author James M. Azam
#' @export
print.epichains <- function(x, ...) {
  format(x, ...)
}

#' Print an `<epichains_summary>` object
#'
#' @param x An `<epichains_summary>` object.
#' @description
#' Prints a summary of the `<epichains_summary>` object. In particular, it
#' prints the number of index cases used for the simulation, and the range of
#' the statistic, represented as the maximum (`max_stat`) and minimum
#' (`min_stat`). If the minimum or maximum is infinite, it is represented as
#' `>= stat_max` where `stat_max` is the value of the censoring limit. See
#' `?epichains_summary()` for the definition of `stat_max`.
#' @param ... Not used.
#' @return Invisibly returns an `<epichains_summary>`. Called for
#' side-effects.
#' @author James M. Azam
#' @export
print.epichains_summary <- function(x, ...) {
  format(x, ...)
}

#' Format method for `<epichains>` class
#'
#' @param x An `<epichains>` object.
#' @param ... Not used.
#' @return Invisibly returns an `<epichains>`.
#' Called for printing side-effects.
#' @author James M. Azam
#' @export
format.epichains <- function(x, ...) {
  # check that x is an <epichains> object
  .validate_epichains(x)

  writeLines(sprintf("`<epichains>` object\n"))

  # print head of the object
  writeLines("< tree head (from first known infector_id) >\n")
  print(head(x))

  # print summary information
  writeLines(
    c(
      sprintf(
        "%s",
        "\n"
      ),
      sprintf(
        "Trees simulated: %s",
        attr(x, "index_cases")
      ),
      sprintf(
        "Number of infectors (known): %s",
        length(unique(x$infector_id))
      ),
      sprintf(
        "Number of generations: %s",
        max(x$generation)
      )
    )
  )

  # Offer more information to view the full dataset
  writeLines(
    sprintf(
      "%s %s", "Use `as.data.frame(<object_name>)`",
      "to view the full output in the console."
    )
  )
  invisible(x)
}

#' Format method for `<epichains_summary>` class
#'
#' @param x An `<epichains_summary>` object.
#' @param ... Not used.
#' @return Invisibly returns an `<epichains_summary>`. Called for printing
#' side-effects.
#' @author James M. Azam
#' @export
format.epichains_summary <- function(x, ...) {
  # check that x is an <epichains_summary> object
  .validate_epichains_summary(x)

  # summarise the information stored in x
  statistics <- summary(x)

  writeLines(sprintf("`epichains_summary` object \n"))
  print(as.vector(x))
  writeLines(
    sprintf(
      "\n Number of trees simulated: %s",
      statistics[["unique_trees"]]
    )
  )
  writeLines(
    c(
      sprintf(
        "\n Simulated tree %ss: \n",
        attr(x, "statistic", exact = TRUE)
      ),
      sprintf(
        "Max: %s",
        ifelse(
          is.infinite(
            statistics[["max_stat"]]),
            paste0(">=", attr(x, "stat_max")
          ),
          statistics[["max_stat"]]
        )
      ),
      sprintf(
        "Min: %s",
        ifelse(
          is.infinite(
            statistics[["min_stat"]]),
          paste0(">=", attr(x, "stat_max")
          ),
          statistics[["min_stat"]]
        )
      )
    )
  )

  invisible(x)
}

#' Summary method for `<epichains>` class
#'
#' This calculates the chain statistic (size/length) for the simulated
#' chains and returns an object with the same information as that returned
#' by an equivalent `simulate_summary()` call.
#'
#' @param object An `<epichains>` object.
#' @param ... Not used.
#'
#' @return An `<epichains_summary>` object containing the chain summary
#' statistics as follows:
#' * "size": the total number of offspring produced by a chain before it
#' goes extinct.
#' * "length": the number of generations achieved by a chain before
#' it goes extinct.
#' @author James M. Azam
#' @export
#' @examples
#' # Using a negative binomial offspring distribution and simulating from a
#' # finite population up to chain size 10.
#' set.seed(32)
#' sim_chains_nbinom <- simulate_chains(
#'   index_cases = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   stat_max = 10,
#'   mu = 2,
#'   size = 0.2
#' )
#' # Summarise the simulated chains
#' sim_chains_nbinom_summary <- summary(sim_chains_nbinom)
#' sim_chains_nbinom_summary
#'
#' # Same results can be obtained using `simulate_summary()`
#' set.seed(32)
#' sim_summary_nbinom <- simulate_summary(
#'   index_cases = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   stat_max = 10,
#'   mu = 2,
#'   size = 0.2
#' )
#' sim_summary_nbinom
#'
#' # Check that the results are the same
#' setequal(sim_chains_nbinom_summary, sim_summary_nbinom)
summary.epichains <- function(object, ...) {
  # Check that object has <epichains> class
  .validate_epichains(object)

  # Get relevant attributes for computing summaries
  statistic <- attr(object, "statistic")
  index_cases <- attr(object, "index_cases")

  # Initialize summary statistics
  chain_summaries <- vector(length = index_cases, mode = "integer")
  # Calculate the summary statistic based on the specified statistic type
  if (statistic == "size") {
    # size is the number of infectees produced by a chain before it goes
    # extinct.
    chain_summaries <- as.numeric(table(object$infectee_id))
  } else {
    # length is the number of infectors generations a chain produces before
    # it goes extinct.
    for (i in seq_len(index_cases)) {
      chain_generations <- object[object$infectee_id == i, "generation"]
      chain_summaries[i] <- max(chain_generations)
    }
  }
  # Get other required attributes from passed object
  stat_max <- attr(object, "stat_max")
  offspring_dist <- attr(object, "offspring_dist")

  # Apply truncation
  chain_summaries[chain_summaries >= stat_max] <- Inf

  # Return an <epichains_summary> object
  chain_summaries <- .epichains_summary(
    chains_summary = chain_summaries,
    index_cases = index_cases,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max
  )
  return(chain_summaries)
}

#' Summary method for `<epichains_summary>` class
#'
#' @param object An `<epichains_summary>` object.
#' @param ... Not used.
#'
#' @return A list of chain summaries. The list contains the following
#' elements:
#' * `index_cases`: the number of index cases used to simulate the chains.
#' * `max_stat`: the maximum chain statistic (size/length) achieved by the
#' chains.
#' * `min_stat`: the minimum chain statistic (size/length) achieved by the
#' chains.
#' @author James M. Azam
#' @export
summary.epichains_summary <- function(object, ...) {
  # Check that object has <epichains_summary> class
  .validate_epichains_summary(object)

  # Get the summaries
  index_cases <- attr(object, "index_cases", exact = TRUE)


  if (all(is.infinite(object))) {
    max_stat <- min_stat <- Inf
  } else {
    max_stat <- max(object)
    min_stat <- min(object)
  }

  out <- list(
    index_cases = index_cases,
    max_stat = max_stat,
    min_stat = min_stat
  )

  return(out)
}

#' Test if x is an `epichains` object
#'
#' @param x An R object.
#'
#' @return Logical; `TRUE` if the object is an `<epichains>` and `FALSE`
#' otherwise.
#' @author James M. Azam
#' @keywords internal
.is_epichains <- function(x) {
  inherits(x, "epichains")
}

#' Test if x is an `epichains_summary` object
#'
#' @param x An R object.
#'
#' @return Logical; `TRUE` if the object is an `<epichains_summary>` and
#' `FALSE` otherwise.
#' @author James M. Azam
#' @keywords internal
.is_epichains_summary <- function(x) {
  inherits(x, "epichains_summary")
}

#' Validate an `<epichains>` object
#'
#' @param x An `<epichains>` object.
#'
#' @return Invisibly returns the object if it is valid.
#' @author James M. Azam
#' @keywords internal
.validate_epichains <- function(x) {
  if (!.is_epichains(x)) {
    stop("Object must have an `<epichains>` class")
  }

  # check for class invariants
  stopifnot(
    "object does not contain the correct columns" =
      c("sim_id", "infector_id", "generation") %in%
      colnames(x),
    "column `sim_id` must be a numeric" =
      is.numeric(x$sim_id),
    "column `infector_id` must be a numeric" =
      is.numeric(x$infector_id),
    "column `generation` must be a numeric" =
      is.numeric(x$generation)
  )

  invisible(x)
}

#' Validate an `<epichains_summary>` object
#'
#' @param x An `<epichains_summary>` object.
#'
#' @return Invisibly returns the object if it is valid.
#' @author James M. Azam
#' @keywords internal
.validate_epichains_summary <- function(x) {
  if (!.is_epichains_summary(x)) {
    stop("Object must have an `<epichains_summary>` class")
  }

  invisible(x)
}

#' `head` and `tail` method for `<epichains>` class
#'
#' @param x An `<epichains>` object.
#' @param ... Further arguments passed to or from other methods.
#' @importFrom utils head
#' @importFrom utils tail
#' @return An object of class `<data.frame>`.
#' @author James M. Azam
#' @export
#' @details
#' This returns the top rows of an `<epichains>` object. Note that
#' the object is originally sorted by `sim_id` and `infector_id` and the first
#' unknown infectors (NA) have been dropped from
#' printing method.
#'
#' To view the full output, use `as.data.frame(<object_name>)`.
#' @examples
#' set.seed(32)
#' chains_pois_offspring <- simulate_chains(
#'   index_cases = 10,
#'   statistic = "size",
#'   offspring_dist = rpois,
#'   stat_max = 10,
#'   generation_time = function(n) rep(3, n),
#'   lambda = 2
#' )
#' head(chains_pois_offspring)
head.epichains <- function(x, ...) {
  # print head of the simulation output from the first known infector_id
  x <- x[!is.na(x$infector_id), ]
  return(
    utils::head(as.data.frame(x), ...)
  )
}

#' @rdname head.epichains
#' @export
#' @examples
#' set.seed(32)
#' chains_pois_offspring <- simulate_chains(
#'   index_cases = 10,
#'   statistic = "size",
#'   offspring_dist = rpois,
#'   stat_max = 10,
#'   generation_time = function(n) rep(3, n),
#'   lambda = 2
#' )
#' tail(chains_pois_offspring)
tail.epichains <- function(x, ...) {
  return(
    utils::tail(as.data.frame(x), ...)
  )
}

#' Aggregate cases in `<epichains>` objects by "generation" or "time", if
#' present
#'
#' @description
#' This function provides a quick way to create a time series of cases over
#' generation or time (if serials_dist was specified) from simulated
#' `<epichains>` objects.
#'
#' @param x An `<epichains>` object.
#' @param by The variable to aggregate by; A character string with options
#' "time" and "generation".
#' @param ... Not used.
#' @importFrom stats aggregate
#' @return A `<data.frame>` object of cases by `by`.
#' @author James M. Azam
#' @export
#' @examples
#' set.seed(32)
#' chains <- simulate_chains(
#'   index_cases = 10,
#'   statistic = "size",
#'   offspring_dist = rpois,
#'   stat_max = 10,
#'   generation_time = function(n) rep(3, n),
#'   lambda = 2
#' )
#' chains
#'
#' # Aggregate cases per time
#' cases_per_time <- aggregate(chains, by = "time")
#' head(cases_per_time)
#'
#' # Aggregate cases per generation
#' cases_per_gen <- aggregate(chains, by = "generation")
#' head(cases_per_gen)
aggregate.epichains <- function(x,
                                     by = c(
                                       "time",
                                       "generation"
                                     ),
                                     ...) {
  .validate_epichains(x)
  checkmate::assert_string(by)
  # Get grouping variable
  by <- match.arg(by)

  out <- if (by == "time") {
    if (is.null(x$time)) {
      stop(
        "Object must have a time column. ",
        "To simulate time, specify `serials_dist` ",
        "in the `simulate_tree()` setup."
      )
    }
    # Count the number of cases per generation
    stats::aggregate(
      list(cases = x$sim_id),
      list(time = x$time),
      FUN = NROW
    )
  } else if (by == "generation") {
    # Count the number of cases per time
    stats::aggregate(
      list(cases = x$sim_id),
      list(generation = x$generation),
      FUN = NROW
    )
  }

  return(out)
}
