#' Construct an `<epichains_tree>` object
#'
#' @description
#' `new_epichains_tree()` constructs an `<epichains_tree>` object from a
#' supplied `<data.frame>` and extra attributes passed as individual arguments.
#' It is meant to be lazy and performant, by creating the object without
#' checking the arguments for correctness. It is not safe to call
#' `new_epichains_tree()` on its own as is called within `epichains_tree()`
#' after the arguments have been checked. To create an `<epichains_tree>`
#' object, use `epichains_tree()`.
#' @param tree_df a `<data.frame>` containing at least columns for
#' "infectee_id", "infector_id", and "generation". Also has optional columns
#' for "time", and "chain_id".
#' @param ntrees Number of initial cases used to generate the outbreak; Integer
#' @param track_pop Was the susceptible population tracked? Logical
#' @inheritParams epichains_tree
#' @author James M. Azam
#' @keywords internal
new_epichains_tree <- function(tree_df,
                               ntrees = integer(),
                               statistic = character(),
                               stat_max = integer(),
                               track_pop = logical()) {
  # Assemble the elements of the object
  obj <- structure(
    tree_df,
    ntrees = ntrees,
    statistic = statistic,
    stat_max = stat_max,
    track_pop = track_pop,
    class = c("epichains_tree", class(tree_df))
  )
  return(obj)
}

#' Create an `<epichains_tree>` object
#'
#' @description
#' `epichains_tree()` constructs an `<epichains_tree>` object, which is
#' inherently an `<data.frame>` object that stores some of the inputs
#' passed to the `simulate_tree()` and `simulate_tree_from_pop()` and the
#' simulated output. The stored attributes are useful for downstream
#' analyses and reproducibility. This function checks the validity of the
#' object created to ensure it has the right columns and column types.
#'
#' An `<epichains_tree>` object contains a `<data.frame>` of the simulated
#' outbreak tree with ids for each infector and infectee, generation, and
#' optionally, time, the number of initial cases used for the simulation,
#' the statistic that was tracked, and whether the susceptible population was
#' tracked.
#'
#' @inheritParams simulate_tree
#' @inheritParams new_epichains_tree
#'
#' @return An `<epichains_tree>` object
#' @author James M. Azam
#' @export
epichains_tree <- function(tree_df,
                           ntrees = integer(),
                           statistic = character(),
                           stat_max = integer(),
                           track_pop = logical()) {
  # Check that inputs are well specified
  checkmate::assert_data_frame(tree_df)
  checkmate::assert_integerish(ntrees, null.ok = TRUE)
  checkmate::assert_character(statistic, null.ok = TRUE)
  checkmate::assert_logical(track_pop)
  checkmate::assert_number(stat_max, null.ok = TRUE)

  # Create <epichains_tree> object
  epichains_tree <- new_epichains_tree(
    tree_df = tree_df,
    ntrees = ntrees,
    statistic = statistic,
    stat_max = stat_max,
    track_pop = track_pop
  )

  # Validate the created object
  validate_epichains_tree(epichains_tree)

  return(epichains_tree)
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
#' @param chains_summary a `<vector>` of chain sizes and lengths.
#' @inheritParams new_epichains_tree
#' @inheritParams simulate_tree
#' @author James M. Azam
#' @keywords internal
new_epichains_summary <- function(chains_summary,
                                  ntrees = integer(),
                                  statistic = character(),
                                  stat_max = integer()) {
  # Assemble the elements of the object
  obj <- structure(
    chains_summary,
    ntrees = ntrees,
    statistic = statistic,
    stat_max = stat_max,
    class = c("epichains_summary", class(chains_summary))
  )
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
#' @inheritParams new_epichains_summary
#'
#' @return An `<epichains_summary>` object
#' @author James M. Azam
#' @export
epichains_summary <- function(chains_summary,
                              ntrees = integer(),
                              statistic = character(),
                              stat_max = integer()) {
  # Check that inputs are well specified
  checkmate::assert_vector(chains_summary)
  checkmate::assert_integerish(ntrees, null.ok = TRUE)
  checkmate::assert_character(statistic)
  checkmate::assert_number(stat_max, null.ok = TRUE)

  # Create <epichains_summary> object
  epichains_summary <- new_epichains_summary(
    chains_summary,
    ntrees = ntrees,
    statistic = statistic,
    stat_max = stat_max
  )

  # Validate the created object
  validate_epichains_summary(epichains_summary)

  return(epichains_summary)
}

#' Print an `<epichains_tree>` object
#'
#' @param x An `<epichains_tree>` object.
#' @param ... Other parameters passed to `print()`.
#' @return Invisibly returns an `<epichains_tree>`. Called for
#' side-effects.
#' @author James M. Azam
#' @export
print.epichains_tree <- function(x, ...) {
  format(x, ...)
}

#' Print an `<epichains_summary>` object
#'
#' @param x An `<epichains_summary>` object.
#' @param ... Other parameters passed to `print()`.
#' @return Invisibly returns an `<epichains_summary>`. Called for
#' side-effects.
#' @author James M. Azam
#' @export
print.epichains_summary <- function(x, ...) {
  format(x, ...)
}

#' Format method for `<epichains_tree>` class
#'
#' @param x An `<epichains_tree>` object
#' @param ... further arguments passed to or from other methods
#' @return Invisibly returns an `<epichains_tree>`.
#' Called for printing side-effects.
#' @author James M. Azam
#' @export
format.epichains_tree <- function(x, ...) {
  # check that x is an <epichains_tree> object
  validate_epichains_tree(x)

  # summarise the information stored in x
  chain_info <- summary(x)

  writeLines(sprintf("`<epichains_tree>` object\n"))

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
        tree_info[["ntrees"]]
      ),
      sprintf(
        "Number of infectors (known): %s",
        tree_info[["unique_infectors"]]
      ),
      sprintf(
        "Number of generations: %s",
        chain_info[["max_generation"]]
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
#' @param x An `<epichains_summary>` object
#' @param ... further arguments passed to or from other methods
#' @return Invisibly returns an `<epichains_summary>`. Called for printing
#' side-effects.
#' @author James M. Azam
#' @export
format.epichains_summary <- function(x, ...) {
  # check that x is an <epichains_summary> object
  validate_epichains_summary(x)

  # summarise the information stored in x
  chain_info <- summary(x)

  writeLines(sprintf("`epichains_summary` object \n"))
  print(as.vector(x))
  writeLines(
    sprintf(
      "\n Number of chains simulated: %s",
      chain_info[["unique_chains"]]
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
        chain_info[["max_chain_stat"]]
      ),
      sprintf(
        "Min: %s",
        chain_info[["min_chain_stat"]]
      )
    )
  )

  invisible(x)
}

#' Summary method for `epichains_tree` class
#'
#' @param object An `<epichains_tree>` object
#' @param ... further arguments passed to or from other methods
#'
#' @return List of summaries
#' @author James M. Azam
#' @export
summary.epichains_tree <- function(object, ...) {
  # Check that object has <epichains_tree> class
  validate_epichains_tree(object)

  # Get the summaries
  ntrees <- attr(object, "ntrees", exact = TRUE)

  max_time <- ifelse(("time" %in% names(object)), max(object$time), NA)

  n_unique_infectors <- length(
    unique(object$infector_id[!is.na(object$infector_id)])
  )

  max_generation <- max(object$generation)

  # List of summaries
  out <- list(
    ntrees = ntrees,
    max_time = max_time,
    unique_infectors = n_unique_infectors,
    max_generation = max_generation
  )

  return(out)
}

#' Summary method for `<epichains_summary>` class
#'
#' @param object An `<epichains_summary>` object
#' @param ... further arguments passed to or from other methods
#'
#' @return List of summaries
#' @author James M. Azam
#' @export
summary.epichains_summary <- function(object, ...) {
  # Check that object has <epichains_summary> class
  validate_epichains_summary(object)

  # Get the summaries
  ntrees <- attr(object, "ntrees", exact = TRUE)


  if (all(is.infinite(object))) {
    max_chain_stat <- min_chain_stat <- Inf
  } else {
    max_chain_stat <- max(object[!is.infinite(object)])
    min_chain_stat <- min(object[!is.infinite(object)])
  }

  out <- list(
    ntrees = ntrees,
    max_chain_stat = max_chain_stat,
    min_chain_stat = min_chain_stat
  )

  return(out)
}

#' Test if x is an `epichains_tree` object
#'
#' @param x An R object
#'
#' @return logical, `TRUE` if the object is an `<epichains_tree>` and `FALSE`
#' otherwise
#' @author James M. Azam
#' @export
is_epichains_tree <- function(x) {
  inherits(x, "epichains_tree")
}

#' Test if x is an `epichains_summary` object
#'
#' @param x An R object
#'
#' @return logical, `TRUE` if the object is an `epichains_summary` and `FALSE`
#' otherwise
#' @author James M. Azam
#' @export
is_epichains_summary <- function(x) {
  inherits(x, "epichains_summary")
}

#' Validate an `<epichains_tree>` object
#'
#' @param x An `<epichains_tree>` object
#'
#' @return No return.
#' @author James M. Azam
#' @export
validate_epichains_tree <- function(x) {
  if (!is_epichains_tree(x)) {
    stop("Object must have an `<epichains_tree>` class")
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
#' @param x An `<epichains_summary>` object
#'
#' @return No return.
#' @author James M. Azam
#' @export
validate_epichains_summary <- function(x) {
  if (!is_epichains_summary(x)) {
    stop("Object must have an `<epichains_summary>` class")
  }

  invisible(x)
}

#' `head` and `tail` method for `<epichains_tree>` class
#'
#' @param x An `<epichains_tree>` object
#' @param ... further arguments passed to or from other methods
#' @importFrom utils head
#' @importFrom utils tail
#' @return Object of class `data.frame`
#' @author James M. Azam
#' @export
#' @details
#' This returns the top rows of an `<epichains_tree>` object. Note that
#' the object is originally sorted by `sim_id` and `infector_id` and the first
#' unknown infectors (NA) have been dropped from
#' printing method.
#'
#' To view the full output, use `as.data.frame(<object_name>)`.
head.epichains_tree <- function(x, ...) {
  # print head of the simulation output from the first known infector_id
  x <- x[!is.na(x$infector_id), ]
  utils::head(as.data.frame(x), ...)
}

#' @rdname head.epichains_tree
#' @export
tail.epichains_tree <- function(x, ...) {
  utils::tail(as.data.frame(x), ...)
}

#' Aggregate cases in `<epichains_tree>` objects by "generation" or "time", if
#' present
#'
#' @description
#' This function provides a quick way to create a time series of cases over
#' generation or time (if serials_dist was specified) from simulated
#' `<epichains_tree>` objects.
#'
#' @param x An `<epichains_tree>` object.
#' @param by The variable to aggregate by. Options include
#' "time" and "generation".
#' @param ... Other arguments passed to aggregate.
#' @importFrom stats aggregate
#' @return A `<data.frame>` object of cases by `by`.
#' @author James M. Azam
#' @export
#' @examples
#' set.seed(123)
#' chains <- simulate_tree(
#'   ntrees = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
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
aggregate.epichains_tree <- function(x,
                                     by = c(
                                       "time",
                                       "generation"
                                     ),
                                     ...) {
  validate_epichains_tree(x)

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
