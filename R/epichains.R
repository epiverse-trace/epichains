#' Construct a `<epichains_tree>` object
#'
#' @description
#' `new_epichains_tree()` constructs an `<epichains_tree>` object from a
#' supplied `<data.frame>` and extra attributes passed as individual arguments.
#' It is meant to be lazy and performant, by creating the object without
#' checking the arguments for correctness. It is not safe to call
#' `new_epichains_tree()` on its own as is called within `epichains_tree()`
#' after the arguments have been checked. To create an `<epichains_tree>`
#' object, use `epichains_tree()`.
#' @param tree_df a `<data.frame>` containing at least columns for "chain_id",
#' "ancestor", and "generation". Also has optional columns for "time", and
#' "chain_id".
#' @param chains_run Number of chains/cases used to generate the outbreak;
#' Integer
#' @param track_pop Was the susceptible population tracked; Logical
#' @inheritParams epichains_tree
#' @author James M. Azam
#' @keywords internal
new_epichains_tree <- function(tree_df = data.frame(),
                               chains_run = integer(),
                               statistic = character(),
                               stat_max = double(),
                               intvn_mean_reduction = double(),
                               track_pop = logical()
                               ) {
  # Assemble the elements of the object
  obj <- structure(
    tree_df,
    chains_run = chains_run,
    statistic = statistic,
    stat_max = stat_max,
    intvn_mean_reduction = intvn_mean_reduction,
    track_pop = track_pop,
    class = c("epichains_tree", "data.frame")
  )
  return(obj)
}

#' Create an `<epichains_tree>` object
#'
#' @description
#' `epichains_tree()` constructs an `<epichains_tree>` object, which is
#' inherently an `<data.frame>` object that stores some of the inputs
#' passed to the `simulate_tree()` and `simulate_tree_from_pop()` and the
#' simulated output. The stored attributes are useful for scenario
#' analyses where the inputs are required for downstream analyses.
#'
#' An `<epichains_tree>` object contains a `<data.frame>` of the simulated
#' outbreak with ids for each case/chain and the chain the produced, the
#' number of cases/chains used for the simulation, the statistic that was
#' tracked, the intervention level, and whether the susceptible population was
#' tracked.
#'
#' @inheritParams simulate_tree
#' @inheritParams new_epichains_tree
#'
#' @return An `<epichains_tree>` object
#' @author James M. Azam
#' @export
epichains_tree <- function(tree_df = data.frame(),
                           chains_run = integer(),
                           statistic = character(),
                           stat_max = double(),
                           intvn_mean_reduction = double(),
                           track_pop = logical()
                           ) {
  # Check that inputs are well specified
  checkmate::assert_data_frame(tree_df)
  checkmate::assert_integerish(chains_run, null.ok = TRUE)
  checkmate::assert_character(statistic, null.ok = TRUE)
  checkmate::assert_integerish(stat_max, null.ok = TRUE)
  checkmate::assert_double(intvn_mean_reduction)
  checkmate::assert_logical(track_pop)

  # Create <epichains_tree> object
  epichains_tree <- new_epichains_tree(
    tree_df = tree_df,
    chains_run = chains_run,
    statistic = statistic,
    stat_max = stat_max,
    intvn_mean_reduction = intvn_mean_reduction,
    track_pop = track_pop
    )

  # Validate the created object
  validate_epichains_tree(epichains_tree)

  return(epichains_tree)
}
  format(x, ...)
}

#' Format method for epichains class
#'
#' @param x epichains object
#' @param ... further arguments passed to or from other methods
#' @return Invisibly returns an [`epichains`]. Called for printing side-effects.
#' @author James M. Azam
#' @export
format.epichains <- function(x, ...) {
  # check that x is an epichains object
  validate_epichains(x)

  # summarise the information stored in x
  chain_info <- summary(x)

  if (is_chains_tree(x)) {
    writeLines(sprintf("`epichains` object\n"))
    # print head of the object
    print(head(x))
    # print tail of object
    print(tail(x))

    # print summary information
    writeLines(
      c(
        sprintf("Chains simulated: %s", chain_info[["chains_run"]]),
        sprintf(
          "Number of infectors (known): %s",
          chain_info[["unique_infectors"]]
        ),
        sprintf(
          "Number of generations: %s", chain_info[["max_generation"]]
        )
      )
    )

    # Offer more information to view the full dataset
    writeLines(sprintf(
      "%s %s", "Use `as.data.frame(<object_name>)`",
      "to view the full output in the console."
    ))
  } else if (is_chains_summary(x)) {
    writeLines(sprintf("`epichains` object \n"))
    print(as.vector(x))
    writeLines(sprintf(
      "\n Number of chains simulated: %s",
      chain_info[["unique_chains"]]
    ))
    writeLines(
      c(
        sprintf(
          "\n Simulated chain %ss: \n",
          attr(x, "statistic", exact = TRUE)
        ),
        sprintf("Max: %s", chain_info[["max_chain_stat"]]),
        sprintf("Min: %s", chain_info[["min_chain_stat"]])
      )
    )
  }

  invisible(x)
}



#' Summary method for epichains class
#'
#' @param object An [`epichains`] object
#' @param ... further arguments passed to or from other methods
#'
#' @return data frame of information
#' @author James M. Azam
#' @export
summary.epichains <- function(object, ...) {
  validate_epichains(object)

  chains_run <- attr(object, "chains", exact = TRUE)

  if (is_chains_tree(object)) {
    max_time <- ifelse(("time" %in% names(object)), max(object$time), NA)

    n_unique_infectors <- length(
      unique(object$infector_id[!is.na(object$infector_id)])
    )

    max_generation <- max(object$generation)

    # out of summary
    res <- list(
      chains_run = chains_run,
      max_time = max_time,
      unique_infectors = n_unique_infectors,
      max_generation = max_generation
    )
  } else if (is_chains_summary(object)) {
    if (all(is.infinite(object))) {
      max_chain_stat <- min_chain_stat <- Inf
    } else {
      max_chain_stat <- max(object[!is.infinite(object)])
      min_chain_stat <- min(object[!is.infinite(object)])
    }

    res <- list(
      chains_run = chains_run,
      max_chain_stat = max_chain_stat,
      min_chain_stat = min_chain_stat
    )
  }

  return(res)
}

#' Reports whether x is an `epichains` object
#'
#' @param x An R object
#'
#' @return logical, `TRUE` if the object is an `epichains` and `FALSE`
#' otherwise
#' @export
#' @author James M. Azam
is_epichains <- function(x) {
  inherits(x, "epichains")
}

#' Reports whether x is an "epichains_aggregate_df" object
#'
#' @param x An [`epichains`] object
#' @return logical, `TRUE` if the object is an `epichains_aggregate_df` and
#' `FALSE` otherwise
#' @export
#' @author James M. Azam
is_epichains_aggregate_df <- function(x) {
  inherits(x, "epichains_aggregate_df")
}

#' `epichains` class validator
#'
#' @param x An `epichains` object
#'
#' @return No return.
#' @export
#' @author James M. Azam
validate_epichains <- function(x) {
  if (!is_epichains(x)) {
    stop("Object must have an epichains class")
  }

  # check for class invariants

  if (is_chains_tree(x)) {
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
  } else {
    stopifnot(
      "object must be a numeric vector" =
        is.numeric(x)
    )
  }

  invisible(x)
}

#' Check if an epichains object has the `chains_tree` attribute
#'
#' @param x An [`epichains`] object
#'
#' @export
#' @author James M. Azam
is_chains_tree <- function(x) {
  !is.null(attributes(x)$chain_type) &&
    attributes(x)$chain_type == "chains_tree"
}

#' Check if an epichains object has the `chains_summary` attribute
#'
#' @param x An [`epichains`] object
#'
#' @export
#' @author James M. Azam
is_chains_summary <- function(x) {
  !is.null(attributes(x)$chain_type) &&
    attributes(x)$chain_type == "chains_summary"
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
#' the object is originally sorted by `sim_id` and `ancestor` and the first
#' unknown ancestors (NA) have been dropped from
#' printing method.
#'
#' To view the full output, use `as.data.frame(<object_name>)`.
head.epichains_tree <- function(x, ...) {
  # print head of the simulation output from the first known ancestor
  x <- x[!is.na(x$ancestor), ]
  utils::head(as.data.frame(x), ...)
}

#' @rdname head.epichains_tree
#' @export
tail.epichains_tree <- function(x, ...) {
  utils::tail(as.data.frame(x), ...)
}

#' Aggregate cases in `<epichains_tree>` objects by "time" or "generation"
#'
#' @description
#' This function provides a quick way to create a time series of cases over
#' time or generation from simulated `<epichains_tree>` objects.
#'
#' @param x An `<epichains_tree>` object.
#' @param grouping_var The variable to aggregate by. Options include
#' "time" and "generation".
#' @param ... Other arguments passed to aggregate.
#' @importFrom stats aggregate
#' @return A `<data.frame>` object of cases by `grouping_var`.
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
#' cases_per_time <- aggregate(chains, grouping_var = "time")
#' head(cases_per_time)
#'
#' # Aggregate cases per generation
#' cases_per_gen <- aggregate(chains, grouping_var = "generation")
#' head(cases_per_gen)
aggregate.epichains <- function(x,
                                grouping_var = c(
                                  "time",
                                  "generation"
                                ),
                                ...) {
  validate_epichains_tree(x)

  # Get grouping variable
  grouping_var <- match.arg(grouping_var)

  out <- if (grouping_var == "time") {
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
  } else if (grouping_var == "generation") {
    # Count the number of cases per time
    stats::aggregate(
      list(cases = x$sim_id),
      list(generation = x$generation),
      FUN = NROW
    )
  }

  return(out)
}
