#' Print an [`epichains`] object
#'
#' @param x An [`epichains`] object.
#' @param ... Other parameters passed to [print()].
#' @return Invisibly returns an [`epichains`]. Called for side-effects.
#' @author James M. Azam
#' @export
print.epichains <- function(x, ...) {
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
        sprintf("Chains simulated: %s", chain_info[["chains_ran"]]),
        sprintf(
          "Number of ancestors (known): %s",
          chain_info[["unique_ancestors"]]
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

  chains_ran <- attr(object, "chains", exact = TRUE)

  if (is_chains_tree(object)) {
    max_time <- ifelse(("time" %in% names(object)), max(object$time), NA)

    n_unique_ancestors <- length(
      unique(object$ancestor[!is.na(object$ancestor)])
    )

    max_generation <- max(object$generation)

    # out of summary
    res <- list(
      chains_ran = chains_ran,
      max_time = max_time,
      unique_ancestors = n_unique_ancestors,
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
      chain_ran = chains_ran,
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
        c("sim_id", "ancestor", "generation") %in%
        colnames(x),
      "column `sim_id` must be a numeric" =
        is.numeric(x$sim_id),
      "column `ancestor` must be a numeric" =
        is.numeric(x$ancestor),
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


#' `head` method for [`epichains`] class
#'
#' @param x An [`epichains`] object
#' @param ... further arguments passed to or from other methods
#' @importFrom utils head
#' @return object of class `data.frame`
#' @author James M. Azam
#' @export
#' @details
#' This returns the top rows of an `epichains` object. Note that the object
#' is originally sorted by `sim_id` and `ancestor` and the first
#' unknown ancestors (NA) have been dropped from
#' printing method. To view the full output, use `as.data.frame(<object_name>)`.
#'
head.epichains <- function(x, ...) {
  writeLines("< tree head (from first known ancestor) >\n")
  # print head of the simulation output from the first known ancestor
  x <- x[!is.na(x$ancestor), ]
  utils::head(as.data.frame(x), ...)
}

#' `tail` method for [`epichains`] class
#'
#' @param x An [`epichains`] object
#' @param ... further arguments passed to or from other methods
#' @importFrom utils tail
#' @author James M. Azam
#' @export
#' @details
#' This returns the top rows of an `epichains` object. Note that the object
#' is originally sorted by `sim_id` and `ancestor` and the first
#' unknown ancestors (NA) have been dropped from
#' printing method. To view the full output, use `as.data.frame(<object_name>)`.
tail.epichains <- function(x, ...) {
  writeLines("\n< tree tail >\n")
  utils::tail(as.data.frame(x), ...)
}

#' Aggregate cases in `<epichains>` objects by "time" or "generation"
#'
#' @param x An `<epichains>` object.
#' @param grouping_var The variable to group and count over. Options include
#' "time" and "generation".
#' @param ... Other arguments passed to aggregate.
#' @importFrom stats aggregate
#' @return An `<epichains_aggregate_df>` object, which is basically a
#' `<data.frame>`. The object stores the `chain_type = chains_tree` and
#' `grouping_var` attributes.
#' @export
#' @author James M. Azam
#' @examples
#' set.seed(123)
#' chains <- simulate_tree(
#'   nchains = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
#'   stat_max = 10,
#'   serials_dist = function(x) 3,
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
  validate_epichains(x)
  # Check that the object is of type "chains_tree"
  if (!is_chains_tree(x)) {
    stop(
      "object must be an epichains object with 'chains_tree' attribute, ",
      "which can be generated using the `simulate_tree()` function."
    )
  }

  # Get grouping variable
  grouping_var <- match.arg(grouping_var)

  out <- if (grouping_var == "time") {
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

  structure(
    out,
    class = c("epichains_aggregate_df", class(out)),
    chain_type = attributes(x)$chain_type,
    rownames = NULL,
    aggregated_over = grouping_var
  )
}
