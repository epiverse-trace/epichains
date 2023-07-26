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
    writeLines(
      c(
        sprintf("`epichains` object"),

        "< tree head (from first known ancestor) >\n"
        )
      )

    #sort by ancestor first

    x <- x[order(x$sim_id, x$ancestor), ]

    # print head of the simulation output
    print(head(x[!is.na(x$ancestor), ]))

    cat("< tree tail >\n")

    # print tail of object
    print(tail(as.data.frame(x)))

    # print summary information
    writeLines(
      c(
        sprintf("Chains simulated: %s", chain_info[["chains"]]),
        sprintf(
          "Unique number of ancestors: %s",
          chain_info[["unique_ancestors"]]
        ),
        sprintf(
          "Unique number of generations: %s", chain_info[["unique_generations"]]
        )
      )
    )

    # Offer more information to view the full dataset
    writeLines(sprintf("%s %s", "Use `as.data.frame(<object_name>)`",
                       "to view the full output in the console.")
               )

  } else if (is_chains_vec(x)) {
    cat(sprintf("epichains object \n"))
    print(as.vector(x))
    cat(sprintf("Number of chains simulated: %s",
                chain_info[["unique_chains"]]
                )
        )
    writeLines(
      c(
        "\n Simulated chain stats: \n",
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

  if (is_chains_tree(object)) {

    chains_ran <- length(object$n)

    max_time <- max(object$time)

    n_unique_ancestors <- length(
      unique(object$ancestor[!is.na(object$ancestor)])
    )

    num_generations <- length(unique(object$generation))

    max_generation <- max(object$generation)

    # out of summary
    res <- list(
      unique_chains = chains_ran,
      max_time = max_time,
      unique_ancestors = n_unique_ancestors,
      unique_generations = n_unique_ancestors,
      num_generations = num_generations,
      max_generation = max_generation
    )
  } else if (is_chains_vec(object)) {
    chains_ran <- length(object)

    if(!all(is.infinite(object))){
    max_chain_stat <- max(object[!is.infinite(object)])
    min_chain_stat <- min(object[!is.infinite(object)])
    }else{
    max_chain_stat <- min_chain_stat <- Inf
    }

    res <- list(
      unique_chains = chains_ran,
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

  if (attributes(x)$chain_type == "chains_tree") {
    stopifnot(
      "object does not contain the correct columns" =
        c("sim_id", "ancestor", "generation", "time") %in%
          colnames(x),
      "column `sim_id` must be a numeric" =
        is.numeric(x$sim_id),
      "column `ancestor` must be a numeric" =
        is.numeric(x$ancestor),
      "column `generation` must be a numeric" =
        is.numeric(x$generation),
      "column `time` must be a numeric" =
        is.numeric(x$time)
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

#' Check if an epichains object has the `chains_vec` attribute
#'
#' @param x An [`epichains`] object
#'
#' @export
#' @author James M. Azam
is_chains_vec <- function(x) {
  !is.null(attributes(x)$chain_type) &&
    attributes(x)$chain_type == "chains_vec"
}


#' `head` method for [`epichains`] class
#'
#' @param x An [`epichains`] object
#' @param ... further arguments passed to or from other methods
#' @importFrom utils head
#' @return object of class `data.frame`
#' @author James M. Azam
#' @export
head.epichains <- function(x, ...) {
  utils::head(as.data.frame(x), ...)
}

#' `tail` method for [`epichains`] class
#' @param x An [`epichains`] object
#' @param ... further arguments passed to or from other methods
#' @importFrom utils tail
#' @author James M. Azam
#' @export
tail.epichains <- function(x, ...) {
  utils::tail(as.data.frame(x), ...)
}

#' Aggregate cases in epichains objects according to a grouping variable
#'
#' @param x An [`epichains`] object.
#' @param grouping_var The variable to group and count over. Options include
#' "time", "generation", and "both".
#' @param ... Other arguments passed to aggregate.
#' @importFrom stats aggregate
#' @return If grouping_var is either "time" or "generation", a data.frame
#' with cases aggregated over `grouping_var`; If
#' \code{grouping_var = "both"}, a list of data.frames, the first being for
#'  cases over time, and the second being for cases over generations.
#' @export
#' @author James M. Azam
#' @examples
#' set.seed(123)
#' chains <- simulate_tree(nchains = 10, serials_sampler = function(x) 3,
#' offspring_sampler = "pois", lambda = 2, chain_stat_max = 10)
#' chains
#'
#' # Aggregate cases per time
#' aggregate(chains, grouping_var = "time")
#'
#' # Aggregate cases per generation
#' aggregate(chains, grouping_var = "generation")
#'
#' # Aggregate cases per both time and generation
#' aggregate(chains, grouping_var = "both")
aggregate.epichains <- function(x,
                                grouping_var = c("time",
                                                 "generation",
                                                 "both"
                                                 ),
                                ...) {
  validate_epichains(x)
  # Check that the object is of type "chains_tree"
  if (!is_chains_tree(x)) {
    stop("object must be an epichains object with 'chains_tree' attribute.")
  }

  # Get grouping variable
  grouping_var <- match.arg(grouping_var)

  out <- if (grouping_var == "time") {
    # Count the number of cases per generation
    stats::aggregate(list(cases = x$sim_id),
      list(time = x$time),
      FUN = NROW
    )
  } else if (grouping_var == "generation") {
    # Count the number of cases per time
    stats::aggregate(list(cases = x$sim_id),
      list(generation = x$generation),
      FUN = NROW
    )
  } else if (grouping_var == "both") {
    # Count the number of cases per time
    list(
      stats::aggregate(list(cases = x$sim_id),
                       list(time = x$time),
                       FUN = NROW),
      # Count the number of cases per generation
      stats::aggregate(list(cases = x$sim_id),
                       list(generation = x$generation),
                       FUN = NROW)
    )
  }

  structure(out,
    class = c("epichains_aggregate_df", class(out)),
    chain_type = attributes(x)$chain_type,
    rownames = NULL,
    aggregated_over = grouping_var
  )
}
