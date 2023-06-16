#' Print an [`epichains`] object
#'
#' @param x An [`epichains`] object.
#' @param ... Other parameters passed to [print()].
#' @return Invisibly returns an [`epichains`]. Called for side-effects.
#' @export
print.epichains <- function(x, ...) {
  format(x, ...)
}

#' Format method for epichains class
#'
#' @param x epichains object
#' @param ... further arguments passed to or from other methods
#' @return Invisibly returns an [`epichains`]. Called for printing side-effects.
#' @export
format.epichains <- function(x, ...) {
  # check that x is an epichains object
  validate_epichains(x)

  # summarise the information stored in x
  chain_info <- summary(x)

  if (attributes(x)$chain_type == "chains_tree") {
    writeLines(
      c(
        sprintf("`epichains` object"),

        "< tree head (from first known ancestor) >\n"
        )
      )

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
    writeLines(sprintf("Use View(<object_name>) to view the full output."))

  } else if (attributes(x)$chain_type == "chains_vec") {
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
#' @export
summary.epichains <- function(object, ...) {
  validate_epichains(object)

  if (attributes(object)$chain_type == "chains_tree") {

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
  } else if (attributes(object)$chain_type == "chains_vec") {
    chains_ran <- length(object)
    max_chain_stat <- max(!is.infinite(object))
    min_chain_stat <- min(!is.infinite(object))

    res <- list(
      unique_chains = chains_ran,
      max_chain_stat = max_chain_stat,
      min_chain_stat = min_chain_stat
    )
  }

  return(res)
}

#' Checks whether the object is an `epichains`
#'
#' @param x An R object
#'
#' @return logical, `TRUE` if the object is an `epichains` and `FALSE`
#' otherwise
#' @keywords internal
is_epichains <- function(x) {
  inherits(x, "epichains")
}

#' Check if an object is of class "epichains_aggregate_df"
#'
#' @param x An [`epichains`] object
#'
#' @keywords internal
is_epichains_aggregate_df <- function(x) {
  if (!inherits(x, "epichains_aggregate_df")) {
    stop("Object must have class 'epichains_aggregate_df'")
  }
}

#' `epichains` class validator
#'
#' @param x An `epichains` object
#'
#' @return Checks if an object is of class `epichains` and if so
#' checks that it's in the right format as a "data.frame" or vector.
#' @keywords internal
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

#' Plot epichains tree objects
#'
#' This method accepts epichains aggregated through the `aggregate` method,
#' which returns an object of class `epichains_aggregate_df` with an
#' `aggregated_over` attribute that tells `plot()` which variable to plot.
#'
#' @param x An `epichains_aggregate_df` object with a `chains_tree` attribute.
#' @param ... Other arguments passed to plot.
#' @importFrom graphics barplot par
#' @return A plot of cases over time, generation, or both, depending on which
#' of the options in the simulated dataset was aggregated over. See
#' \code{?epichains::aggregate}.
#' @author James M. Azam
#' @examples
#' # Generate chains with poisson offspring using simulate_tree()
#' set.seed(123)
#' chains <- simulate_tree(nchains = 10,
#' serials_sampler = function(x) rpois(x, 2),
#' offspring_sampler = "pois", lambda = 2, chain_stat_max = 10)
#'
#' # Aggregate cases per time and plot the results
#' cases_per_time <- aggregate(chains, "time")
#' plot(cases_per_time)
#' # Aggregate cases per generation and plot the results
#' cases_per_gen <- aggregate(chains, "generation")
#' plot(cases_per_gen)
#'
#' # Aggregate cases per time and generation and plot the results
#' cases_aggreg <- aggregate(chains, "both")
#' plot(cases_aggreg)
#'
#' # Generate chains with negative
#' # binomial offspring and from a fixed population size using
#' # simulate_tree_from_pop()
#' set.seed(123)
#' chains_bn <- simulate_tree_from_pop(pop = 1000, offspring_sampler = "nbinom",
#' mean_offspring = 0.5, disp_offspring = 1.1,
#' serial_sampler = function(x) rpois(x, 2))
#'
#' # Plot them
#' plot(aggregate(chains_bn, "time"))
#' @export
plot.epichains <- function(x, ...) {

  # Object should have been aggregated using the aggregate.epichains method
  is_epichains_aggregate_df(x)

  check_chain_tree_attribute(x)

  plotting_var <- attributes(x)$aggregated_over

  if (plotting_var == "time") {
    graphics::barplot(x$cases,
      names.arg = x$time,
      xlab = "Time",
      ylab = "Cases",
      type = "b", ,
      col = "tomato3",
      main = "Number of cases per time"
    )
  } else if (plotting_var == "generation") {
    graphics::barplot(x$cases,
      names.arg = x$generation,
      xlab = "Generation",
      ylab = "Cases", ,
      col = "steelblue",
      main = "Number of cases per generation"
    )
  } else if (plotting_var == "both") {
    par(mfrow = c(1, 2))
    # Make first plot
    graphics::barplot(x[[1]]$cases,
      names.arg = x$time,
      xlab = "Time",
      ylab = "Cases",
      type = "b", ,
      col = "tomato3",
      main = "Number of cases per time"
    )
    # Make second plot
    graphics::barplot(x[[2]]$cases,
      names.arg = x$generation,
      xlab = "Generation",
      ylab = "Cases", ,
      col = "steelblue",
      main = "Number of cases per generation"
    )
  }
}

#' Aggregate cases in epichains objects according to a grouping variable
#'
#' @param x An [`epichains`] object.
#' @param grouping_var The variable to group and count over. Options include
#' "time", "generation", and "both".
#' @param ... Other arguments passed to aggregate.
#'
#' @return If grouping_var is either "time" or "generation", a data.frame
#' with cases aggregated over `grouping_var`; If
#' \code{grouping_var = "both"}, a list of data.frames, the first being for
#'  cases over time, and the second being for cases over generations.
#' @export
#'
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
  if (attributes(x)$chain_type != "chains_tree") {
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
