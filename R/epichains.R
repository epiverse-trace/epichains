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
#'
#' @examples
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
#' @param object epichains object
#' @param ... further arguments passed to or from other methods
#'
#' @return data frame of information
#' @export
#'
#' @examples
summary.epichains <- function(x, ...) {
  validate_epichains(x)

  if (attributes(x)$chain_type == "chains_tree") {

    chains_ran <- length(x$n)

    max_time <- max(x$time)

    n_unique_ancestors <- length(
      unique(x$ancestor[!is.na(x$ancestor)])
    )

    num_generations <- length(unique(x$generation))

    max_generation <- max(x$generation)

    # out of summary
    res <- list(
      unique_chains = chains_ran,
      max_time = max_time,
      unique_ancestors = n_unique_ancestors,
      unique_generations = n_unique_ancestors,
      num_generations = num_generations,
      max_generation = max_generation
    )
  } else if (attributes(x)$chain_type == "chains_vec") {
    chains_ran <- length(x)
    max_chain_stat <- max(!is.infinite(x))
    min_chain_stat <- min(!is.infinite(x))

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
#' @export
#'
#' @examples
is_epichains <- function(x) {
  inherits(x, "epichains")
}

#' `epichains` class validator
#'
#' @param x An `epichains` object
#'
#' @return Checks if an object is of class `epichains` and if so
#' checks that it's in the right format as a "data.frame" or vector.
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

#' `head` and `tail` methods for [`epichains`] class
#'
#' @param x An [`epichains`] object
#' @param ... further arguments passed to or from other methods
#'
#' @return object of class `data.frame`
#' @export
#'
#' @importFrom utils head
#' @importFrom utils tail
head.epichains <- function(x, ...) {
  utils::head(as.data.frame(x), ...)
}

#' @rdname head.epichains
#' @export
tail.epichains <- function(x, ...) {
  utils::tail(as.data.frame(x), ...)
}

#' Plot epichains tree objects
#'
#' @param x An [`epichains`] object with a chains_tree attribute
#' @param ... Other arguments passed to plot
#'
#' @return A plot of cases over time and generation
#' @author James M. Azam
#' @export
plot.epichains <- function(x, ...){
  validate_epichains(x)

  if (attributes(x)$chain_type != "chains_tree") {
    stop("Object must be an epichains object with a chains_tree attribute.")
  }

  cases_per_generation <- aggregate(sim_id ~ generation, x = as.data.frame(x), FUN = NROW)

  cases_per_time <- aggregate(sim_id ~ time, x = as.data.frame(x), FUN = NROW)
  cases_per_time <- stats::aggregate(sim_id ~ time, x = as.data.frame(x), FUN = NROW)

  graphics::par(mfrow = c(1, 2), mar = c(4, 3, 3, 1), oma = c(0, 0, 0, 0))

  # Make first plot
  graphics::plot(cases_per_generation$generation,
       cases_per_generation$sim_id,
       xlab = "Generation",
       ylab = "Cases",
       type = "b",
       main = "Number of cases per generation"
       )

  # Make second plot
  graphics::plot(cases_per_time$time,
       cases_per_time$sim_id,
       xlab = "Time",
       ylab = "Cases",
       type = "b",
       main = "Number of cases per time"
  )
}
