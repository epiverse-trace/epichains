print.epichains <- function(x, ...) {
  format(x, ...)
}

#' Format method for epichains class
#'
#' @param x epichains object
#' @param ... further arguments passed to or from other methods
#' @importFrom tibble as_tibble
#' @return Invisibly returns an [`epichains`]. Called for printing side-effects.
#' @export
#'
#' @examples
format.epichains <- function(x, ...) {
  chain_info <- summary(x)
  if (attributes(x)$chain_type == "chains_tree") {
    cat("head starting from first known ancestor \n")
    print(tibble::as_tibble(head(subset(x, !is.na(ancestor)))))
    cat("--- \n")
    print(tail(tibble::as_tibble(x)))
    writeLines(
      c(
        sprintf("`epichains` `chains_tree` object"),
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
    writeLines(sprintf("Use View(<object_name>) to view the full output."))
    invisible(x)
  } else if (attributes(x)$chain_type == "chains_vec") {
    cat(sprintf("epichains object \n"))
    print(as.vector(x))
    cat(sprintf("Number of chains simulated: %s",
                chain_info[["unique_chains"]]
                )
        )
    writeLines(
      c(
        cat("\n Simulated chain stats: \n"),
        sprintf("Max: %s", chain_info[["max_chain_stat"]]),
        sprintf("Min: %s", chain_info[["min_chain_stat"]])
      )
    )
  }
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
  if (attributes(x)$chain_type == "chains_tree") {
    is_epichains(x)

    chains_ran <- length(x$n)

    max_time <- max(x$time)

    n_unique_ancestors <- length(
      unique(x$ancestor[!is.na(x$ancestor)])
    )

    num_generations <- length(unique(x$generations))

    # out of summary
    res <- list(
      unique_chains = chains_ran,
      max_time = max_time,
      unique_ancestors = n_unique_ancestors,
      unique_generations = n_unique_ancestors,
      num_generations = num_generations
      # WIP
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

  if (attributes(x)$is_tree) {
    stopifnot(
      "object does not contain the correct columns" =
        c("n", "id", "ancestor", "generation", "time") %in%
          colnames(x),
      "column `n` must be a numeric" =
        is.numeric(x$n),
      "column `id` must be a numeric" =
        is.numeric(x$id),
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
