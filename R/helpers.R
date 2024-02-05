#' Determine and update the chain statistic being tracked
#'
#' @param stat_type Chain statistic (size/length) to update.
#' @param stat_latest The latest chain statistic vector to be updated.
#' @param n_offspring A vector of offspring per chain.
#' @return A vector of chain statistics (size/length).
#' @keywords internal
.update_chain_stat <- function(stat_type, stat_latest, n_offspring) {
  return(
    switch(
      stat_type,
      size = stat_latest + n_offspring,
      length = stat_latest + pmin(1, n_offspring),
      stop("stat_type must be 'size' or 'length'")
    )
  )
}

#' Return a function for calculating chain statistics
#'
#' @inheritParams simulate_chains
#'
#' @return a function for calculating chain statistics
#' @keywords internal
.get_statistic_func <- function(chain_statistic) {
  return(
    switch(
      chain_statistic,
      size = rbinom_size,
      length = rgen_length,
      stop("chain_statistic must be 'size' or 'length'")
    )
  )
}

#' Adjust next generation vector to match susceptible population size
#'
#' @description Calculates the initial susceptible population size given
#' the total population size, the percent immune, and the number of index
#' cases. This function is used internally, and input checking is not
#' performed here, only in the context where it is used. Using it directly
#' is not recommended.
#'
#' @inheritParams simulate_chains
#'
#' @return numeric; initial susceptible population size
#' @keywords internal
.init_susc_pop <- function(pop,
                           percent_immune,
                           index_cases) {
  ss <- max(round(pop * (1 - percent_immune)) - index_cases, 0)
  return(ss)
}
}
#'
#' @param next_gen numeric; vector of next generation offspring
#' @param susc_pop numeric; susceptible population size
#'
#' @return numeric; adjusted next generation offspring vector
#' @keywords internal
.adjust_next_gen <- function(next_gen, susc_pop) {
  ## create hypothetical next generation individuals to sample from
  next_gen_pop <- rep(
    seq_along(next_gen),
    times = next_gen
  )
  ## sample from hypothetical individuals so that total = susc_pop
  next_gen_sample <- sample(
    x = next_gen_pop,
    size = susc_pop
  )
  ## create adjusted next_gen vector
  next_gen <- rep(
    0L,
    length(next_gen)
  )
  ## count occurrences in next generation sample
  next_gen_count <- table(next_gen_sample)
  next_gen[as.integer(names(next_gen_count))] <- unname(next_gen_count)
  return(next_gen)
}
