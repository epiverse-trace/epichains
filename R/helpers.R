#' Determine and update the chain statistic being tracked
#'
#' @param stat_type Chain statistic (size/length) to update; A character string.
#' Must be one of 'size' or 'length'.
#' @param stat_latest The latest chain statistic numeric vector to be updated.
#' @param n_offspring A vector of offspring per chain. A numeric vector
#' (coercible to integer).
#' @return A vector of chain statistics (size/length). A numeric vector
#' coercible to integer.
#' @keywords internal
.update_chain_stat <- function(stat_type, stat_latest, n_offspring) {
  return(
    switch(stat_type,
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
#' @return A function for calculating chain statistics.
#' @keywords internal
.get_statistic_func <- function(chain_statistic) {
  return(
    switch(chain_statistic,
      size = .rbinom_size,
      length = .rgen_length,
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
#' @return Initial susceptible population size; A numeric coercible to integer.
#' @keywords internal
.init_susc_pop <- function(pop,
                           percent_immune,
                           index_cases) {
  ss <- max(round(pop * (1 - percent_immune)) - index_cases, 0)
  return(ss)
}

#' Sample all possible offspring for the next generation
#'
#' @description
#' Sample next generation of offspring using offspring distribution and
#' associated parameters. This function is used internally, and input
#' checking is not performed here, only in the context where it is used.
#' Using it directly is not recommended.
#' @param offspring_func A function to sample offspring.
#' @param offspring_func_pars A list of parameters for the offspring function.
#' @param n_offspring A numeric vector of the number of offspring per chain.
#' @param chains Numeric indices of chains/infectors being simulated
#'
#' @return A numeric vector of the number of offspring per chain.
#' @keywords internal
.sample_possible_offspring <- function(offspring_func,
                                       offspring_func_pars,
                                       n_offspring,
                                       chains) {
  possible_new_offspring <- do.call(
    offspring_func,
    c(
      list(n = sum(n_offspring[chains])),
      offspring_func_pars
    )
  )
  # check that offspring distribution returns integers
  stopifnot(
    "Offspring distribution must return integers" =
      !all(possible_new_offspring %% 1 > 0)
  )

  return(possible_new_offspring)
}

#' Sample the number of susceptible offspring from all possible offspring
#'
#' @description
#' Sample susceptible offspring to be infected from all possible offspring.
#' This function is used internally, and input checking is not
#' performed here, only in the context where it is used. Using it directly
#' is not recommended.
#' @inheritParams simulate_chains
#' @param new_offspring A numeric vector of the possible new offspring per
#' chain produced by [.sample_possible_offspring()].
#' @return A numeric vector of the number of offspring that can be infected
#' given the current susceptible population size.
#' @keywords internal
.get_susceptible_offspring <- function(new_offspring,
                                       susc_pop,
                                       pop) {
  # We first adjust for the case where susceptible can be Inf but prob can only
  # be maximum 1.
  binom_prob <- min(1, susc_pop / pop, na.rm = TRUE)
  # Sample the number of infectible offspring from all possible offspring
  susceptible_offspring <- stats::rbinom(
    n = length(new_offspring),
    size = new_offspring,
    prob = binom_prob
  )
  return(susceptible_offspring)
}

#' Adjust new offspring if it exceeds the susceptible population size
#' @description
#' This function is used internally, and input checking is not
#' performed here, only in the context where it is used. Using it directly
#' is not recommended.
#' @param next_gen A numeric vector of next generation offspring.
#' @param susc_pop The susceptible population size; A number coercible to
#' integer.
#'
#' @return A numeric vector of the adjusted next generation offspring.
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
  next_gen[as.integer(names(next_gen_count))] <- as.integer(unname(next_gen_count))
  return(next_gen)
}
