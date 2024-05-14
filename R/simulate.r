#' Simulate transmission chains
#'
#' @description
#' It generates independent transmission chains starting with a single case
#' per chain, using a simple branching process model (See details for
#' definition of "chains" and assumptions). Offspring for each
#' chain are generated with an offspring distribution, and an optional
#' generation time distribution function.
#'
#' The individual chain simulations are controlled by customisable stopping
#' criteria, including a threshold chain size or length, and a generation time
#' cut off. The function also optionally accepts population related inputs
#' such as the population size (defaults to Inf) and percentage of the
#' population initially immune (defaults to 0).
#' @param nchains Number of chains to simulate.
#' @param offspring_dist Offspring distribution: a `<function>` like the ones
#' provided by R to generate random numbers from given distributions (e.g.,
#' \code{\link{rpois}} for Poisson). More specifically, the function needs to
#' accept at least one argument, \code{n}, which is the number of random
#' numbers to generate. It can accept further arguments, which will be passed
#' on to the random number generating functions. Examples that can be provided
#' here are `rpois` for Poisson distributed offspring, `rnbinom` for negative
#' binomial offspring, or custom functions.
#' @param statistic The chain statistic to track as the
#' stopping criteria for each chain being simulated when `stat_max` is not
#' `Inf`; A `<string>`. It can be one of:
#' \itemize{
#'   \item "size": the total number of cases produced by a chain before it goes
#'   extinct.
#'   \item "length": the total number of generations reached by a chain before
#'   it goes extinct.
#' }
#' @param stat_max The cut off for the chain statistic (size/length) being
#' computed; A number coercible to integer. Results above `stat_max` are set to
#' `stat_max`; An `<integer>`. Defaults to `Inf`.
#' @param pop Population size; An `<Integer>`. Used alongside `percent_immune`
#' to define the susceptible population. Defaults to `Inf`.
#' @param percent_immune Percent of the population immune to
#' infection at the start of the simulation; A `<numeric>` between 0 and 1.
#' Used alongside `pop` to initialise the susceptible population. Defaults to
#' 0.
#' @param generation_time The generation time function; the name
#' of a user-defined named or anonymous function with only one argument `n`,
#' representing the number of generation times to sample.
#' @param t0 Start time (if generation time is given); either a single value
#' or a vector of same length as `nchains` (number of initial cases) with
#' corresponding initial times. Defaults to 0, meaning all cases started at
#' time 0.
#' @param tf A number for the cut-off for the infection times (if generation
#' time is given); Defaults to `Inf`.
#' @param ... Parameters of the offspring distribution as required by R.
#' @return An `<epichains>` object, which is basically a `<data.frame>`
#' with columns:
#' * `chain` - an ID for active/ongoing chains,
#' * `infectee` - a unique ID for each infectee.
#' * `infector` - an ID for the infector of each infectee.
#' * `generation` - a discrete time point during which infection occurs, and
#' optionally,
#' * `time` - the time of infection.
#' @author James M. Azam, Sebastian Funk
#' @export
#nolint start
#' @details
#' # Definition of a transmission chain
#' A transmission chain as used here is an independent case and all
#' the secondary cases linked to it through transmission. The chain starts with
#' a single case, and each case in the chain generates secondary cases
#' according to the offspring distribution. The chain ends when no more
#' secondary cases are generated.
#'
#' # Calculating chain sizes and lengths
#' The function simulates the chain size for chain \eqn{i} at time
#' \eqn{t}, \eqn{I_{t, i}}, as:
#' \deqn{I_{t, i} = \sum_{i}^{I_{t-1}}X_{t, i},}
#' and the chain length/duration for chain \eqn{i} at time \eqn{t},
#' \eqn{L_{t, i}}, as:
#' \deqn{L_{t, i} = {\sf min}(1, X_{t, i}), }
#' where \eqn{X_{t, i}} is the secondary cases generated by chain \eqn{i}
#' at time \eqn{t}, and \eqn{I_{0, i} = L_{0, i} = 1}.
#'
#' The distribution of secondary cases, \eqn{X_{t, i}} is modelled by the
#' offspring distribution (`offspring_dist`).
#'
#' # Specifying `generation_time`
#'
#' The argument `generation_time` must be specified as a function with
#' one argument, `n`.
#'
#' For example, assuming we want to specify the generation time as a random
#' log-normally distributed variable with `meanlog = 0.58` and `sdlog = 1.58`,
#' we could define a named function, let's call it "generation_time_fn",
#' with only one argument representing the number of generation times to
#' sample: \code{generation_time_fn <- function(n){rlnorm(n, 0.58, 1.38)}},
#' and assign the name of the function to `generation_time` in
#' the simulation function, i.e.
#' \code{`simulate_*`(..., generation_time = generation_time_fn)},
#' where `...` are the other arguments to `simulate_*()` and * is a placeholder
#' for the rest of simulation function's name.
#'
#' Alternatively, we could assign an anonymous function to `generation_time`
#' in the `simulate_*()` call, i.e.
#' \code{simulate_*(..., generation_time = function(n){rlnorm(n, 0.58, 1.38)})}
#' OR \code{simulate_*(..., generation_time = \(n){rlnorm(n, 0.58, 1.38)})},
#' where `...` are the other arguments to `simulate_*()`.
#nolint end
#' @examples
#' # Using a Poisson offspring distribution and simulating from an infinite
#' # population up to chain size 10.
#' set.seed(32)
#' chains_pois_offspring <- simulate_chains(
#'   nchains = 10,
#'   statistic = "size",
#'   offspring_dist = rpois,
#'   stat_max = 10,
#'   generation_time = function(n) rep(3, n),
#'   lambda = 2
#' )
#' chains_pois_offspring
#'
#' # Using a Negative binomial offspring distribution and simulating from a
#' # finite population up to chain size 10.
#' set.seed(32)
#' chains_nbinom_offspring <- simulate_chains(
#'   nchains = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   stat_max = 10,
#'   generation_time = function(n) rep(3, n),
#'   mu = 2,
#'   size = 0.2
#' )
#' chains_nbinom_offspring
#' @references
#' Jacob C. (2010). Branching processes: their role in epidemiology.
#' International journal of environmental research and public health, 7(3),
#' 1186–1204. \doi{https://doi.org/10.3390/ijerph7031204}
#'
#' Blumberg, S., and J. O. Lloyd-Smith. 2013. "Comparing Methods for
#' Estimating R0 from the Size Distribution of Subcritical Transmission
#' Chains." Epidemics 5 (3): 131–45.
#' \doi{https://doi.org/10.1016/j.epidem.2013.05.002}.
#'
#' Farrington, C. P., M. N. Kanaan, and N. J. Gay. 2003.
#' "Branching Process Models for Surveillance of Infectious Diseases
#' Controlled by Mass Vaccination.” Biostatistics (Oxford, England)
#' 4 (2): 279–95. \doi{https://doi.org/10.1093/biostatistics/4.2.279}.
simulate_chains <- function(nchains,
                            statistic = c("size", "length"),
                            offspring_dist,
                            ...,
                            stat_max = Inf,
                            pop = Inf,
                            percent_immune = 0,
                            generation_time = NULL,
                            t0 = 0,
                            tf = Inf) {
  # Check offspring and population-related arguments
  .check_sim_args(
    nchains = nchains,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    pop = pop,
    percent_immune = percent_immune
  )
  # Check time-related arguments
  # Since tf is passed to .check_time_args, we need to check if it is specified
  # in this function environment. If tf is specified, we expect generation_time
  # to be specified too.
  tf_specified <- !missing(tf)
  .check_time_args(
    generation_time = generation_time,
    t0 = t0,
    tf_specified = tf_specified,
    tf = tf
  )
  # Gather offspring distribution parameters
  pars <- list(...)

  # Initialisations
  stat_track <- rep(1, nchains) # track statistic (length or size)
  n_offspring <- rep(1, nchains) # current number of offspring
  chains_active <- seq_len(nchains) # track ongoing simulations
  new_infectors_ids <- rep(1, nchains) # track infectors

  # initialise list of data frame to hold the transmission trees
  generation <- 1L
  sim_df <- list(
    data.frame(
      chain = seq_len(nchains),
      infectee = 1L,
      infector = NA_integer_, # infectors are unknown in the first generation
      generation = generation
    )
  )
  # Initialise susceptible population
  susc_pop <- .init_susc_pop(pop, percent_immune, nchains)

  # Add optional columns
  if (!missing(generation_time)) {
    sim_df[[generation]]$time <- t0
    times <- sim_df[[generation]]$time
  }
  # Simulate chains until stopping conditions are met
  while (length(chains_active) > 0 && susc_pop > 0) {
    # simulate the next possible offspring
    next_gen <- .sample_possible_offspring(
      offspring_func = offspring_dist,
      offspring_func_pars = pars,
      n_offspring = n_offspring,
      chains = chains_active
    )
    # from all possible offspring, get those that could be infected
    next_gen <- .get_susceptible_offspring(
      new_offspring = next_gen,
      susc_pop = susc_pop,
      pop = pop
    )
    # Adjust the infectibles if they exceed the susceptible population
    if (sum(next_gen) > susc_pop) {
      next_gen <- .adjust_next_gen(
        next_gen = next_gen,
        susc_pop = susc_pop
      )
    }
    # create ids linking active simulations to their offspring
    active_chain_ids <- rep(chains_active, n_offspring[chains_active])

    # initialise place holder for the number of offspring
    n_offspring <- rep(0, nchains)
    # find the number of new offspring for each active simulation
    n_offspring[chains_active] <- tapply(next_gen, active_chain_ids, sum)
    # update the size/length statistic
    stat_track <- .update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )
    # Adorn the new offspring with their information: their ids, their
    # infector's ids, and the generation they were infected in.
    # Also add the time of infection if generation_time was specified
    if (sum(n_offspring) > 0) {
      infector_ids <- rep(new_infectors_ids, next_gen)
      current_max_id <- unname(tapply(new_infectors_ids, active_chain_ids, max))
      active_chain_ids <- rep(chains_active, n_offspring[chains_active])

      # ids linking each new offspring to their original simulation
      sim_offspring_ids <- rep(current_max_id, n_offspring[chains_active]) +
        sequence(n_offspring[chains_active])

      # increment the generation
      generation <- generation + 1L
      # Update susceptible population
      susc_pop <- susc_pop - sum(n_offspring)

      # store new simulation results
      sim_df[[generation]] <-
        data.frame(
          chain = active_chain_ids,
          infectee = sim_offspring_ids,
          infector = infector_ids,
          generation = generation
        )

      # if a generation time model/function was specified, use it
      # to generate generation times for the cases
      if (!missing(generation_time)) {
        times <- rep(times, next_gen) + generation_time(sum(n_offspring))
        current_min_time <- unname(tapply(times, active_chain_ids, min))
        sim_df[[generation]]$time <- times
      }
    }

    # Find ongoing simulations (those still infecting): those that have still
    # offspring and haven't reached the threshold statistic yet
    chains_active <- which(n_offspring > 0 & stat_track < stat_max)
    if (length(chains_active) > 0) {
      if (!missing(generation_time)) {
        ## only continue to simulate trees that don't go beyond tf
        unique_index_cases <- unique(active_chain_ids)
        chains_active <- intersect(
          chains_active,
          unique_index_cases[current_min_time < tf]
        )
        times <- times[active_chain_ids %in% chains_active]
      }
      # infectees of active index cases become infectors in the next generation
      new_infectors_ids <- sim_offspring_ids[active_chain_ids %in% chains_active]
    }
  }

  # Combine the results
  sim_df <- do.call(rbind, sim_df)

  # time column only exists if tf was specified
  if (!missing(tf)) {
    sim_df <- sim_df[sim_df$time < tf, ]
  }

  # Post processing
  rownames(sim_df) <- NULL
  # We want to reorder the columns but that depends on whether "time" is
  # present or not, so we need to determine that first
  column_order <- if (missing(generation_time)) {
    c("chain", "infector", "infectee", "generation")
  } else {
    c("chain", "infector", "infectee", "generation", "time")
  }
  sim_df <- sim_df[, c(column_order)]
  out <- .epichains(
    sim_df,
    nchains = nchains,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    track_pop = !missing(pop)
  )
  return(out)
}

#' Simulate a vector of transmission chains statistics (sizes/lengths)
#'
#' @description
#' It generates a vector of transmission chain sizes or lengths using the
#' same model as [simulate_chains()] but without tracking details of the
#' individual chains. This function is useful when only the chain sizes or
#' lengths are of interest.
#'
#' It uses a simple branching process model that simulates independent
#' chains, using an offspring distribution for each chain. Each chain
#' uses a threshold chain size or length as the
#' stopping criterion especially where R0 > 1. The function also optionally
#' accepts population related inputs such as the population size (defaults
#' to Inf) and percentage of the population initially immune (defaults to 0).
#' @inheritParams simulate_chains
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. A number coercible to integer. Results above the specified value,
#' are set to `Inf`.
#' @return An object of class `<epichains_summary>`, which is a numeric
#' vector of chain sizes or lengths with extra attributes for storing the
#' simulation parameters.
#' @inheritSection simulate_chains Definition of a transmission chain
#' @inheritSection simulate_chains Calculating chain sizes and lengths
#' @inherit simulate_chains references
#' @details
#' # `simulate_chain_stats()` vs `simulate_chains()`
#' `simulate_chain_stats()` is a time-invariant version of `simulate_chains()`.
#' In particular, it does not track the details of individual transmission
#' events but deals with eventual chain statistics, that is, the statistic
#' realised by a chain after dying out.
#'
#' It is useful for generating a vector of chain sizes or lengths for a given
#' number of index cases, if details of who infected whom and the timing of
#' infection are not of interest.
#'
#' This function is used in `{epichains}` for calculating likelihoods in
#' the `likelihood()` function and for sampling from the borel
#' distribution (See ?epichains::rborel). It is used extensively in the
#' vignette on
#nolint start
#' [modelling disease control](https://epiverse-trace.github.io/epichains/articles/interventions.html),
#nolint end
#' where only data on observed chain sizes and lengths are available.
#' @author James M. Azam, Sebastian Funk
#' @examples
#' # Simulate chain sizes with a poisson offspring distribution, assuming an
#' # infinite population and no immunity.
#' set.seed(32)
#' simulate_chain_stats(
#'   nchains = 20,
#'   statistic = "size",
#'   offspring_dist = rpois,
#'   stat_max = 10,
#'   lambda = 0.9
#' )
#' # Simulate chain sizes with a negative binomial distribution and assuming
#' # a finite population and 10% immunity.
#' set.seed(32)
#' simulate_chain_stats(
#'   pop = 1000,
#'   percent_immune = 0.1,
#'   nchains = 20,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   stat_max = 10,
#'   mu = 0.9,
#'   size = 0.36
#' )
#' @export
simulate_chain_stats <- function(nchains,
                             statistic = c("size", "length"),
                             offspring_dist,
                             ...,
                             stat_max = Inf,
                             pop = Inf,
                             percent_immune = 0) {
  # Check offspring and population-related arguments
  .check_sim_args(
    nchains = nchains,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    pop = pop,
    percent_immune = percent_immune
  )
  # Gather offspring distribution parameters
  pars <- list(...)

  # Initialisations
  stat_track <- rep(1, nchains) ## track statistic
  n_offspring <- rep(1, nchains) ## current number of offspring
  chains_active <- seq_len(nchains) # track trees being simulated

  # Initialise susceptible population
  susc_pop <- .init_susc_pop(pop, percent_immune, nchains)

  ## next, simulate transmission chains from index cases
  while (length(chains_active) > 0 && susc_pop > 0) {
    # simulate the possible next generation of offspring
    next_gen <- .sample_possible_offspring(
      offspring_func = offspring_dist,
      offspring_func_pars = pars,
      n_offspring = n_offspring,
      chains = chains_active
    )
    # from all possible offspring, get those that are infectible
    next_gen <- .get_susceptible_offspring(
      new_offspring = next_gen,
      susc_pop = susc_pop,
      pop = pop
    )
    # Adjust next_gen if the number of offspring is greater than the
    # susceptible population
    if (sum(next_gen) > susc_pop) {
      next_gen <- .adjust_next_gen(
        next_gen = next_gen,
        susc_pop = susc_pop
      )
    }
    # create ids linking active simulations to their offspring
    active_chain_ids <- rep(chains_active, n_offspring[chains_active])

    ## initialise number of offspring
    n_offspring <- rep(0, nchains)
    ## assign offspring sum to their corresponding simulations
    n_offspring[chains_active] <- tapply(next_gen, active_chain_ids, sum)

    # track size/length
    stat_track <- .update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )
    # Update susceptible population
    susc_pop <- susc_pop - sum(n_offspring)
    # only continue simulations that have offspring and haven't reached stat_max
    chains_active <- which(n_offspring > 0 & stat_track < stat_max)
  }

  stat_track[stat_track >= stat_max] <- Inf

  out <- .epichains_summary(
    chains_summary = stat_track,
    nchains = nchains,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max
  )

  return(out)
}
