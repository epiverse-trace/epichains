#' Simulate transmission chains
#'
#' @description
#' Generates transmission chains using a simple branching process model that
#' accepts an index number of cases that seed the outbreak, a distribution
#' of offspring per case, and a chain size or length/duration (statistic)
#' to track. It optionally accepts other population related inputs
#' such as the population size (defaults to Inf) and percentage of the
#' population initially immune (defaults to 0).
#' @param index_cases Number of index cases to simulate transmission chains for.
#' @param offspring_dist Offspring distribution: a `<character>` string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers).
#' @param statistic `<String>`; Chain statistic to track as the stopping
#' criteria for each chain being simulated when `stat_max` is not `Inf`.
#' Can be one of:
#' \itemize{
#'   \item "size": the total number of cases produced by a chain before it goes
#'   extinct.
#'   \item "length": the total number of ancestors produced by a chain before
#'   it goes extinct.
#' }
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above `stat_max` are set to `stat_max`. Defaults to `Inf`.
#' @param pop `<Integer>`; Population size. Used alongside `percent_immune`. to
#' define the susceptible population. Defaults to `Inf`.
#' @param percent_immune `<numeric>`; Percent of the population immune to
#' infection at the start of the simulation. Used alongside `pop` to initialise
#' the susceptible population. Accepted values lie between 0 and 1.
#' Defaults to 0.
#' @param generation_time The generation time function; the name
#' of a user-defined named or anonymous function with only one argument `n`,
#' representing the number of generation times to sample.
#' @param t0 Start time (if generation time is given); either a single value
#' or a vector of same length as `index_cases` (number of initial cases) with
#' corresponding initial times. Defaults to 0, meaning all cases started at
#' time 0.
#' @param tf Cut-off for the infection times (if generation time is given).
#' Defaults to `Inf`.
#' @param ... Parameters of the offspring distribution as required by R.
#' @return An `<epichains_tree>` object, which is basically a `<data.frame>`
#' with columns `infectee_id`, `sim_id` (a unique ID within each simulation
#' for each infectee), `infector_id`, `generation`, and `time` (of infection)
#' @author James M. Azam, Sebastian Funk
#' @export
#nolint start
#' @details
#' # Calculating chain sizes and lengths
#' The function simulates the chain size for individual \eqn{i} at time
#' \eqn{t}, \eqn{I_{t, i}}, as:
#' \deqn{I_{t, i} = \sum_{i}^{I_{t-1}}X_{t, i},}
#' and the chain length/duration for individual \eqn{i} at time \eqn{t},
#' \eqn{L_{t, i}}, as:
#' \deqn{L_{t, i} = {\sf min}(1, X_{t, i}), }
#' where \eqn{X_{t, i}} is the secondary cases generated by individual \eqn{i}
#' at time \eqn{t}, and \eqn{I_{0, i} = L_{0, i} = 1}.
#'
#' The distribution of secondary cases, \eqn{X_{t, i}} is modelled by the
#' offspring distribution (`offspring_dist`).
#'
#' ## Specifying `generation_time`
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
#' set.seed(123)
#' # Using a Poisson offspring distribution
#' chains_pois_offspring <- simulate_chains(
#'   index_cases = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = "pois",
#'   stat_max = 10,
#'   generation_time = function(n) rep(3, n),
#'   lambda = 2
#' )
#' chains_pois_offspring
#'
#' # Using a Negative binomial offspring distribution
#' chains_nbinom_offspring <- simulate_chains(
#'   index_cases = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = "nbinom",
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
simulate_chains <- function(
    index_cases,
    statistic = c("size", "length"),
    offspring_dist,
    ...,
    stat_max = Inf,
    pop = Inf,
    percent_immune = 0,
    generation_time = NULL,
    t0 = 0,
    tf = Inf) {
  # Input checking
  checkmate::assert_count(index_cases, positive = TRUE)
  checkmate::assert_choice(statistic, choices = c("size", "length"))
  checkmate::assert_string(offspring_dist)
  # check that offspring function exists in the environment
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)
  checkmate::assert(
    is.infinite(stat_max) ||
      checkmate::assert_integerish(stat_max, lower = 0)
  )
  checkmate::assert(
    checkmate::assert_integerish(pop, lower = 0) ||
      is.infinite(pop)
  )
  checkmate::assert_number(
    percent_immune,
    lower = 0, upper = 1
  )
  if (!missing(generation_time)) {
    check_generation_time_valid(generation_time)
  } else if (!missing(tf)) {
    stop("If `tf` is specified, `generation_time` must be specified too.")
  }
  checkmate::assert_numeric(
    t0,
    lower = 0, finite = TRUE
  )
  checkmate::assert_number(
    tf,
    lower = 0
  )
  # Gather offspring distribution parameters
  pars <- list(...)

  # Initialisations
  stat_track <- rep(1, index_cases) # track statistic (length or size)
  n_offspring <- rep(1, index_cases) # current number of offspring
  sim <- seq_len(index_cases) # track trees that are still being simulated
  infector_ids <- rep(1, index_cases)

  # initialise list of data frame to hold the transmission trees
  generation <- 1L
  tree_df <- list(
    data.frame(
      infectee_id = seq_len(index_cases),
      sim_id = 1L,
      infector_id = NA_integer_,
      generation = generation
    )
  )
  # Initialise susceptible population
  susc_pop <- max(round(pop * (1 - percent_immune)) - index_cases, 0)

  # Add optional columns
  if (!missing(generation_time)) {
    tree_df[[generation]]$time <- t0
    times <- tree_df[[generation]]$time
  }
  if (!missing(pop)) {
    tree_df[[generation]]$susc_pop <- susc_pop
  }
  # next, simulate n trees
  while (length(sim) > 0 && susc_pop > 0) {
    # sample next generation of offspring
    next_gen <- do.call(
      roffspring_name,
      c(
        list(n = sum(n_offspring[sim])),
        pars
      )
    )
    # check that offspring distribution returns integers
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }
    # Sample susceptible offspring to be infected from all possible offspring
    next_gen <- rbinom(n = next_gen, size = next_gen, prob = susc_pop / pop)
    # Adjust next_gen if sum exceeds the number of susceptibles
    # Adjust next_gen if the number of offspring is greater than the
    # susceptible population.
    if (sum(next_gen) > susc_pop) {
      ## create hypothetical next generation individuals to sample from
      next_gen_pop <- rep(seq_along(next_gen), times = next_gen)
      ## sample from hypothetical next generation
      next_gen_sample <- sample(x = next_gen_pop, size = susc_pop)
      ## sample from hypothetical individuals so that total = susc_pop
      ## create adjusted next_gen vector
      next_gen <- rep(0L, length(next_gen))
      ## count occurrences in next generation sample
      next_gen_count <- table(next_gen_sample)
      next_gen[as.integer(names(next_gen_count))] <- unname(next_gen_count)
    }
    # record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    # initialise placeholder for the number of offspring
    n_offspring <- rep(0, index_cases)
    # assign offspring sum to indices still being simulated
    n_offspring[sim] <- if (all(next_gen) == 0) {
      0
    } else {
      tapply(next_gen, indices, sum)
    }
    # track size/length
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )
    # Adorn the new offspring with their information: their ids, their
    # infector's ids, and the generation they were infected in.
    # Also update the susceptible population and generation.
    if (sum(n_offspring[sim]) > 0) {
      infectors <- rep(infector_ids, next_gen)
      current_max_id <- unname(tapply(infector_ids, indices, max))
      indices <- rep(sim, n_offspring[sim])

      # create new ids
      ids <- rep(current_max_id, n_offspring[sim]) +
        unlist(lapply(n_offspring[sim], seq_len))

      # increment the generation
      generation <- generation + 1L
      # Update susceptible population
      susc_pop <- susc_pop - sum(n_offspring[sim])

      # store new simulation results
      tree_df[[generation]] <-
        data.frame(
          infectee_id = indices,
          sim_id = ids,
          infector_id = infectors,
          generation = generation
        )

      # if a generation time model/function was specified, use it
      # to generate generation times for the cases
      if (!missing(generation_time)) {
        times <- rep(times, next_gen) + generation_time(sum(n_offspring))
        current_min_time <- unname(tapply(times, indices, min))
        tree_df[[generation]]$time <- times
      }
      if (!missing(pop)) {
        tree_df[[generation]]$susc_pop <- susc_pop
      }
    }

    ## Find chains that can still be simulated: those that have still offspring
    ## and aren't of the specified stat_max
    sim <- which(n_offspring > 0 & stat_track < stat_max)
    if (length(sim) > 0) {
      if (!missing(generation_time)) {
        ## only continue to simulate trees that don't go beyond tf
        sim <- intersect(sim, unique(indices)[current_min_time < tf])
      }
      if (!missing(generation_time)) {
        times <- times[indices %in% sim]
      }
      infector_ids <- ids[indices %in% sim]
    }
  }

  # Combine the results
  tree_df <- do.call(rbind, tree_df)

  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  # Post processing
  #   # sort by sim_id and infector_id
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$infector_id), ]
  rownames(tree_df) <- NULL
  out <- epichains_tree(
    tree_df,
    ntrees = index_cases,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    track_pop = !missing(pop)
  )
  return(out)
}


#' Simulate transmission trees from an initial number of infections
#'
#' @param ntrees Number of trees to simulate.
#' @param offspring_dist Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers).
#' @param statistic String; Statistic (size/length) to calculate. Used to
#' determine stopping criteria for simulations when `stat_max` is finite.
#' Can be one of:
#' \itemize{
#'   \item "size": the total number of offspring.
#'   \item "length": the total number of infectors.
#' }
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to this value.
#' Defaults to `Inf`.
#' @param generation_time The generation time function; the name
#' of a user-defined named or anonymous function with only one argument `n`,
#' representing the number of generation times to sample.
#' @param t0 Start time (if generation time is given); either a single value
#' or a vector of same length as `ntrees` (number of simulations) with
#' initial times. Defaults to 0.
#' @param tf End time (if generation time is given).
#' @param ... Parameters of the offspring distribution as required by R.
#' @return An `<epichains_tree>` object, which is basically a `<data.frame>`
#' with columns `infectee_id`, `sim_id` (a unique ID within each simulation
#' for each infectee), `infector_id`, `generation`, and `time` (of infection)
#' @author James M. Azam, Sebastian Funk
#' @export
#nolint start
#' @details
#' # Calculating chain sizes and lengths
#' The function simulates the chain size for individual \eqn{i} at time
#' \eqn{t}, \eqn{I_{t, i}}, as:
#' \deqn{I_{t, i} = \sum_{i}^{I_{t-1}}X_{t, i},}
#' and the chain length/duration for individual \eqn{i} at time \eqn{t},
#' \eqn{L_{t, i}}, as:
#' \deqn{L_{t, i} = {\sf min}(1, X_{t, i}), }
#' where \eqn{X_{t, i}} is the secondary cases generated by individual \eqn{i}
#' at time \eqn{t}, and \eqn{I_{0, i} = L_{0, i} = 1}.
#'
#' The distribution of secondary cases, \eqn{X_{t, i}} is modelled by the
#' offspring distribution (`offspring_dist`).
#'
#' ## Specifying `generation_time`
#'
#' `generation_time` must be specified as a function with one argument.
#'
#' For example, assuming we want to specify the generation time
#' as a random log-normally distributed variable with
#' `meanlog = 0.58` and `sdlog = 1.58`, we could define a named function,
#' let's call it "generation_time_fn", with only one argument representing the
#' number of generation times to sample:
#' \code{generation_time_fn <- function(n){rlnorm(n, 0.58, 1.38)}},
#' and assign the name of the function to `generation_time` in
#' the simulation function, i.e.
#' \code{`simulate_*`(..., generation_time = generation_time_fn)},
#' where `...` are the other arguments to `simulate_*()` and * is a placeholder
#' for the rest of simulation function's name.
#'
#' Alternatively, we could assign an anonymous function to `generation_time`
#' in the `simulate_*()` call, i.e.
#' \code{simulate_*(..., generation_time = function(n){rlnorm(n, 0.58, 1.38)})},
#' where `...` are the other arguments to `simulate_*()`.
#nolint end
#' @seealso
#' * [simulate_summary()] for simulating transmission chains
#'   statistics (sizes or lengths) without the infection tree.
#' * [simulate_tree_from_pop()] for simulating transmission trees from a
#'   susceptible or partially immune population.
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
#' @references
#' Lehtinen S, Ashcroft P, Bonhoeffer S. On the relationship
#' between serial interval, infectiousness profile and generation time.
#' J R Soc Interface. 2021 Jan;18(174):20200756.
#' \doi{10.1098/rsif.2020.0756}. Epub 2021 Jan 6.
#' PMID: 33402022; PMCID: PMC7879757.
#'
#' Fine PE. The interval between successive cases of an
#' infectious disease. Am J Epidemiol. 2003 Dec 1;158(11):1039-47.
#' \doi{10.1093/aje/kwg251. PMID: 14630599}
#'
#' Jacob C. (2010). Branching processes: their role in epidemiology.
#' International journal of environmental research and public health, 7(3),
#' 1186–1204. \doi{https://doi.org/10.3390/ijerph7031204}
simulate_tree <- function(ntrees, statistic = c("size", "length"),
                          offspring_dist, stat_max = Inf,
                          generation_time, t0 = 0,
                          tf = Inf, ...) {
  # Input checking
  checkmate::assert_count(ntrees, positive = TRUE)
  checkmate::assert_choice(
    statistic,
    choices = c("size", "length")
  )
  checkmate::assert_string(offspring_dist)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)

  checkmate::assert_number(
    stat_max, lower = 0
  )

  if (!missing(generation_time)) {
    check_generation_time_valid(generation_time)
  } else if (!missing(tf)) {
    stop("If `tf` is specified, `generation_time` must be specified too.")
  }
  checkmate::assert_numeric(
    t0, lower = 0, finite = TRUE
  )
  checkmate::assert_number(
    tf, lower = 0
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  # Initialisations
  stat_track <- rep(1, ntrees) # track length or size (depending on `statistic`)
  n_offspring <- rep(1, ntrees) # current number of offspring
  sim <- seq_len(ntrees) # track trees that are still being simulated
  infector_ids <- rep(1, ntrees)

  # initialise data frame to hold the transmission trees
  generation <- 1L
  tree_df <- data.frame(
    infectee_id = seq_len(ntrees),
    sim_id = 1L,
    infector_id = NA_integer_,
    generation = generation
  )

  if (!missing(generation_time)) {
    tree_df$time <- t0
    times <- tree_df$time
  }

  # next, simulate n trees
  while (length(sim) > 0) {
    # simulate next generation
    next_gen <- do.call(
      get(roffspring_name),
      c(
        list(n = sum(n_offspring[sim])),
        pars
      )
    )
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }

    # record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    # initialise placeholder for the number of offspring
    n_offspring <- rep(0, ntrees)
    # assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )

    # record times/infectors
    if (sum(n_offspring[sim]) > 0) {
      infectors <- rep(infector_ids, next_gen)
      current_max_id <- unname(tapply(infector_ids, indices, max))
      indices <- rep(sim, n_offspring[sim])

      # create new ids
      ids <- rep(current_max_id, n_offspring[sim]) +
        unlist(lapply(n_offspring[sim], seq_len))

      # increment the generation
      generation <- generation + 1L

      # store new simulation results
      new_df <-
        data.frame(
          infectee_id = indices,
          sim_id = ids,
          infector_id = infectors,
          generation = generation
        )

      # if a generation time model/function was specified, use it
      # to generate generation times for the cases
      if (!missing(generation_time)) {
        times <- rep(times, next_gen) + generation_time(sum(n_offspring))
        current_min_time <- unname(tapply(times, indices, min))
        new_df$time <- times
      }
      tree_df <- rbind(tree_df, new_df)
    }

    ## only continue to simulate trees that have offspring and aren't of
    ## the specified maximum size/length
    sim <- which(n_offspring > 0 & stat_track < stat_max)
    if (length(sim) > 0) {
      if (!missing(generation_time)) {
        ## only continue to simulate trees that don't go beyond tf
        sim <- intersect(sim, unique(indices)[current_min_time < tf])
      }
      if (!missing(generation_time)) {
        times <- times[indices %in% sim]
      }
      infector_ids <- ids[indices %in% sim]
    }
  }

  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  # sort by sim_id and infector_id
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$infector_id), ]
  rownames(tree_df) <- NULL
  out <- epichains_tree(
    tree_df = tree_df,
    ntrees = ntrees,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    track_pop = FALSE
  )
  return(out)
}



#' Simulate transmission chains sizes/lengths
#'
#' @inheritParams simulate_tree
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to `Inf`.
#' @inheritSection simulate_tree Calculating chain sizes and lengths
#' @author James M. Azam, Sebastian Funk
#' @seealso
#' * [simulate_tree()] for simulating transmission trees from an
#'   initial number of infections.
#' * [simulate_tree_from_pop()] for simulating transmission trees from a
#'   susceptible or partially immune population.
#' @examples
#' simulate_summary(
#'   ntrees = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
#'   stat_max = 10,
#'   lambda = 2
#' )
#' @export
simulate_summary <- function(ntrees, statistic = c("size", "length"),
                             offspring_dist,
                             stat_max = Inf, ...) {
  # Input checking
  checkmate::assert_count(ntrees, positive = TRUE)
  statistic <- match.arg(statistic)
  checkmate::assert_choice(
    statistic,
    choices = c("size", "length")
  )

  # check that offspring is properly specified
  checkmate::assert_string(offspring_dist)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)

  checkmate::assert_number(
    stat_max, lower = 0
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  # Initialisations
  stat_track <- rep(1, ntrees) ## track length or size (depending on `stat`)
  n_offspring <- rep(1, ntrees) ## current number of offspring
  sim <- seq_len(ntrees) ## track trees that are still being simulated

  ## next, simulate ntrees trees
  while (length(sim) > 0) {
    ## simulate next generation
    next_gen <- do.call(
      get(roffspring_name),
      c(
        list(n = sum(n_offspring[sim])),
        pars
      )
    )
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }

    ## record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    ## initialise number of offspring
    n_offspring <- rep(0, ntrees)
    ## assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )

    ## only continue to simulate trees that have offspring and aren't of
    ## stat_max size/length
    sim <- which(n_offspring > 0 & stat_track < stat_max)
  }

  stat_track[stat_track >= stat_max] <- Inf

  out <- epichains_summary(
    chains_summary = stat_track,
    ntrees = ntrees,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max
  )

  return(out)
}

#' Simulate transmission trees from a susceptible or partially immune
#' population
#'
#' @inheritParams simulate_tree
#' @param pop The susceptible population size.
#' @param offspring_dist Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers). Only supports "pois" and "nbinom".
#' @param initial_immune The number of initial immunes in the population.
#' Must be less than `pop` - 1.
#' @param t0 Start time; Defaults to 0.
#' @param tf End time; Defaults to `Inf`.
#' @return An `<epichains>` object, which is basically a `<data.frame>` with
#' columns `sim_id` (a unique ID within each simulation for each infectee
#' in the chain), `infector_id`, `generation`, and `time` (of infection).
#' @details
#' # Offspring distributions
#' Currently, `offspring_dist` only supports "pois" & "nbinom".
#' Internally, the respective truncated poisson and negative binomial
#' distributions are used to avoid the situation where there are more cases
#' than susceptibles at any point.
#'
#' The poisson model has mean, lambda, parametrised as:
#' \deqn{{\sf lambda} = \dfrac{{\sf lambda} \times ({\sf pop} -
#' {\sf initial\_immune} - 1)}{{\sf pop}}}
#'
#' The negative binomial model, has mean, mu, parametrised as:
#' \deqn{{\sf mu} = \dfrac{{\sf mu} \times ({\sf pop} -
#' {\sf initial\_immune} - 1)}{{\sf pop}},}
#' and dispersion, size, parametrised as:
#' \deqn{{\sf size} = \dfrac{{\sf mu}}{{\sf size} - 1}.}
#' This is why `size` must be greater than 1.
#'
#' # Differences with `simulate_tree()`
#' `simulate_tree_from_pop()` has a couple of key differences from
#' `simulate_tree()`:
#'  * the maximal chain statistic is limited by `pop` instead of
#'  `stat_max` (in `simulate_tree()`),
#'  * `offspring_dist` can only handle "pois" and "nbinom".
#' @author Flavio Finger, James M. Azam, Sebastian Funk
#' @seealso
#' * [simulate_tree()] for simulating transmission trees from an
#'   initial number of infections.
#' * [simulate_summary()] for simulating transmission chains
#'   statistics (sizes or lengths) without the infection tree.
#' @examples
#' # Simulate with poisson offspring
#' simulate_tree_from_pop(
#'   pop = 100,
#'   offspring_dist = "pois",
#'   lambda = 0.5,
#'   generation_time = function(n) rep(3, n)
#' )
#'
#' # Simulate with negative binomial offspring
#' simulate_tree_from_pop(
#' pop = 100, offspring_dist = "nbinom",
#' mu = 0.5,
#' size = 1.1,
#' generation_time = function(n) rep(3, n)
#' )
#' @export
simulate_tree_from_pop <- function(pop,
                                   offspring_dist = c("pois", "nbinom"),
                                   generation_time,
                                   initial_immune = 0,
                                   t0 = 0,
                                   tf = Inf,
                                   ...) {
  # Input checking
  checkmate::assert_number(
    pop, lower = 1, finite = TRUE
  )
  offspring_dist <- match.arg(offspring_dist)
  checkmate::assert_choice(
    offspring_dist,
    choices = c("pois", "nbinom")
  )
  if (!missing(generation_time)) {
    check_generation_time_valid(generation_time)
  }
  checkmate::assert_number(
    initial_immune, lower = 0, upper = pop - 1
  )
  checkmate::assert_number(
    t0, lower = 0, finite = TRUE
  )
  checkmate::assert_number(
    tf, lower = 0
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  if (offspring_dist == "pois") {
    ## Use a right truncated poisson distribution
    ## to avoid more cases than susceptibles
    offspring_func <- function(n, susc) {
      truncdist::rtrunc(
        n,
        spec = "pois",
        lambda = pars$lambda * susc / pop,
        b = susc
      )
    }
  } else if (offspring_dist == "nbinom") {
    if (is.null(pars$size)) {
      stop("'size' must be specified.")
    } else if (pars$size <= 1) { ## dispersion coefficient
      stop(
        "Offspring distribution 'nbinom' requires",
        "argument 'size' > 1.",
        "Use 'pois' if there is no overdispersion."
      )
    }
    ## get distribution params from mean and dispersion
    offspring_func <- function(n, susc) {
      ## get distribution params from mean and dispersion
      ## see ?rnbinom for parameter definition
      new_mn <- pars$mu * susc / pop ## apply susceptibility
      size <- new_mn / (pars$size - 1)

      ## using a right truncated nbinom distribution
      ## to avoid more cases than susceptibles
      truncdist::rtrunc(
        n,
        spec = "nbinom",
        b = susc,
        mu = new_mn,
        size = size
      )
    }
  }

  ## initializations
  tree_df <- data.frame(
    sim_id = 1L,
    infector_id = NA_integer_,
    generation = 1L,
    time = t0,
    offspring_generated = FALSE # tracks simulation and dropped afterwards
  )

  susc <- pop - initial_immune - 1L
  t <- t0

  ## continue if any unsimulated trees have t <= tf
  ## AND there is still susceptibles left
  while (any(tree_df$time[!tree_df$offspring_generated] <= tf) && susc > 0) {
    ## select from which case to generate offspring
    t <- min(tree_df$time[!tree_df$offspring_generated]) # lowest unsimulated t

    ## index of the first in df with t, extract vars
    idx <- which(tree_df$time == t & !tree_df$offspring_generated)[1]
    id_parent <- tree_df$sim_id[idx]
    t_parent <- tree_df$time[idx]
    gen_parent <- tree_df$generation[idx]

    ## generate it
    current_max_id <- max(tree_df$sim_id)
    n_offspring <- offspring_func(1, susc)

    if (n_offspring %% 1 > 0) {
      stop("Offspring distribution must return integers")
    }

    ## mark as done
    tree_df$offspring_generated[idx] <- TRUE

    ## add to df
    if (n_offspring > 0) {
      ## draw generation times
      new_times <- generation_time(n_offspring)

      if (any(new_times < 0)) {
        stop("Generation time must be >= 0.")
      }

      new_df <- data.frame(
        sim_id = current_max_id + seq_len(n_offspring),
        infector_id = id_parent,
        generation = gen_parent + 1L,
        time = new_times + t_parent,
        offspring_generated = FALSE
      )

      ## add new cases to tree_df
      tree_df <- rbind(tree_df, new_df)
    }

    ## adjust susceptibles
    susc <- susc - n_offspring
  }

  ## remove cases with time > tf that could
  ## have been generated in the last generation
  tree_df <- tree_df[tree_df$time <= tf, ]

  # sort by sim_id and infector
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$infector_id), ]
  tree_df$offspring_generated <- NULL
  rownames(tree_df) <- NULL

  out <- epichains_tree(
    tree_df,
    ntrees = NULL,
    statistic = NULL,
    offspring_dist = offspring_dist,
    stat_max = NULL,
    track_pop = TRUE
  )
  return(out)
}
