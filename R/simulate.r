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
#'   index_cases = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
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
# nolint start: cyclocomp_linter.
simulate_chains <- function(index_cases,
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
  checkmate::assert_count(
    index_cases,
    positive = TRUE
  )
  checkmate::assert_choice(
    statistic,
    choices = c("size", "length")
  )
  checkmate::assert_string(offspring_dist)
  # check that offspring function exists in the environment
  roffspring_name <- paste0(
    "r",
    offspring_dist
  )
  .check_offspring_func_valid(roffspring_name)
  checkmate::assert(
    is.infinite(stat_max) ||
      checkmate::assert_integerish(stat_max, lower = 0)
  )
  checkmate::assert(
    is.infinite(pop) ||
      checkmate::assert_integerish(pop, lower = 1)
  )
  checkmate::assert_number(
    percent_immune,
    lower = 0, upper = 1
  )
  if (!missing(generation_time)) {
    .check_generation_time_valid(generation_time)
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
    stopifnot(
      "Offspring distribution must return integers" =
        !all(next_gen %% 1 > 0)
    )
    # Sample susceptible offspring to be infected from all possible offspring
    # We first adjust for the case where susceptible can be Inf but prob is max
    # 1.
    binom_prob <- min(1, susc_pop / pop, na.rm = TRUE)
    next_gen <- stats::rbinom(
      n = length(next_gen),
      size = next_gen,
      prob = binom_prob
    )
    # Adjust next_gen if the number of offspring is greater than the
    # susceptible population.
    if (sum(next_gen) > susc_pop) {
      next_gen <- .adjust_next_gen(
        next_gen = next_gen,
        susc_pop = susc_pop
      )
    }
    # record parent ids corresponding to the number of offspring
    parent_ids <- rep(sim, n_offspring[sim])

    # initialise placeholder for the number of offspring
    n_offspring <- rep(0, index_cases)
    # assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, parent_ids, sum)
    # track size/length
    stat_track <- .update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )
    # Adorn the new offspring with their information: their ids, their
    # infector's ids, and the generation they were infected in.
    # Also update the susceptible population and generation.
    if (sum(n_offspring) > 0) {
      infectors <- rep(infector_ids, next_gen)
      current_max_id <- unname(tapply(infector_ids, parent_ids, max))
      parent_ids <- rep(sim, n_offspring[sim])

      # create new ids
      ids <- rep(current_max_id, n_offspring[sim]) +
        sequence(n_offspring[sim])

      # increment the generation
      generation <- generation + 1L
      # Update susceptible population
      susc_pop <- susc_pop - sum(n_offspring)

      # store new simulation results
      tree_df[[generation]] <-
        data.frame(
          infectee_id = parent_ids,
          sim_id = ids,
          infector_id = infectors,
          generation = generation
        )

      # if a generation time model/function was specified, use it
      # to generate generation times for the cases
      if (!missing(generation_time)) {
        times <- rep(times, next_gen) + generation_time(sum(n_offspring))
        current_min_time <- unname(tapply(times, parent_ids, min))
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
        sim <- intersect(sim, unique(parent_ids)[current_min_time < tf])
        times <- times[parent_ids %in% sim]
      }
      infector_ids <- ids[parent_ids %in% sim]
    }
  }

  # Combine the results
  tree_df <- do.call(rbind, tree_df)

  # time column only exists if tf was specified
  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  # Post processing
  #   # sort by sim_id and infector_id
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$infector_id), ]
  rownames(tree_df) <- NULL
  out <- epichains_tree(
    tree_df,
    index_cases = index_cases,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max,
    track_pop = !missing(pop)
  )
  return(out)
}
# nolint end

#' Simulate a vector of transmission chains sizes/lengths
#'
#' @inheritParams simulate_chains
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to `Inf`.
#' @inheritSection simulate_chains Calculating chain sizes and lengths
#' @author James M. Azam, Sebastian Funk
#' @examples
#' simulate_summary(
#'   index_cases = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
#'   stat_max = 10,
#'   lambda = 2
#' )
#' @export
simulate_summary <- function(index_cases, statistic = c("size", "length"),
                             offspring_dist,
                             stat_max = Inf, ...) {
  # Input checking
  checkmate::assert_count(index_cases, positive = TRUE)
  statistic <- match.arg(statistic)
  checkmate::assert_choice(
    statistic,
    choices = c("size", "length")
  )

  # check that offspring is properly specified
  checkmate::assert_string(offspring_dist)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  .check_offspring_func_valid(roffspring_name)

  checkmate::assert_number(
    stat_max, lower = 0
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  # Initialisations
  stat_track <- rep(1, index_cases) ## track statistic
  n_offspring <- rep(1, index_cases) ## current number of offspring
  sim <- seq_len(index_cases) ## track trees that are still being simulated

  ## next, simulate trees from index cases
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
    n_offspring <- rep(0, index_cases)
    ## assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- .update_chain_stat(
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
    index_cases = index_cases,
    statistic = statistic,
    offspring_dist = offspring_dist,
    stat_max = stat_max
  )

  return(out)
}
