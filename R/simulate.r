#' Simulate a tree of infections with a serial and offspring distributions
#'
#' @param nchains number of chains to simulate
#' @param offspring_sampler Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers)
#' @param chain_statistic String; Statistic to calculate. Can be one of:
#' \itemize{
#'   \item "size": the total number of offspring.
#'   \item "length": the total number of ancestors.
#' }
#' @param chain_stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to this value.
#' Defaults to `Inf`.
#' @param serials_sampler The serial interval generator function; the name of a
#' user-defined named or anonymous function with only one argument `n`,
#' representing the number of serial intervals to generate.
#' @param t0 Start time (if serial interval is given); either a single value
#' or a vector of same length as `nchains` (number of simulations) with
#' initial times. Defaults to 0.
#' @param tf End time (if serial interval is given).
#' @param ... Parameters of the offspring distribution as required by R.
#' @return an `epichains` object, which is basically a `data.frame` with
#' columns `chain_id` (chain ID), `sim_id` (a unique ID within each simulation
#' for each individual element of the chain), `ancestor`
#' (the ID of the ancestor of each element), `generation`, and
#' `time` (of infection)
#' @author James M. Azam, Sebastian Funk
#' @export
#' @details
#' `sim_chain_tree()` simulates a branching process of the form:
#' WIP
#' # The serial interval (`serials_sampler`):
#'
#' ## Assumptions/disambiguation
#'
#' In epidemiology, the generation interval is the duration between successive
#' infectious events in a chain of transmission. Similarly, the serial
#' interval is the duration between observed symptom onset times between
#' successive cases in a transmission chain. The generation interval is
#' often hard to observe because exact times of infection are hard to
#' measure hence, the serial interval is often used instead . Here, we
#' use the serial interval to represent what would normally be called the
#' generation interval, that is, the time between successive cases.
#'
#' See References below for some literature on the subject.
#'
#' ## Specifying `serials_sampler` in `sim_chain_tree()`
#'
#' `serials_sampler` must be specified as a named or
#' [anonymous/inline/unnamed function](https://en.wikipedia.org/wiki/Anonymous_function#R) # nolint
#' with one argument.
#'
#' For example, assuming we want to specify the serial interval
#' generator as a random log-normally distributed variable with
#' `meanlog = 0.58` and `sdlog = 1.58`, we could define a named function,
#' let's call it "serial_interval", with only one argument representing the
#' number of serial intervals to sample:
#' \code{serial_interval <- function(n){rlnorm(n, 0.58, 1.38)}},
#' and assign the name of the function to `serials_sampler` in
#' `sim_chain_tree()` like so
#' \code{sim_chain_tree(..., serials_sampler = serial_interval)},
#' where `...` are the other arguments to `sim_chain_tree()`.
#'
#' Alternatively, we could assign an anonymous function to `serials_sampler`
#' in the `sim_chain_tree()` call like so
#' \code{simulate_tree(..., serials_sampler = function(n){rlnorm(n, 0.58, 1.38)})}, #nolint
#' where `...` are the other arguments to `simulate_tree()`.
#' @seealso [simulate_vec()] for simulating transmission chains as a vector
#' @examples
#' set.seed(123)
#' chains <- simulate_tree(nchains = 10, serials_sampler = function(x) 3,
#' offspring = "pois", lambda = 2, infinite = 10)
#' chains
#' @references
#'
#' Lehtinen S, Ashcroft P, Bonhoeffer S. On the relationship
#' between serial interval, infectiousness profile and generation time.
#' J R Soc Interface. 2021 Jan;18(174):20200756.
#' doi: 10.1098/rsif.2020.0756. Epub 2021 Jan 6.
#' PMID: 33402022; PMCID: PMC7879757.
#'
#'
#' Fine PE. The interval between successive cases of an
#' infectious disease. Am J Epidemiol. 2003 Dec 1;158(11):1039-47.
#' doi: 10.1093/aje/kwg251. PMID: 14630599.
#'
simulate_tree <- function(nchains, offspring_sampler,
                           chain_statistic = c("size", "length"),
                           chain_stat_max = Inf, serials_sampler, t0 = 0,
                           tf = Inf, ...) {
  chain_statistic <- match.arg(chain_statistic)

  check_nchains_valid(nchains = nchains)

  # check that offspring is properly specified
  check_offspring_valid(offspring_sampler)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_sampler)
  check_offspring_func_valid(roffspring_name)

  if (!missing(serials_sampler)) {
    check_serial_valid(serials_sampler)
  } else if (!missing(tf)) {
    stop("If `tf` is specified, `serials_sampler` must be specified too.")
  }

  # Initialisations
  stat_track <- rep(1, nchains) # track length or size (depending on `chain_statistic`) #nolint
  n_offspring <- rep(1, nchains) # current number of offspring
  sim <- seq_len(nchains) # track chains that are still being simulated
  ancestor_ids <- rep(1, nchains) # all chains start in generation 1

  # initialise data frame to hold the transmission trees
  generation <- 1L
  tree_df <- data.frame(
    chain_id = seq_len(nchains),
    sim_id = 1L,
    ancestor = NA_integer_,
    generation = generation
  )

  if (!missing(serials_sampler)) {
    tree_df$time <- t0
    times <- tree_df$time
  }

  # next, simulate n chains
  while (length(sim) > 0) {
    # simulate next generation
    next_gen <- get(roffspring_name)(n = sum(n_offspring[sim]), ...)
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }

    # record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    # initialise placeholder for the number of offspring
    n_offspring <- rep(0, nchains)
    # assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- update_chain_stat(stat_type = chain_statistic,
                                    stat_latest = stat_track,
                                    n_offspring = n_offspring)

    # record times/ancestors
    if (sum(n_offspring[sim]) > 0) {
      ancestors <- rep(ancestor_ids, next_gen)
      current_max_id <- unname(tapply(ancestor_ids, indices, max))
      indices <- rep(sim, n_offspring[sim])

      # create new ids
      ids <- rep(current_max_id, n_offspring[sim]) +
        unlist(lapply(n_offspring[sim], seq_len))

      # increment the generation
      generation <- generation + 1L

      # store new simulation results
      new_df <-
        data.frame(
          chain_id = indices,
          sim_id = ids,
          ancestor = ancestors,
          generation = generation
        )

      # if a serial interval model/function was specified, use it
      # to generate serial intervals for the cases
      if (!missing(serials_sampler)) {
        times <- rep(times, next_gen) + serials_sampler(sum(n_offspring))
        current_min_time <- unname(tapply(times, indices, min))
        new_df$time <- times
      }
      tree_df <- rbind(tree_df, new_df)
    }

    ## only continue to simulate chains that have offspring and aren't of
    ## infinite size/length
    sim <- which(n_offspring > 0 & stat_track < chain_stat_max)
    if (length(sim) > 0) {
      if (!missing(serials_sampler)) {
        ## only continue to simulate chains that don't go beyond tf
        sim <- intersect(sim, unique(indices)[current_min_time < tf])
      }
      if (!missing(serials_sampler)) {
          times <- times[indices %in% sim]
          }
        ancestor_ids <- ids[indices %in% sim]
    }
    }

  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  structure(
    tree_df,
    chains = nchains,
    chain_type = "chains_tree",
    rownames = NULL,
    class = c("epichains", "tbl", "data.frame")
  )
}



#' Simulate transmission chains without tree (as a vector)
#'
#' @inheritParams sim_chain_tree
#' @param chain_stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to `Inf`.
#' @examples #' simulate_vect(n = 10, offspring_sampler = "pois", lambda = 2,
#' chain_stat_max = 10)
simulate_vect <- function(nchains, offspring_sampler,
                           chain_statistic = c("size", "length"),
                           chain_stat_max = Inf, ...) {
  chain_statistic <- match.arg(chain_statistic)

  check_nchains_valid(nchains = nchains)

  # check that offspring is properly specified
  check_offspring_valid(offspring_sampler)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_sampler)
  check_offspring_func_valid(roffspring_name)

  # Initialisations
  stat_track <- rep(1, nchains) ## track length or size (depending on `stat`)
  n_offspring <- rep(1, nchains) ## current number of offspring
  sim <- seq_len(nchains) ## track chains that are still being simulated

  ## next, simulate nchains chains
  while (length(sim) > 0) {
    ## simulate next generation
    next_gen <- get(roffspring_name)(n = sum(n_offspring[sim]), ...)
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }

    ## record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    ## initialise number of offspring
    n_offspring <- rep(0, nchains)
    ## assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- update_chain_stat(stat_type = chain_statistic,
                                    stat_latest = stat_track,
                                    n_offspring = n_offspring
                                    )

    ## only continue to simulate chains that offspring and aren't of
    ## chain_stat_max size/length
    sim <- which(n_offspring > 0 & stat_track < chain_stat_max)
  }

  stat_track[stat_track >= chain_stat_max] <- Inf

  structure(
    stat_track,
    chain_type = "chains_vec",
    chains = nchains,
    class = c("epichains", class(stat_track))
  )
}


#'
#' @param offspring
#'
#' @return
#' @export
#' @examples
check_offspring_valid <- function(offspring) {
  if (!is.character(offspring)) {
    stop(sprintf(
      "%s %s",
      "'offspring' must be specified as a character string.",
      "Did you forget to enclose it in quotes?"
    ))
  }
}


#' Check if constructed random number generator for offspring exists
#'
  }
}


#' Check if the serials_sampler argument is specified as a function
#'
  }
}


check_nchains_valid <- function(nchains) {
  if (nchains < 1 || is.infinite(nchains)) {
    stop("`nchains` must be > 0 but less than `Inf`")

#' Determine and update the chain statistic being tracked
  }

