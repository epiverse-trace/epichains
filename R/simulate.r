#' Simulate transmission trees from an initial number of infections
#'
#' @inheritParams intvn_scale_r0
#' @param nchains Number of chains to simulate.
#' @param offspring_dist Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers).
#' @param statistic String; Statistic (size/length) to calculate. Used to
#' determine stopping criteria for simulations when `stat_max` is finite.
#' Can be one of:
#' \itemize{
#'   \item "size": the total number of offspring.
#'   \item "length": the total number of ancestors.
#' }
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to this value.
#' Defaults to `Inf`.
#' @param serials_dist The serial interval distribution function; the name
#' of a user-defined named or anonymous function with only one argument `n`,
#' representing the number of serial intervals to generate. See details.
#' @param t0 Start time (if serial interval is given); either a single value
#' or a vector of same length as `nchains` (number of simulations) with
#' initial times. Defaults to 0.
#' @param tf End time (if serial interval is given).
#' @param ... Parameters of the offspring distribution as required by R.
#' @return An `<epichains>` object, which is basically a `<data.frame>` with
#' columns `chain_id` (chain ID), `sim_id` (a unique ID within each simulation
#' for each individual), `ancestor`
#' (the ID of the ancestor of each individual), `generation`, and
#' `time` (of infection)
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
#' # The serial interval (`serials_dist`)
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
#' ## Specifying `serials_dist`
#'
#' `serials_dist` must be specified as a named or
#' [anonymous/inline/unnamed function](https://en.wikipedia.org/wiki/Anonymous_function#R)
#' with one argument.
#'
#' For example, assuming we want to specify the serial interval
#' distribution as a random log-normally distributed variable with
#' `meanlog = 0.58` and `sdlog = 1.58`, we could define a named function,
#' let's call it "serial_interval", with only one argument representing the
#' number of serial intervals to sample:
#' \code{serial_interval <- function(n){rlnorm(n, 0.58, 1.38)}},
#' and assign the name of the function to `serials_dist` in
#' the simulation function, i.e.
#' \code{`simulate_*`(..., serials_dist = serial_interval)},
#' where `...` are the other arguments to `simulate_*()` and * is a placeholder
#' for the rest of simulation function's name.
#'
#' Alternatively, we could assign an anonymous function to `serials_dist`
#' in the `simulate_*()` call, i.e.
#' \code{simulate_*(..., serials_dist = function(n){rlnorm(n, 0.58, 1.38)})},
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
#'   nchains = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
#'   stat_max = 10,
#'   serials_dist = function(x) 3,
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
simulate_tree <- function(nchains, statistic = c("size", "length"),
                          offspring_dist, stat_max = Inf,
                          r0_reduction = 0,
                          serials_dist, t0 = 0,
                          tf = Inf, ...) {
  statistic <- match.arg(statistic)

  check_nchains_valid(nchains = nchains)

  # check that offspring is properly specified
  check_offspring_valid(offspring_dist)

  # Check that the r0_reduction is well specified
  checkmate::assert_number(
    r0_reduction,
    lower = 0,
    upper = 1
  )

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)

  # Gather offspring distribution parameters
  pars <- list(...)

  # Prepare interventions if specified
  if (r0_reduction > 0) {
    pars <- intvn_scale_r0(
      r0_reduction = r0_reduction,
      offspring_dist = offspring_dist,
      pars_list = pars
    )
  }

  if (!missing(serials_dist)) {
    check_serial_valid(serials_dist)
  } else if (!missing(tf)) {
    stop("If `tf` is specified, `serials_dist` must be specified too.")
  }

  # Initialisations
  stat_track <- rep(1, nchains) # track length or size (depending on `statistic`) #nolint
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

  if (!missing(serials_dist)) {
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
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )

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
      if (!missing(serials_dist)) {
        times <- rep(times, next_gen) + serials_dist(sum(n_offspring))
        current_min_time <- unname(tapply(times, indices, min))
        new_df$time <- times
      }
      tree_df <- rbind(tree_df, new_df)
    }

    ## only continue to simulate chains that have offspring and aren't of
    ## the specified maximum size/length
    sim <- which(n_offspring > 0 & stat_track < stat_max)
    if (length(sim) > 0) {
      if (!missing(serials_dist)) {
        ## only continue to simulate chains that don't go beyond tf
        sim <- intersect(sim, unique(indices)[current_min_time < tf])
      }
      if (!missing(serials_dist)) {
        times <- times[indices %in% sim]
      }
      ancestor_ids <- ids[indices %in% sim]
    }
  }

  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  # sort by sim_id and ancestor
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$ancestor), ]

  structure(
    tree_df,
    chains = nchains,
    chain_type = "chains_tree",
    rownames = NULL,
    track_pop = FALSE,
    class = c("epichains", "data.frame")
  )
}



#' Simulate transmission chains sizes/lengths
#'
#' @inheritParams simulate_tree
#' @inheritParams intvn_scale_r0
#' @param stat_max A cut off for the chain statistic (size/length) being
#' computed. Results above the specified value, are set to `Inf`.
#' @inheritSection simulate_tree Calculating chain sizes and lengths
#' @inheritSection simulate_tree The serial interval (`serials_dist`)
#' @author James M. Azam, Sebastian Funk
#' @seealso
#' * [simulate_tree()] for simulating transmission trees from an
#'   initial number of infections.
#' * [simulate_tree_from_pop()] for simulating transmission trees from a
#'   susceptible or partially immune population.
#' @examples
#' simulate_summary(
#'   nchains = 10,
#'   statistic = "size",
#'   offspring_dist = "pois",
#'   stat_max = 10,
#'   lambda = 2
#' )
#' @export
simulate_summary <- function(nchains, statistic = c("size", "length"),
                             offspring_dist,
                             r0_reduction = 0,
                             stat_max = Inf, ...) {
  statistic <- match.arg(statistic)

  check_nchains_valid(nchains = nchains)

  # check that offspring is properly specified
  check_offspring_valid(offspring_dist)

  # Check that the r0_reduction is well specified
  checkmate::assert_number(
    r0_reduction,
    lower = 0,
    upper = 1
  )

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)

  # Gather offspring distribution parameters
  pars <- list(...)

  # Prepare interventions if specified
  if (r0_reduction > 0) {
    pars <- intvn_scale_r0(
      r0_reduction = r0_reduction,
      offspring_dist = offspring_dist,
      pars_list = pars
    )
  }

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
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )

    ## only continue to simulate chains that offspring and aren't of
    ## stat_max size/length
    sim <- which(n_offspring > 0 & stat_track < stat_max)
  }

  stat_track[stat_track >= stat_max] <- Inf

  structure(
    stat_track,
    chain_type = "chains_summary",
    statistic = statistic,
    chains = nchains,
    class = c("epichains", class(stat_track))
  )
}

#' Simulate transmission trees from a susceptible or partially immune
#' population
#'
#' @inheritParams simulate_tree
#' @inheritParams intvn_scale_r0
#' @param pop The susceptible population size.
#' @param offspring_dist Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers). Only supports "pois" and "nbinom".
#' @param offspring_mean The average number of secondary cases for each case.
#' Same as \eqn{R_0}.
#' @param offspring_disp The dispersion parameter of the number of
#' secondary cases. Ignored if \code{offspring == "pois"}. Must be > 1 to
#' avoid division by 0 when calculating the size. See details and
#'  \code{?rnbinom} for details on the parameterisation in Ecology.
#' @param initial_immune The number of initial immunes in the population.
#' Must be less than `pop` - 1.
#' @param t0 Start time; Defaults to 0.
#' @param tf End time; Defaults to `Inf`.
#' @return An `<epichains>` object, which is basically a `<data.frame>` with
#' columns `sim_id` (a unique ID within each simulation for each individual
#' of the chain), `ancestor` (the ID of the ancestor of each individual),
#' `generation`, and `time` (of infection).
#' @details
#' # Offspring distributions
#' Currently, `offspring_dist` only supports "pois" & "nbinom".
#' Internally, the respective truncated poisson and negative binomial
#' distributions are used to avoid the situation where there are more cases
#' than susceptibles at any point.
#'
#' The poisson model has mean, lambda, parametrised as:
#' \deqn{{\sf lambda} = \dfrac{{\sf offspring\_mean} \times ({\sf pop} -
#' {\sf initial\_immune} - 1)}{{\sf pop}}}
#'
#' The negative binomial model, has mean, mu, parametrised as:
#' \deqn{{\sf mu} = \dfrac{{\sf offspring\_mean} \times ({\sf pop} -
#' {\sf initial\_immune} - 1)}{{\sf pop}},}
#' and dispersion, size, parametrised as:
#' \deqn{{\sf size} = \dfrac{{\sf mu}}{{\sf offspring\_disp} - 1}.}
#' This is why `offspring_disp` must be greater than 1.
#'
#' # Differences with `simulate_tree()`
#' `simulate_tree_from_pop()` has a couple of key differences from
#' `simulate_tree()`:
#'  * the maximal chain statistic is limited by `pop` instead of
#'  `stat_max` (in `simulate_tree()`),
#'  * `offspring_dist` can only handle "pois" and "nbinom".
#' @inheritSection simulate_tree The serial interval (`serials_dist`)
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
#'   offspring_mean = 0.5,
#'   serials_dist = function(x) 3
#' )
#'
#' # Simulate with negative binomial offspring
#' simulate_tree_from_pop(
#'   pop = 100,
#'   offspring_dist = "nbinom",
#'   offspring_mean = 0.5,
#'   offspring_disp = 1.1,
#'   serials_dist = function(x) 3
#' )
#' @export
simulate_tree_from_pop <- function(pop,
                                   offspring_dist = c("pois", "nbinom"),
                                   r0_reduction = 0,
                                   offspring_disp,
                                   serials_dist,
                                   initial_immune = 0,
                                   t0 = 0,
                                   tf = Inf) {
  offspring_dist <- match.arg(offspring_dist)

  # Check that the r0_reduction is well specified
  checkmate::assert_number(
    r0_reduction,
    lower = 0,
    upper = 1
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  # Prepare interventions if specified
  if (r0_reduction > 0) {
    pars <- intvn_scale_r0(
      r0_reduction = r0_reduction,
      offspring_dist = offspring_dist,
      pars_list = pars
    )
  }

    ## using a right truncated poisson distribution
    ## to avoid more cases than susceptibles
    offspring_func <- function(n, susc) {
      truncdist::rtrunc(
        n,
        spec = "pois",
        lambda = offspring_mean * susc / pop,
        b = susc
      )
    }
  } else if (offspring_dist == "nbinom") {
    if (missing(offspring_disp)) {
      stop(sprintf("%s", "'offspring_disp' must be specified."))
    } else if (offspring_disp <= 1) { ## dispersion coefficient
      stop(sprintf(
        "%s %s %s",
        "Offspring distribution 'nbinom' requires",
        "argument 'offspring_disp' > 1.",
        "Use 'pois' if there is no overdispersion."
      ))
    }
    ## get distribution params from mean and dispersion
    offspring_func <- function(n, susc) {
      ## get distribution params from mean and dispersion
      ## see ?rnbinom for parameter definition
      new_mn <- offspring_mean * susc / pop ## apply susceptibility
      size <- new_mn / (offspring_disp - 1)

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
    ancestor = NA_integer_,
    generation = 1L,
    time = t0,
    offspring_generated = FALSE # tracks simulation and dropped afterwards
  )

  susc <- pop - initial_immune - 1L
  t <- t0

  ## continue if any unsimulated chains have t <= tf
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
      ## draw serial times
      new_times <- serials_dist(n_offspring)

      if (any(new_times < 0)) {
        stop("Serial interval must be >= 0.")
      }

      new_df <- data.frame(
        sim_id = current_max_id + seq_len(n_offspring),
        ancestor = id_parent,
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

  # sort by sim_id and ancestor
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$ancestor), ]
  tree_df$offspring_generated <- NULL

  structure(
    tree_df,
    chain_type = "chains_tree",
    rownames = NULL,
    track_pop = TRUE,
    class = c("epichains", "data.frame")
  )
}
