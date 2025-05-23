#' Estimate the log-likelihood/likelihood for observed branching processes
#'
#' @inheritParams .offspring_ll
#' @inheritParams simulate_chain_stats
#' @param chains Vector of chain summaries (sizes/lengths). Can be a
#' `<numeric>` vector or an object of class `<epichains>` or
#' `<epichains_summary>` (obtained from [simulate_chains()] or
#' [simulate_chain_stats()]). See examples below.
#' @param nsim_obs Number of simulations to be used to approximate the
#' log-likelihood/likelihood if `obs_prob < 1` (imperfect observation). If
#' `obs_prob == 1`, this argument is ignored.
#' @param obs_prob Observation probability. A number (probability) between 0
#' and 1. A value greater than 0 but less 1 implies imperfect observation and
#' simulations will be used to approximate the (log)likelihood. This will
#' also require specifying `nsim_obs`. In the simulation, the observation
#' process is assumed to be constant.
#' @param stat_threshold A stopping criterion for individual chain simulations;
#' a positive number coercible to integer. When any chain's cumulative statistic
#' reaches or surpasses `stat_threshold`, that chain ends. It also serves as a
#' censoring limit so that results above the specified value, are set to `Inf`.
#' Defaults to `Inf`. NOTE: For objects of class `<epichains>` or
#' `<epichains_summary>`, the `stat_threshold` used in the simulation is
#' extracted and used here so if this argument is specified, it is ignored and
#' a warning is thrown.
#' @param log Should the log-likelihoods be transformed to
#' likelihoods? Logical. Defaults to `TRUE`.
#' @param exclude A vector of indices of the sizes/lengths to exclude from the
#' log-likelihood calculation.
#' @param individual Logical; If TRUE, a vector of individual
#' log-likelihood/likelihood contributions will be returned rather than the
#' sum/product.
#' @return
#' If \code{log = TRUE}
#'
#' * A joint log-likelihood (sum of individual log-likelihoods), if
#' \code{individual == FALSE} (default) and \code{obs_prob == 1} (default), or
#' * A list of individual log-likelihoods, if \code{individual == TRUE} and
#' \code{obs_prob == 1} (default), or
#' * A list of individual log-likelihoods (same length as `nsim_obs`), if
#' \code{individual == TRUE} and \code{0 <= obs_prob < 1}, or
#' * A vector of joint log-likelihoods (same length as `nsim_obs`), if
#' individual == FALSE and \code{0 <= obs_prob < 1} (imperfect observation).
#'
#' If \code{log = FALSE}, the same structure of outputs as above are returned,
#' except that likelihoods, instead of log-likelihoods, are calculated in all
#' cases. Moreover, the joint likelihoods are the product, instead of the sum,
#' of the individual likelihoods.
#' @author Sebastian Funk, James M. Azam
#' @examples
#' # example of observed chain sizes
#' set.seed(121)
#' # randomly generate 20 chains of size 1 to 10
#' chain_sizes <- sample(1:10, 20, replace = TRUE)
#' likelihood(
#'   chains = chain_sizes,
#'   statistic = "size",
#'   offspring_dist = rpois,
#'   nsim_obs = 100,
#'   lambda = 0.5
#' )
#' # Example using an <epichains> object
#' set.seed(32)
#' epichains_obj_eg <- simulate_chains(
#'   n_chains = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   stat_threshold = 10,
#'   generation_time = function(n) rep(3, n),
#'   mu = 2,
#'   size = 0.2
#' )
#'
#' epichains_obj_eg_lik <- likelihood(
#'   chains = epichains_obj_eg,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   mu = 2,
#'   size = 0.2,
#'   stat_threshold = 10
#' )
#' epichains_obj_eg_lik
#'
#' # Example using a <epichains_summary> object
#' set.seed(32)
#' epichains_summary_eg <- simulate_chain_stats(
#'   n_chains = 10,
#'   pop = 100,
#'   percent_immune = 0,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   stat_threshold = 10,
#'   mu = 2,
#'   size = 0.2
#' )
#' epichains_summary_eg_lik <- likelihood(
#'   chains = epichains_summary_eg,
#'   statistic = "size",
#'   offspring_dist = rnbinom,
#'   mu = 2,
#'   size = 0.2
#' )
#' epichains_summary_eg_lik
#' @export
# nolint start: cyclocomp_linter
likelihood <- function(chains, statistic = c("size", "length"), offspring_dist,
                       nsim_obs, obs_prob = 1, stat_threshold = Inf, log = TRUE,
                       exclude = NULL, individual = FALSE, ...) {
  statistic <- match.arg(statistic)

  ## Input checking
  checkmate::assert(
    checkmate::check_numeric(
      chains,
      lower = 0, upper = Inf, any.missing = FALSE
    ),
    checkmate::check_class(
      chains, "epichains"
    ),
    checkmate::check_class(
      chains, "epichains_summary"
    )
  )
  # check that arguments related to the statistic are valid
  .check_statistic_args(
    statistic,
    stat_threshold
  )
  .check_offspring_func_valid(offspring_dist)
  checkmate::assert_number(
    obs_prob,
    lower = 0, upper = 1, finite = TRUE, na.ok = FALSE
  )
  checkmate::assert_logical(
    log,
    any.missing = FALSE, all.missing = FALSE, len = 1
  )
  checkmate::assert_logical(
    individual,
    any.missing = FALSE, all.missing = FALSE, len = 1
  )
  checkmate::assert_numeric(
    exclude,
    null.ok = TRUE
  )
  # likelihood is designed to work with numeric objects to <epichains> objects
  # need to be coerced to <epichains_summary> objects (numeric vector under
  # the hood) for the likelihood calculation
  if (.is_epichains(chains)) {
    chains <- summary(chains)
  }

  # Logic:
  # * If the object is an <epichains>/<epichains_summary>, we'll give
  # preference to the stat_threshold used in the simulation.
  if (.is_epichains_summary(chains)) {
    stat_threshold_from_sim <- attr(chains, "stat_threshold")
    if (!missing(stat_threshold)) {
      warning(
        "`stat_threshold` specified but will be ignored. ",
        "Using `stat_threshold` = ",
        stat_threshold_from_sim,
        " as used in the simulation.",
        call. = FALSE
      )
    }
    stat_threshold <- stat_threshold_from_sim
  }

  if (is.finite(stat_threshold)) {
    # censor the chains to be at most stat_threshold
    chains <- pmin(chains, stat_threshold)
  } else if (any(is.infinite(chains)) && !.is_epichains_summary(chains)) {
    # Developer note: <epichains_summary> objects also pass the`is.numeric()`
    # check, so we need to check if the object is not an <epichains_summary>
    # object.
    # Numeric chains can only contain Inf values based on human error/decision
    # to censor it with infinite values, so we ask the user to fix that.
    stop(
      "`chains` must be censored with a finite value. ",
      "Replace the `Inf` values with a finite value.",
      call. = FALSE
    )
  }

  # Apply the observation process
  if (obs_prob < 1) {
    if (missing(nsim_obs)) {
      stop("'nsim_obs' must be specified if 'obs_prob' is < 1")
    } else {
      checkmate::assert_integerish(
        nsim_obs,
        lower = 1
      )
    }

    statistic_func <- .get_statistic_func(statistic)

    stat_rep_list <- replicate(nsim_obs, pmin(
      statistic_func(
        length(chains),
        chains, obs_prob
      ),
      stat_threshold
    ), simplify = FALSE)
    stat_rep_vect <- unlist(stat_rep_list)
    if (!is.finite(stat_threshold)) {
      stat_threshold <- max(stat_rep_vect) + 1
    }
  } else {
    chains[chains >= stat_threshold] <- stat_threshold
    stat_rep_vect <- chains
    stat_rep_list <- list(chains)
  }

  ## Determine for which sizes to calculate the log-likelihood
  ## (for true chain size)
  if (any(stat_rep_vect == stat_threshold)) {
    calc_sizes <- seq_len(stat_threshold - 1)
  } else {
    calc_sizes <- unique(c(stat_rep_vect, exclude))
  }

  ## get log-likelihood function as given by offspring_dist and statistic
  likelihoods <- vector(mode = "numeric")

  func_name <- tail(as.character(substitute(offspring_dist)), 1)
  dist_name <- substr(func_name, 2, nchar(func_name))
  ll_func <- paste(dist_name, statistic, "ll", sep = "_")

  pars <- as.list(unlist(list(...))) ## converts vectors to lists

  ## calculate log-likelihoods
  possible_internal_func <- paste0(".", ll_func)
  if (exists(possible_internal_func,
    where = asNamespace("epichains"),
    mode = "function"
  )
  ) {
    func <- get(possible_internal_func)
    likelihoods[calc_sizes] <- do.call(func, c(list(x = calc_sizes), pars))
  } else {
    likelihoods[calc_sizes] <-
      do.call(
        .offspring_ll,
        c(
          list(
            x = calc_sizes,
            offspring_dist = offspring_dist,
            statistic = statistic,
            stat_threshold = stat_threshold
          ),
          pars
        )
      )
  }

  ## assign probabilities to stat_threshold outbreak sizes
  if (any(stat_rep_vect == stat_threshold)) {
    likelihoods[stat_threshold] <- .complementary_logprob(likelihoods)
  }

  if (!missing(exclude)) {
    likelihoods <- likelihoods - log(-expm1(sum(likelihoods[exclude])))
    likelihoods[exclude] <- -Inf
    stat_rep_list <- lapply(stat_rep_list, function(y) {
      y[!(y %in% exclude)]
    })
  }

  ## assign likelihoods
  chains_likelihood <- lapply(stat_rep_list, function(sx) {
    likelihoods[sx[!(sx %in% exclude)]]
  })

  ## if individual == FALSE, return the joint log-likelihood
  ## (sum of the log-likelihoods)
  if (!individual) {
    chains_likelihood <- vapply(chains_likelihood, sum, 0)
  }

  ## transform log-likelihoods into likelihoods if required
  if (!log) {
    if (individual) {
      chains_likelihood <- lapply(chains_likelihood, exp)
    } else {
      chains_likelihood <- exp(chains_likelihood)
    }
  }

  return(chains_likelihood)
}
# nolint end
