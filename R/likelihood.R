#' Estimate the (log) likelihood for observed branching processes
#'
#' @inheritParams simulate_summary
#' @param chains Vector of sizes/lengths of transmission chains.
#' @param nsim_obs Number of simulations if the likelihood is to be
#' approximated for imperfect observations.
#' @param log Logical; Should the results be log-transformed? (Defaults
#' to TRUE).
#' @param obs_prob Observation probability (assumed constant)
#' @param exclude A vector of indices of the sizes/lengths to exclude from the
#' likelihood calculation.
#' @param individual If TRUE, a vector of individual (log)likelihood
#' contributions will be returned rather than the sum.
#' @param ... Parameters for the offspring distribution.
#' @return
#' * A vector of log-likelihoods, if \code{log = TRUE} (the default) and
#' \code{obs_prob < 1}, or
#' * A list of individual log-likelihood contributions, if
#' \code{log = TRUE} (the default) and \code{individual = TRUE}.
#' else raw likelihoods, or vector of likelihoods
#' @seealso offspring_ll, pois_size_ll, nbinom_size_ll, gborel_size_ll,
#' pois_length_ll, geom_length_ll.
#' @author Sebastian Funk
#' @examples
#' # example of observed chain sizes
#' chain_sizes <- c(1, 1, 4, 7)
#' likelihood(
#'   chains = chain_sizes, statistic = "size",
#'   offspring_dist = "pois", nsim_obs = 100, lambda = 0.5
#' )
#' @export
likelihood <- function(chains, statistic = c("size", "length"), offspring_dist,
                       nsim_obs = 100, log = TRUE, obs_prob = 1, stat_max = Inf,
                       exclude = NULL, individual = FALSE, ...) {
  statistic <- match.arg(statistic)

  ## checks
  check_offspring_valid(offspring_dist)

  if (obs_prob <= 0 || obs_prob > 1) stop("'obs_prob' must be within (0,1]")
  if (obs_prob < 1) {
    if (missing(nsim_obs)) {
      stop("'nsim_obs' must be specified if 'obs_prob' is < 1")
    }

    sample_func <- get_statistic_func(statistic)

    sampled_x <- replicate(nsim_obs, pmin(
      sample_func(
        length(chains),
        chains, obs_prob
      ),
      stat_max
    ), simplify = FALSE)
    size_x <- unlist(sampled_x)
    if (!is.finite(stat_max)) {
      stat_max <- max(size_x) + 1
    }
  } else {
    chains[chains >= stat_max] <- stat_max
    size_x <- chains
    sampled_x <- list(chains)
  }

  ## determine for which sizes to calculate the likelihood (for true chain size)
  if (any(size_x == stat_max)) {
    calc_sizes <- seq_len(stat_max - 1)
  } else {
    calc_sizes <- unique(c(size_x, exclude))
  }

  ## get likelihood function as given by offspring_dist and statistic
  likelihoods <- vector(mode = "numeric")
  ll_func <- construct_offspring_ll_name(offspring_dist, statistic)
  pars <- as.list(unlist(list(...))) ## converts vectors to lists

  ## calculate likelihoods
  if (exists(ll_func, where = asNamespace("epichains"), mode = "function")) {
    func <- get(ll_func)
    likelihoods[calc_sizes] <- do.call(func, c(list(x = calc_sizes), pars))
  } else {
    likelihoods[calc_sizes] <-
      do.call(
        offspring_ll,
        c(list(
          chains = calc_sizes, offspring_dist = offspring_dist,
          statistic = statistic, stat_max = stat_max,
          log = log
        ), pars)
      )
  }

  ## assign probabilities to stat_max outbreak sizes
  if (any(size_x == stat_max)) {
    likelihoods[stat_max] <- complementary_logprob(likelihoods)
  }

  if (!missing(exclude)) {
    likelihoods <- likelihoods - log(-expm1(sum(likelihoods[exclude])))
    likelihoods[exclude] <- -Inf

    sampled_x <- lapply(sampled_x, function(y) {
      y[!(y %in% exclude)]
    })
  }

  ## assign likelihoods
  chains_likelihood <- lapply(sampled_x, function(sx) {
    likelihoods[sx[!(sx %in% exclude)]]
  })

  if (!individual) {
    chains_likelihood <- vapply(chains_likelihood, sum, 0)
  }

  return(chains_likelihood)
}
