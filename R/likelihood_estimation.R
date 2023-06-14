#' Estimate the (log) likelihood for observed branching processes
#'
#' @param chains_observed Vector of sizes/lengths of transmission chains.
#' @param chain_statistic Statistic given as \code{chains_observed}
#' ("size" or "length" of chains).
#' @param offspring_sampler Offspring distribution: a character string
#' corresponding to the R distribution function (e.g., "pois" for Poisson,
#' where \code{\link{rpois}} is the R function to generate Poisson random
#' numbers).
#' @param nsim_obs Number of simulations if the likelihood is to be
#' approximated for imperfect observations.
#' @param log_trans Logical; Should the results be log-transformed? (Defaults
#' to TRUE).
#' @param obs_prob Observation probability (assumed constant)
#' @param chain_stat_max Any chains of this size/length will be
#' treated as infinite.
#' @param exclude A vector of indices of the sizes/lengths to exclude from the
#' likelihood calculation.
#' @param individual If TRUE, a vector of individual (log)likelihood
#' contributions will be returned rather than the sum.
#' @param ... Parameters for the offspring distribution.
#' @return
#' * A log-likelihood, if \code{log_trans = TRUE} (the default)
#' * A vector of log-likelihoods, if \code{log_trans = TRUE} (the default) and
#' \code{obs_prob < 1}, or
#' * A list of individual log-likelihood contributions, if
#' \code{log_trans = TRUE} (the default) and \code{individual = TRUE}.
#' else raw likelihoods, or vector of likelihoods
#' @seealso offspring_ll, pois_size_ll, nbinom_size_ll, gborel_size_ll,
#' pois_length_ll, geom_length_ll.
#' @author Sebastian Funk
#' @examples
#' # example of observed chain sizes
#' chain_sizes <- c(1, 1, 4, 7)
#' estimate_likelihood(chains_observed = chain_sizes, chain_statistic = "size",
#'  offspring_sampler = "pois", nsim_obs = 100, lambda = 0.5)
#' @export
estimate_likelihood <- function(chains_observed,
                                chain_statistic = c("size", "length"),
                                offspring_sampler,
                                nsim_obs,
                                log_trans = TRUE,
                                obs_prob = 1, chain_stat_max = Inf,
                                exclude = NULL, individual = FALSE, ...) {
  chain_statistic <- match.arg(chain_statistic)

  ## checks
  if (!is.character(offspring_sampler)) {
    stop("Object passed as 'offspring_sampler' is not a character string.")
  }
  if (obs_prob <= 0 || obs_prob > 1) stop("'obs_prob' must be within (0,1]")
  if (obs_prob < 1) {
    if (missing(nsim_obs)) {
      stop("'nsim_obs' must be specified if 'obs_prob' is < 1")
    }
    if (stat == "size") {
      sample_func <- rbinom_size
    } else if (stat == "length") {
      sample_func <- rgen_length
    }
    sampled_x <-
      replicate(nsim_obs, pmin(sample_func(length(x), x, obs_prob),
                                           ),
                               chain_stat_max), simplify = FALSE)
    size_x <- unlist(sampled_x)
    if (!is.finite(chain_stat_max)) chain_stat_max <- max(size_x) + 1
  } else {
    chains_observed[chains_observed >= chain_stat_max] <- chain_stat_max
    size_x <- x
    sampled_x <- list(x)
  }

  ## determine for which sizes to calculate the likelihood (for true chain size)
  if (any(size_x == chain_stat_max)) {
    calc_sizes <- seq_len(chain_stat_max - 1)
  } else {
    calc_sizes <- unique(c(size_x, exclude))
  }

  ## get likelihood function as given by offspring_sampler and chain_statistic
  likelihoods <- vector(mode = "numeric")
  ll_func <- paste(offspring_sampler, chain_statistic, "ll", sep = "_")
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
          chains_observed = calc_sizes, offspring_sampler = offspring_sampler,
          chain_statistic = chain_statistic, chain_stat_max = chain_stat_max
        ), pars)
      )
  }

  ## assign probabilities to chain_stat_max outbreak sizes
  if (any(size_x == chain_stat_max)) {
    likelihoods[chain_stat_max] <- complementary_logprob(likelihoods)
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

  if (!individual) chains_likelihood <- vapply(chains_likelihood, sum, 0)

  return(chains_likelihood)
}
