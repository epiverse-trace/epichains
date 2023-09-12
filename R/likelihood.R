#' Estimate the log-likelihood/likelihood for observed branching processes
#'
#' @inheritParams offspring_ll
#' @inheritParams simulate_summary
#' @param nsim_obs Number of simulations if the log-likelihood/likelihood is to
#' be approximated for imperfect observations.
#' @param log Logical; Should the log-likelihoods be transformed to
#' likelihoods? (Defaults to TRUE).
#' @param obs_prob Observation probability (assumed constant)
#' @param exclude A vector of indices of the sizes/lengths to exclude from the
#' log-likelihood calculation.
#' @param individual If TRUE, a vector of individual log-likelihood/likelihood
#' contributions will be returned rather than the sum/product.
#' @return
#' If log = TRUE:
#'
#' * A joint log-likelihood (sum of individual log-likelihoods), if
#' \code{individual == FALSE} (default) and \code{obs_prob = 1} (default), or
#' * A list of individual log-likelihoods, if \code{individual == TRUE} and
#' \code{obs_prob = 1} (default), or
#' * A list of individual log-likelihoods (same length as `nsim_obs`), if
#' \code{individual == TRUE} and \code{0 <= obs_prob < 1}, or
#' * A vector of joint log-likelihoods (same length as `nsim_obs`), if
#' individual == FALSE and \code{0 <= obs_prob < 1} (imperfect observation).
#'
#' If \code{log = FALSE}, likelihoods, instead of log-likelihoods, are returned
#' and the joint likelihoods are the product, instead of the sum, of the
#' individual likelihoods.
#' @seealso offspring_ll(), pois_size_ll(), nbinom_size_ll(), gborel_size_ll(),
#' pois_length_ll(), geom_length_ll()
#' @author Sebastian Funk
#' @examples
#' # example of observed chain sizes
#' set.seed(121)
#' # randomly generate 20 chains of size 1 to 10
#' chain_sizes <- sample(1:10, 20, replace = TRUE)
#' likelihood(
#'   chains = chain_sizes, statistic = "size",
#'   offspring_dist = "pois", nsim_obs = 100, lambda = 0.5
#' )
#' @export
likelihood <- function(chains, statistic = c("size", "length"), offspring_dist,
                       nsim_obs, log = TRUE, obs_prob = 1, stat_max = Inf,
                       exclude = NULL, individual = FALSE, ...) {
  statistic <- match.arg(statistic)

  ## checks
  check_offspring_valid(offspring_dist)
  checkmate::assert_logical(
    log,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1
  )

  if (obs_prob <= 0 || obs_prob > 1) {
    stop("'obs_prob' is a probability and must be between 0 and 1 inclusive")
  }
  if (obs_prob < 1) {
    if (missing(nsim_obs)) {
      stop("'nsim_obs' must be specified if 'obs_prob' is < 1")
    }

    statistic_func <- get_statistic_func(statistic)

    stat_rep_list <- replicate(nsim_obs, pmin(
      statistic_func(
        length(chains),
        chains, obs_prob
      ),
      stat_max
    ), simplify = FALSE)
    stat_rep_vect <- unlist(stat_rep_list)
    if (!is.finite(stat_max)) {
    stat_max <- max(stat_rep_vect) + 1
    }
  } else {
    chains[chains >= stat_max] <- stat_max
    stat_rep_vect <- chains
    stat_rep_list <- list(chains)
  }

  ## determine for which sizes to calculate the log-likelihood
  ## (for true chain size)
  if (any(stat_rep_vect == stat_max)) {
    calc_sizes <- seq_len(stat_max - 1)
  } else {
    calc_sizes <- unique(c(stat_rep_vect, exclude))
  }

  ## get log-likelihood function as given by offspring_dist and statistic
  likelihoods <- vector(mode = "numeric")
  ll_func <- construct_offspring_ll_name(offspring_dist, statistic)
  pars <- as.list(unlist(list(...))) ## converts vectors to lists

  ## calculate log-likelihoods
  if (exists(ll_func, where = asNamespace("epichains"), mode = "function")) {
    func <- get(ll_func)
    likelihoods[calc_sizes] <- do.call(func, c(list(x = calc_sizes), pars))
  } else {
    likelihoods[calc_sizes] <-
      do.call(
        offspring_ll,
        c(
          list(
            chains = calc_sizes,
            offspring_dist = offspring_dist,
            statistic = statistic,
            stat_max = stat_max
          ),
          pars
        )
      )
  }

  ## assign probabilities to stat_max outbreak sizes
  if (any(stat_rep_vect == stat_max)) {
    likelihoods[stat_max] <- complementary_logprob(likelihoods)
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

  ## transform log-likelihoods into likelihoods if required
  if (isFALSE(log)) {
    chains_likelihood <- lapply(chains_likelihood, exp)
  }

  ## if individual == FALSE, return the joint log-likelihood
  ## (sum of the log-likelihoods), if log == TRUE, else
  ## multiply the likelihoods
  if (isFALSE(individual)) {
    if (isTRUE(log)) {
      chains_likelihood <- vapply(chains_likelihood, sum, 0)
    } else {
      chains_likelihood <- vapply(chains_likelihood, prod, 0)
    }
  }

  return(chains_likelihood)
}
