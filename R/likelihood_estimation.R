#' Estimate the (log) likelihood for observed branching processes
#'
#' @param x vector of sizes or lengths of transmission chains
#' @param stat statistic given as \code{x} ("size" or "length" of chains)
#' @param obs_prob observation probability (assumed constant)
#' @param infinite any chains of this size/length will be treated as infinite
#' @param exclude any sizes/lengths to exclude from the likelihood calculation
#' @param individual if TRUE, a vector of individual log-likelihood
#' contributions will be returned rather than the sum
#' @param nsim_obs number of simulations if the likelihood is to be
#'   approximated for imperfect observations
#' @param ... parameters for the offspring distribution
#' @return likelihood, or vector of likelihoods (if \code{obs_prob} < 1), or
#'  a list of individual likelihood contributions (if \code{individual=TRUE})
#' @inheritParams chain_sim
#' @seealso pois_size_ll, nbinom_size_ll, gborel_size_ll, pois_length_ll,
#'   geom_length_ll, offspring_ll
#' @author Sebastian Funk
#' @export
#' @examples
#' chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
#' chain_ll(chain_sizes, "pois", "size", lambda = 0.5)
chain_ll <- function(x, offspring, stat = c("size", "length"), obs_prob = 1,
                     infinite = Inf, exclude = NULL, individual = FALSE,
                     nsim_obs, ...) {
  stat <- match.arg(stat)

  ## checks
  if (!is.character(offspring)) {
    stop("Object passed as 'offspring' is not a character string.")
  }
  if (obs_prob <= 0 || obs_prob > 1) stop("'obs_prob' must be within (0,1]")
  if (obs_prob < 1) {
    if (missing(nsim_obs)) {
      stop("'nsim_obs' must be specified if 'obs_prob' is <1")
    }
    if (stat == "size") {
      sample_func <- rbinom_size
    } else if (stat == "length") {
      sample_func <- rgen_length
    }
    sampled_x <-
      replicate(nsim_obs, pmin(sample_func(length(x), x, obs_prob),
                               infinite), simplify = FALSE)
    size_x <- unlist(sampled_x)
    if (!is.finite(infinite)) infinite <- max(size_x) + 1
  } else {
    x[x >= infinite] <- infinite
    size_x <- x
    sampled_x <- list(x)
  }

  ## determine for which sizes to calculate the likelihood (for true chain size)
  if (any(size_x == infinite)) {
    calc_sizes <- seq_len(infinite - 1)
  } else {
    calc_sizes <- unique(c(size_x, exclude))
  }

  ## get likelihood function as given by `offspring` and `stat``
  likelihoods <- vector(mode = "numeric")
  ll_func <- paste(offspring, stat, "ll", sep = "_")
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
          x = calc_sizes, offspring = offspring,
          stat = stat, infinite = infinite
        ), pars)
      )
  }

  ## assign probabilities to infinite outbreak sizes
  if (any(size_x == infinite)) {
    likelihoods[infinite] <- complementary_logprob(likelihoods)
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
