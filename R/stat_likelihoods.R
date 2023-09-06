#' Log-likelihood of the size of chains with Poisson offspring distribution
#'
#' @param x vector of sizes
#' @param lambda rate of the Poisson distribution
#' @return log-likelihood values
#' @author Sebastian Funk
#' @keywords internal
pois_size_ll <- function(x, lambda) {
  (x - 1) * log(lambda) - lambda * x + (x - 2) * log(x) - lgamma(x)
}

#' Log-likelihood of the size of chains with Negative-Binomial offspring
#' distribution
#'
#' @param x vector of sizes
#' @param size the dispersion parameter (often called \code{k} in ecological
#'   applications)
#' @param prob probability of success (in the parameterisation with
#'   \code{prob}, see also \code{\link[stats]{NegBinomial}})
#' @param mu mean parameter
#' @return log-likelihood values
#' @author Sebastian Funk
#' @keywords internal
nbinom_size_ll <- function(x, size, prob, mu) {
  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  lgamma(size * x + (x - 1)) - (lgamma(size * x) + lgamma(x + 1)) +
    (x - 1) * log(mu / size) -
    (size * x + (x - 1)) * log(1 + mu / size)
}

#' Log-likelihood of the size of chains with gamma-Borel offspring distribution
#'
#' @param x vector of sizes
#' @param size the dispersion parameter (often called \code{k} in ecological
#'   applications)
#' @param prob probability of success (in the parameterisation with
#'   \code{prob}, see also \code{\link[stats]{NegBinomial}})
#' @param mu mean parameter
#' @return log-likelihood values
#' @author Sebastian Funk
#' @keywords internal
gborel_size_ll <- function(x, size, prob, mu) {
  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  lgamma(size + x - 1) -
    (lgamma(x + 1) + lgamma(size)) - size * log(mu / size) +
    (x - 1) * log(x) - (size + x - 1) * log(x + size / mu)
}

#' Log-likelihood of the length of chains with Poisson offspring distribution
#'
#' @param x vector of sizes
#' @param lambda rate of the Poisson distribution
#' @return log-likelihood values
#' @author Sebastian Funk
#' @keywords internal
pois_length_ll <- function(x, lambda) {
  ## iterated exponential function
  arg <- exp(lambda * exp(-lambda))
  itex <- 1
  for (i in seq_len(max(x))) itex <- c(itex, arg^itex[i])

  Gk <- c(0, exp(-lambda) * itex) ## set G_{0}=1

  log(Gk[x + 1] - Gk[x])
}

#' Log-likelihood of the length of chains with geometric offspring distribution
#'
#' @param x vector of sizes
#' @param prob probability of the geometric distribution with mean
#' \code{1/prob}
#' @return log-likelihood values
#' @author Sebastian Funk
#' @keywords internal
geom_length_ll <- function(x, prob) {
  lambda <- 1 / prob
  GkmGkm1 <- (1 - lambda^(x)) / (1 - lambda^(x + 1)) -
    (1 - lambda^(x - 1)) / (1 - lambda^(x))

  log(GkmGkm1)
}

#' Log-likelihood of the summary (size/length) of chains with generic offspring
#' distribution
#'
#' The log-likelihoods are calculated with a crude approximation using simulated
#' chain summaries by linearly approximating any missing values in the empirical
#' cumulative distribution function (ecdf).
#' @inheritParams likelihood
#' @inheritParams simulate_summary
#' @param chains Vector of chain summaries (sizes/lengths)
#' @param nsim_offspring Number of simulations of the offspring distribution
#' for approximating the distribution of the chain statistic summary
#' (size/length)
#' @param ... any parameters to pass to \code{\link{simulate_summary}}
#' @return log-likelihood values
#' @author Sebastian Funk
#' @export
offspring_ll <- function(chains, offspring_dist, statistic,
                         nsim_offspring = 100, ...) {
  # Simulate the chains
  chains <- simulate_summary(
    nsim_offspring, offspring_dist,
    statistic, ...
  )

  # Compute the empirical Cumulative Distribution Function of the
  # simulated chains
  chains_empirical_cdf <- stats::ecdf(chains)

  # Perform a lagged linear interpolation of the points
  acdf <-
    diff(c(0, stats::approx(
      unique(chains), chains_empirical_cdf(unique(chains)),
      seq_len(max(chains[is.finite(chains)]))
    )$y))
  lik <- acdf[chains]
  lik[is.na(lik)] <- 0
  out <- log(lik)
  return(out)
}
