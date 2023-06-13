#' Likelihood of the size of chains with Poisson offspring distribution
#'
#' @param x vector of sizes
#' @param lambda rate of the Poisson distribution
#' @return log-likelihood values
#' @author Sebastian Funk
#' @keywords internal
pois_size_ll <- function(x, lambda) {
  (x - 1) * log(lambda) - lambda * x + (x - 2) * log(x) - lgamma(x)
}

#' Likelihood of the size of chains with Negative-Binomial offspring
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

#' Likelihood of the size of chains with gamma-Borel offspring distribution
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

#' Likelihood of the length of chains with Poisson offspring distribution
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

#' Likelihood of the length of chains with geometric offspring distribution
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

#' Likelihood of the length of chains with generic offspring distribution
#'
#' The likelihoods are calculated with a crude approximation using simulated
#'   chains by linearly approximating any missing values in the empirical
#'   cumulative distribution function (ecdf).
#' @param x vector of sizes
#' @param nsim_offspring number of simulations of the offspring distribution
#'   for approximation the size/length distribution
#' @param ... any parameters to pass to \code{\link{chain_sim}}
#' @return log-likelihood values
#' @author Sebastian Funk
#' @inheritParams chain_ll
#' @inheritParams chain_sim
#' @keywords internal
offspring_ll <- function(x, offspring, stat, nsim_offspring = 100, ...) {
  dist <- chain_sim(nsim_offspring, offspring, stat, ...)

  ## linear approximation
  f <- stats::ecdf(dist)
  acdf <-
    diff(c(0, stats::approx(
      unique(dist), f(unique(dist)),
      seq_len(max(dist[is.finite(dist)]))
    )$y))
  lik <- acdf[x]
  lik[is.na(lik)] <- 0
  log(lik)
}
