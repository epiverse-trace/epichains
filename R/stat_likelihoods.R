#' Log-likelihood functions for transmission chain statistics
#'
#' @description
#' These functions compute the log-likelihoods of observing transmission chain
#' summary statistics (sizes or lengths) under specified offspring
#' distributions.
#'
#' The analytical log-likelihood functions are named
#' `.<offspring_dist>_<statistic>_ll()`, for example `.pois_size_ll()` and
#' `.geom_length_ll()`. The function `.offspring_ll()` provides a generic
#' numerical approximation using simulations, for cases where no analytical
#' solution exists.
#'
#' @param x A numeric vector of chain sizes or lengths.
#' @return A numeric vector of log-likelihood values.
#' @name chain_ll
#' @author Sebastian Funk, James M. Azam
#' @keywords internal
NULL

#' @rdname chain_ll
#' @param lambda The rate of the Poisson distribution; A single numeric value.
#' @keywords internal
.pois_size_ll <- function(x, lambda) {
  checkmate::assert_numeric(
    x,
    lower = 0,
    any.missing = FALSE
  )
  checkmate::assert_number(
    lambda,
    finite = TRUE,
    lower = 0
  )

  out <- (x - 1) * log(lambda) - lambda * x + (x - 2) * log(x) - lgamma(x)
  return(out)
}

#' @rdname chain_ll
#' @inheritParams rgborel
#' @keywords internal
.nbinom_size_ll <- function(x, size, prob, mu) {
  checkmate::assert_numeric(
    x,
    lower = 0,
    any.missing = FALSE
  )
  checkmate::assert_number(
    size,
    finite = TRUE,
    lower = 0
  )
  if (!missing(prob)) {
    checkmate::assert_number(
      prob,
      lower = 0,
      upper = 1
    )
  }
  if (!missing(mu)) {
    checkmate::assert_number(
      mu,
      finite = TRUE,
      lower = 0
    )
  }
  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  out <- lgamma(size * x + (x - 1)) - (lgamma(size * x) + lgamma(x + 1)) +
    (x - 1) * log(mu / size) -
    (size * x + (x - 1)) * log(1 + mu / size)
  return(out)
}

#' @rdname chain_ll
#' @inheritParams rgborel
#' @keywords internal
.gborel_size_ll <- function(x, size, prob, mu) {
  checkmate::assert_numeric(
    x,
    lower = 0,
    any.missing = FALSE
  )
  checkmate::assert_number(
    size,
    finite = TRUE,
    lower = 0
  )
  if (!missing(prob)) {
    checkmate::assert_number(
      prob,
      lower = 0,
      upper = 1
    )
  }
  if (!missing(mu)) {
    checkmate::assert_number(
      mu,
      finite = TRUE,
      lower = 0
    )
  }

  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  out <- lgamma(size + x - 1) -
    (lgamma(x + 1) + lgamma(size)) - size * log(mu / size) +
    (x - 1) * log(x) - (size + x - 1) * log(x + size / mu)
  return(out)
}

#' @rdname chain_ll
#' @keywords internal
.pois_length_ll <- function(x, lambda) {
  checkmate::assert_numeric(
    x,
    lower = 0,
    any.missing = FALSE
  )
  checkmate::assert_number(
    lambda,
    finite = TRUE,
    lower = 0
  )

  ## iterated exponential function
  arg <- exp(lambda * exp(-lambda))
  itex <- 1
  for (i in seq_len(max(x))) itex <- c(itex, arg^itex[i])

  Gk <- c(0, exp(-lambda) * itex) ## set G_{0}=1

  out <- log(Gk[x + 1] - Gk[x])
  return(out)
}

#' @rdname chain_ll
#' @param prob The probability of the geometric distribution with mean
#' `1/prob`. A single numeric value between 0 and 1.
#' @keywords internal
.geom_length_ll <- function(x, prob) {
  checkmate::assert_numeric(
    x,
    lower = 0, any.missing = FALSE
  )
  checkmate::assert_number(
    prob,
    lower = 0,
    upper = 1
  )

  lambda <- 1 / prob
  GkmGkm1 <- (1 - lambda^(x)) / (1 - lambda^(x + 1)) -
    (1 - lambda^(x - 1)) / (1 - lambda^(x))

  out <- log(GkmGkm1)
  return(out)
}

#' @rdname chain_ll
#' @description
#' `.offspring_ll()` calculates log-likelihoods using a crude approximation
#' with simulated chain summaries by linearly approximating any missing values
#' in the empirical cumulative distribution function (ecdf). It is used when
#' no analytical solution exists.
#' @inheritParams likelihood
#' @inheritParams simulate_chain_stats
#' @param nsim_offspring Number of simulations of the offspring distribution
#' for approximating the distribution of the chain statistic summary
#' (size/length).
#' @param ... Any parameters to pass to [simulate_chain_stats()].
#' @keywords internal
.offspring_ll <- function(x, offspring_dist, statistic,
                          nsim_offspring = 100, ...) {
  # Input checking
  checkmate::assert_numeric(
    x,
    lower = 0,
    any.missing = FALSE
  )
  # Remaining checks are done in simulate_chain_stats()
  # Simulate the chains
  dist <- simulate_chain_stats(
    n_chains = nsim_offspring,
    offspring_dist = offspring_dist,
    statistic = statistic,
    ...
  )

  # Compute the empirical Cumulative Distribution Function of the
  # simulated chains
  chains_empirical_cdf <- stats::ecdf(dist)

  # Perform a lagged linear interpolation of the points
  acdf <- diff(
    c(
      0,
      stats::approx(
        unique(dist),
        chains_empirical_cdf(unique(dist)),
        seq_len(
          max(dist[is.finite(dist)])
        )
      )$y
    )
  )
  lik <- acdf[x]
  lik[is.na(lik)] <- 0
  out <- log(lik)
  return(out)
}
