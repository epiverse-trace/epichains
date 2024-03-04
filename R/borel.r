##' Density of the Borel distribution
##'
##' @param x Vector of quantiles; integer.
##' @param mu mu parameter (the poisson mean); non-negative.
##' @param log Logical; if TRUE, probabilities p are given as log(p).
##' @return Probability mass.
##' @author Sebastian Funk
##' @export
##' @examples
##' set.seed(32)
##' dborel(1:5, 1)
dborel <- function(x, mu, log = FALSE) {
  checkmate::assert_numeric(
    x, lower = 1, upper = Inf
  )
  checkmate::assert_number(
    mu, lower = 0, finite = TRUE, na.ok = FALSE
  )

  ld <- -mu * x + (x - 1) * log(mu * x) - lgamma(x + 1)
  if (!log) ld <- exp(ld)
  return(ld)
}

##' Generate random numbers from the Borel distribution
##'
##' Random numbers are generated by simulating from a Poisson branching process
##' @param n Number of random variates to generate.
##' @inheritParams dborel
##' @param infinite Any number to treat as infinite; simulations will be
##' stopped if this number is reached
##' @return Vector of random numbers
##' @author Sebastian Funk
##' @export
##' @examples
##' set.seed(32)
##' rborel(5, 1)
rborel <- function(n, mu, infinite = Inf) {
  checkmate::assert_number(
    n, lower = 1, finite = TRUE, na.ok = FALSE
  )
  checkmate::assert_number(
    mu, lower = 0, finite = TRUE, na.ok = FALSE
  )
  # Run simulations
  out <- simulate_summary(
    index_cases = n,
    offspring_dist = rpois,
    statistic = "size",
    stat_max = infinite,
    lambda = mu
  )
  out <- as.numeric(out)
  return(out)
}

##' Generate random numbers from a Gamma-Borel mixture distribution
##'
##' @inheritParams rborel
##' @importFrom stats rgamma rpois
##' @param size the dispersion parameter (often called \code{k} in ecological
##'   applications)
##' @param prob probability of success (in the parameterisation with
##'   \code{prob}, see also \code{\link[stats]{NegBinomial}})
##' @param mu mean parameter
##' @return Numeric vector of random numbers
##' @author Sebastian Funk
##' @export
rgborel <- function(n, size, prob, mu, infinite = Inf) {
  ## This function was introduced to support estimating likelihoods using a
  ## Gamma-Borel mixture distribution. It is not actually called (it only needs)
  ## to exist and could be a dummy. However, the function is here included with
  ## its "correct implementation" for documentation/clarity purposes, as well as
  ## for simulations.
  checkmate::assert_number(
    size, finite = TRUE, lower = 0
  )
  if (!missing(prob)) {
    checkmate::assert_number(
      prob, lower = 0, upper = 1
    )
  }
  if (!missing(mu)) {
    checkmate::assert_number(
      mu, finite = TRUE, lower = 0
    )
  }
  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  ## first, sample from gamma
  x <- rgamma(n, shape = size, rate = size / mu)
  ## then, sample from borel
  return(vapply(x, rborel, n = 1, infinite = infinite, FUN.VALUE = numeric(1)))
}
