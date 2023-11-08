##' Density of the Borel distribution
##'
##' @param x Vector of integers.
##' @param mu mu parameter.
##' @param log Logical; if TRUE, probabilities p are given as log(p).
##' @return Probability mass.
##' @author Sebastian Funk
##' @export
dborel <- function(x, mu, log = FALSE) {
  if (x < 1) stop("'x' must be greater than 0")
  ld <- -mu * x + (x - 1) * log(mu * x) - lgamma(x + 1)
  if (!log) ld <- exp(ld)
  return(ld)
}

##' Generate random numbers from the Borel distribution
##'
##' Random numbers are generated by simulating from a Poisson branching process
##' @param n Number of random variates to generate.
##' @param mu mu parameter.
##' @param infinite Any number to treat as infinite; simulations will be
##' stopped if this number is reached
##' @return Vector of random numbers
##' @author Sebastian Funk
##' @export
rborel <- function(n, mu, infinite = Inf) {
  simulate_summary(
    nchains = n,
    offspring_dist = "pois",
    statistic = "size",
    stat_max = infinite,
    lambda = mu
  )
}