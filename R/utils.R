#' Calculates the complementary log-probability
#'
#' Given x and norm, this calculates log(1-sum(exp(x)))
#' @param x A numeric vector of log-probabilities. Must be negative.
#' @return A numeric value of the complementary log-probability.
#' @author Sebastian Funk
#' @keywords internal
.complementary_logprob <- function(x) {
  checkmate::assert_numeric(
    x,
    lower = -Inf,
    upper = 0
  )

  out <- tryCatch(log1p(-sum(exp(x))), error = function(e) -Inf)
  return(out)
}

#' Samples size (the number of trials) of a binomial distribution
#'
#' Samples the size parameter from the binomial distribution with fixed x
#' (number of successes) and p (success probability)
#' @param n The number of samples to generate. Must be a positive integer.
#' @param x The number of successes. Must be a positive integer.
#' @param prob The probability of success. A numeric between 0 and 1.
#' @return A numeric vector of the sampled sizes.
#' @author Sebastian Funk
#' @keywords internal
.rbinom_size <- function(n, x, prob) {
  out <- x + stats::rnbinom(n, x + 1, prob)
  return(out)
}

#' Samples chain lengths with given observation probabilities
#'
#' Samples the length of a transmission chain where each individual element is
#' observed with binomial probability with parameters n (number of successes)
#' and p (success probability)
#' @param n The number of samples to generate. Must be a positive integer.
#' @param x A numeric vector of the observed chain lengths.
#' @param prob The probability of observation. A numeric between 0 and 1.
#' @return A numeric vector of the sampled chain lengths.
#' @author Sebastian Funk
#' @keywords internal
.rgen_length <- function(n, x, prob) {
  out <- x +
    ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1) +
    ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1)
  return(out)
}
