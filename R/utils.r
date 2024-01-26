#' Calculates the complementary log-probability
#'
#' Given x and norm, this calculates log(1-sum(exp(x)))
#' @param x log-probabilities
#' @return value
#' @author Sebastian Funk
#' @keywords internal
complementary_logprob <- function(x) {
  checkmate::assert_numeric(
    x, lower = -Inf, upper = 0
  )

  out <- tryCatch(log1p(-sum(exp(x))), error = function(e) -Inf)
  return(out)
}

#' Samples size (the number of trials) of a binomial distribution
#'
#' Samples the size parameter from the binomial distribution with fixed x
#' (number of successes) and p (success probability)
#' @param n number of samples to generate
#' @param x number of successes
#' @param prob probability of success
#' @return sampled sizes
#' @author Sebastian Funk
#' @keywords internal
rbinom_size <- function(n, x, prob) {
  out <- x + stats::rnbinom(n, x + 1, prob)
  return(out)
}

#' Samples chain lengths with given observation probabilities
#'
#' Samples the length of a transmission chain where each individual element is
#' observed with binomial probability with parameters n (number of successes)
#' and p (success probability)
#' @param n number of samples to generate
#' @param x observed chain lengths
#' @param prob probability of observation
#' @return sampled lengths
#' @author Sebastian Funk
#' @keywords internal
rgen_length <- function(n, x, prob) {
  out <- x +
    ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1) +
    ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1)
  return(out)
}

#' Negative binomial random numbers parametrized
#' in terms of mean and dispersion coefficient
#' @param n number of samples to draw
#' @param mn mean of distribution; Must be > 0.
#' @param disp dispersion coefficient (var/mean); Must be > 1.
#' @return vector containing the random numbers
#' @author Flavio Finger
#' @export
#' @examples
#' rnbinom_mean_disp(n = 5, mn = 4, disp = 2)
rnbinom_mean_disp <- function(n, mn, disp) {
  checkmate::assert_number(
    n, lower = 1, finite = TRUE, na.ok = FALSE
  )
  checkmate::assert_number(
    disp, lower = 1, finite = TRUE, na.ok = FALSE
  )
  checkmate::assert_number(
    mn, lower = 1E-100, finite = TRUE, na.ok = FALSE
  )

  size <- mn / (disp - 1)
  out <- stats::rnbinom(n, size = size, mu = mn)
  return(out)
}
