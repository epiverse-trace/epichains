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

#' Likelihood for the outcome of a branching process
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
