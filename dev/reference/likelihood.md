# Estimate the log-likelihood/likelihood for observed branching processes

Estimate the log-likelihood/likelihood for observed branching processes

## Usage

``` r
likelihood(
  chains,
  statistic = c("size", "length"),
  offspring_dist,
  nsim_obs,
  obs_prob = 1,
  stat_threshold = Inf,
  log = TRUE,
  exclude = NULL,
  individual = FALSE,
  ...
)
```

## Arguments

- chains:

  Vector of chain summaries (sizes/lengths). Can be a `<numeric>` vector
  or an object of class `<epichains>` or `<epichains_summary>` (obtained
  from
  [`simulate_chains()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chains.md)
  or
  [`simulate_chain_stats()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chain_stats.md)).
  See examples below.

- statistic:

  The chain statistic to track as the stopping criteria for each chain
  being simulated when `stat_threshold` is not `Inf`; A `<string>`. It
  can be one of:

  - "size": the total number of cases produced by a chain before it goes
    extinct.

  - "length": the total number of generations reached by a chain before
    it goes extinct.

- offspring_dist:

  Offspring distribution: a `<function>` like the ones provided by R to
  generate random numbers from given distributions (e.g.,
  [`rpois`](https://rdrr.io/r/stats/Poisson.html) for Poisson). More
  specifically, the function needs to accept at least one argument, `n`,
  which is the number of random numbers to generate. It can accept
  further arguments, which will be passed on to the random number
  generating functions. Examples that can be provided here are `rpois`
  for Poisson distributed offspring, `rnbinom` for negative binomial
  offspring, or custom functions.

- nsim_obs:

  Number of simulations to be used to approximate the
  log-likelihood/likelihood if `obs_prob < 1` (imperfect observation).
  If `obs_prob == 1`, this argument is ignored.

- obs_prob:

  Observation probability. A number (probability) between 0 and 1. A
  value greater than 0 but less 1 implies imperfect observation and
  simulations will be used to approximate the (log)likelihood. This will
  also require specifying `nsim_obs`. In the simulation, the observation
  process is assumed to be constant.

- stat_threshold:

  A stopping criterion for individual chain simulations; a positive
  number coercible to integer. When any chain's cumulative statistic
  reaches or surpasses `stat_threshold`, that chain ends. It also serves
  as a censoring limit so that results above the specified value, are
  set to `Inf`. Defaults to `Inf`. NOTE: For objects of class
  `<epichains>` or `<epichains_summary>`, the `stat_threshold` used in
  the simulation is extracted and used here so if this argument is
  specified, it is ignored and a warning is thrown.

- log:

  Should the log-likelihoods be transformed to likelihoods? Logical.
  Defaults to `TRUE`.

- exclude:

  A vector of indices of the sizes/lengths to exclude from the
  log-likelihood calculation.

- individual:

  Logical; If TRUE, a vector of individual log-likelihood/likelihood
  contributions will be returned rather than the sum/product.

- ...:

  any parameters to pass to
  [`simulate_chain_stats`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chain_stats.md)

## Value

If `log = TRUE`

- A joint log-likelihood (sum of individual log-likelihoods), if
  `individual == FALSE` (default) and `obs_prob == 1` (default), or

- A list of individual log-likelihoods, if `individual == TRUE` and
  `obs_prob == 1` (default), or

- A list of individual log-likelihoods (same length as `nsim_obs`), if
  `individual == TRUE` and `0 <= obs_prob < 1`, or

- A vector of joint log-likelihoods (same length as `nsim_obs`), if
  individual == FALSE and `0 <= obs_prob < 1` (imperfect observation).

If `log = FALSE`, the same structure of outputs as above are returned,
except that likelihoods, instead of log-likelihoods, are calculated in
all cases. Moreover, the joint likelihoods are the product, instead of
the sum, of the individual likelihoods.

## Author

Sebastian Funk, James M. Azam

## Examples

``` r
# example of observed chain sizes
set.seed(121)
# randomly generate 20 chains of size 1 to 10
chain_sizes <- sample(1:10, 20, replace = TRUE)
likelihood(
  chains = chain_sizes,
  statistic = "size",
  offspring_dist = rpois,
  nsim_obs = 100,
  lambda = 0.5
)
#> [1] -67.82879
# Example using an <epichains> object
set.seed(32)
epichains_obj_eg <- simulate_chains(
  n_chains = 10,
  pop = 100,
  percent_immune = 0,
  statistic = "size",
  offspring_dist = rnbinom,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  mu = 2,
  size = 0.2
)

epichains_obj_eg_lik <- likelihood(
  chains = epichains_obj_eg,
  statistic = "size",
  offspring_dist = rnbinom,
  mu = 2,
  size = 0.2,
  stat_threshold = 10
)
#> Warning: `stat_threshold` specified but will be ignored. Using `stat_threshold` = 10 as used in the simulation.
epichains_obj_eg_lik
#> [1] -18.97756

# Example using a <epichains_summary> object
set.seed(32)
epichains_summary_eg <- simulate_chain_stats(
  n_chains = 10,
  pop = 100,
  percent_immune = 0,
  statistic = "size",
  offspring_dist = rnbinom,
  stat_threshold = 10,
  mu = 2,
  size = 0.2
)
epichains_summary_eg_lik <- likelihood(
  chains = epichains_summary_eg,
  statistic = "size",
  offspring_dist = rnbinom,
  mu = 2,
  size = 0.2
)
epichains_summary_eg_lik
#> [1] -18.97756
```
