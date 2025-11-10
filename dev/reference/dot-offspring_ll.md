# Log-likelihood of the summary (size/length) of chains with generic offspring distribution

The log-likelihoods are calculated with a crude approximation using
simulated chain summaries by linearly approximating any missing values
in the empirical cumulative distribution function (ecdf).

## Usage

``` r
.offspring_ll(x, offspring_dist, statistic, nsim_offspring = 100, ...)
```

## Arguments

- x:

  A numeric vector of chain summaries (sizes/lengths).

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

- statistic:

  The chain statistic to track as the stopping criteria for each chain
  being simulated when `stat_threshold` is not `Inf`; A `<string>`. It
  can be one of:

  - "size": the total number of cases produced by a chain before it goes
    extinct.

  - "length": the total number of generations reached by a chain before
    it goes extinct.

- nsim_offspring:

  Number of simulations of the offspring distribution for approximating
  the distribution of the chain statistic summary (size/length)

- ...:

  any parameters to pass to
  [`simulate_chain_stats`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chain_stats.md)

## Value

A numeric vector of log-likelihood values.

## Author

Sebastian Funk James M. Azam
