# Create an `<epichains_summary>` object

`epichains_summary()` constructs an `<epichains_summary>` object.

An `<epichains_summary>` object is a `<vector>` of the simulated chain
sizes or lengths. It also stores information on the number of chains
simulated, and the statistic that was tracked.

## Usage

``` r
.epichains_summary(
  chains_summary,
  n_chains,
  offspring_dist,
  statistic = c("size", "length"),
  stat_threshold = Inf
)
```

## Arguments

- chains_summary:

  A numeric `<vector>` of chain sizes and lengths.

- n_chains:

  Number of chains to simulate.

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

- stat_threshold:

  A stopping criterion for individual chain simulations; a positive
  number coercible to integer. When any chain's cumulative statistic
  reaches or surpasses `stat_threshold`, that chain ends. It also serves
  as a censoring limit so that results above the specified value, are
  set to `Inf`. Defaults to `Inf`.

## Value

An `<epichains_summary>` object.

## Author

James M. Azam
