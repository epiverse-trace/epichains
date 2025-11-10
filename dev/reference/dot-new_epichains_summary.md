# Construct a `<epichains_summary>` object

`new_epichains_summary()` constructs an `<epichains_summary>` object
from a supplied `<vector>` of chain sizes or lengths. It also stores
extra attributes passed as individual arguments.

`new_epichains_summary()` is meant to be lazy and performant, by
creating the object without checking the arguments for correctness. It
is not safe to call `new_epichains_summary()` on its own as is called
within `epichains_summary()` after the arguments have been checked. To
create a new `<epichains_summary>` object safely, use
`epichains_summary()`.

## Usage

``` r
.new_epichains_summary(
  chains_summary,
  n_chains,
  statistic,
  offspring_dist,
  stat_threshold
)
```

## Arguments

- chains_summary:

  A numeric `<vector>` of chain sizes and lengths.

- n_chains:

  Number of chains to simulate.

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

- stat_threshold:

  A stopping criterion for individual chain simulations; a positive
  number coercible to integer. When any chain's cumulative statistic
  reaches or surpasses `stat_threshold`, that chain ends. It also serves
  as a censoring limit so that results above the specified value, are
  set to `Inf`. Defaults to `Inf`.

## Author

James M. Azam
