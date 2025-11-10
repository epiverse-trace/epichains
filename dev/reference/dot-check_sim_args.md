# Check inputs to `simulate_chains()` and `simulate_summary()`

Check inputs to
[`simulate_chains()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chains.md)
and `simulate_summary()`

## Usage

``` r
.check_sim_args(
  n_chains,
  statistic,
  offspring_dist,
  stat_threshold,
  pop,
  percent_immune
)
```

## Arguments

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
  reaches or surpasses `stat_threshold`, that chain ends. Defaults to
  `Inf`. For example, if `statistic = "size"` and `stat_threshold = 10`,
  then any chain that produces 10 or more cases will stop. Note that
  setting `stat_threshold` does not guarantee that all chains will stop
  at the same value.

- pop:

  Population size; An `<Integer>`. Used alongside `percent_immune` to
  define the susceptible population. Defaults to `Inf`.

- percent_immune:

  Percent of the population immune to infection at the start of the
  simulation; A `<numeric>` between 0 and 1. Used alongside `pop` to
  initialise the susceptible population. Defaults to 0.

## Value

NULL; called for side effects
