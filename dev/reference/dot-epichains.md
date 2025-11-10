# Create an `<epichains>` object

[`epichains()`](https://epiverse-trace.github.io/epichains/dev/reference/epichains-package.md)
constructs an `<epichains>` object, which is inherently an
`<data.frame>` object that stores some of the inputs passed to the
[`simulate_chains()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chains.md)
and the simulated output. The stored attributes are useful for
downstream analyses and reproducibility. This function checks the
validity of the object created to ensure it has the right columns and
column types.

An `<epichains>` object contains a `<data.frame>` of the simulated
outbreak with ids for each infector and infectee, generation, and
optionally, time, the number of chains simulated, the chain statistic
that was tracked, and whether the susceptible population was tracked.

## Usage

``` r
.epichains(
  sim_df,
  n_chains,
  offspring_dist,
  track_pop,
  statistic = c("size", "length"),
  stat_threshold = Inf
)
```

## Arguments

- sim_df:

  a `<data.frame>` containing at least columns for "infectee_id",
  "infector_id", and "generation". Also has optional columns for "time",
  and "chain_id".

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

- track_pop:

  Was the susceptible population tracked? Logical.

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
  reaches or surpasses `stat_threshold`, that chain ends. Defaults to
  `Inf`. For example, if `statistic = "size"` and `stat_threshold = 10`,
  then any chain that produces 10 or more cases will stop. Note that
  setting `stat_threshold` does not guarantee that all chains will stop
  at the same value.

## Value

An `<epichains>` object.

## Author

James M. Azam
