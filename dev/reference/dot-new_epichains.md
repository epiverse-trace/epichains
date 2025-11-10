# Construct an `<epichains>` object

`new_epichains()` constructs an `<epichains>` object from a supplied
`<data.frame>` and extra attributes passed as individual arguments. It
is meant to be lazy and performant, by creating the object without
checking the arguments for correctness. It is not safe to call
`new_epichains()` on its own as is called within
[`epichains()`](https://epiverse-trace.github.io/epichains/dev/reference/epichains-package.md)
after the arguments have been checked. To create an `<epichains>`
object, use
[`epichains()`](https://epiverse-trace.github.io/epichains/dev/reference/epichains-package.md).

## Usage

``` r
.new_epichains(
  sim_df,
  n_chains,
  statistic,
  offspring_dist,
  stat_threshold,
  track_pop
)
```

## Arguments

- sim_df:

  a `<data.frame>` containing at least columns for "infectee_id",
  "infector_id", and "generation". Also has optional columns for "time",
  and "chain_id".

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

- track_pop:

  Was the susceptible population tracked? Logical.

## Author

James M. Azam
