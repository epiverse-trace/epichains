# Summary method for `<epichains>` class

This calculates the chain statistic (size/length) for the simulated
chains and returns an object with the same information as that returned
by an equivalent
[`simulate_chain_stats()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chain_stats.md)
call.

## Usage

``` r
# S3 method for class 'epichains'
summary(object, ...)
```

## Arguments

- object:

  An `<epichains>` object.

- ...:

  Not used.

## Value

An `<epichains_summary>` object containing the chain summary statistics
as follows:

- "size": the total number of offspring produced by a chain before it
  goes extinct.

- "length": the number of generations achieved by a chain before it goes
  extinct.

## Author

James M. Azam

## Examples

``` r
# Using a negative binomial offspring distribution and simulating from a
# finite population up to chain size 10.
set.seed(32)
sim_chains_nbinom <- simulate_chains(
  n_chains = 10,
  pop = 100,
  percent_immune = 0,
  statistic = "size",
  offspring_dist = rnbinom,
  stat_threshold = 10,
  mu = 2,
  size = 0.2
)
# Summarise the simulated chains
sim_chains_nbinom_summary <- summary(sim_chains_nbinom)
sim_chains_nbinom_summary
#> `epichains_summary` object 
#> 
#>  [1]   3 Inf Inf   5   6 Inf   1   1   1   1
#> 
#>  Simulated sizes: 
#> 
#> Max: >=10
#> Min: 1

# Same results can be obtained using `simulate_chain_stats()`
set.seed(32)
sim_summary_nbinom <- simulate_chain_stats(
  n_chains = 10,
  pop = 100,
  percent_immune = 0,
  statistic = "size",
  offspring_dist = rnbinom,
  stat_threshold = 10,
  mu = 2,
  size = 0.2
)
sim_summary_nbinom
#> `epichains_summary` object 
#> 
#>  [1]   3 Inf Inf   5   6 Inf   1   1   1   1
#> 
#>  Simulated sizes: 
#> 
#> Max: >=10
#> Min: 1

# Check that the results are the same
setequal(sim_chains_nbinom_summary, sim_summary_nbinom)
#> [1] TRUE
```
