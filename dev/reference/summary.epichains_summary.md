# Summary method for `<epichains_summary>` class

Summary method for `<epichains_summary>` class

## Usage

``` r
# S3 method for class 'epichains_summary'
summary(object, ...)
```

## Arguments

- object:

  An `<epichains_summary>` object.

- ...:

  Not used.

## Value

A list of chain summaries. The list contains the following elements:

- `n_chains`: the number of chains simulated.

- `max_stat`: the maximum chain statistic (size/length) achieved by the
  chains.

- `min_stat`: the minimum chain statistic (size/length) achieved by the
  chains.

## Author

James M. Azam

## Examples

``` r
# Using a Poisson offspring distribution and simulating from an infinite
# population up to chain size 10.
set.seed(32)
chain_stats <- simulate_chain_stats(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  lambda = 2
)
summary(chain_stats)
#> $n_chains
#> [1] 10
#> 
#> $max_stat
#> [1] Inf
#> 
#> $min_stat
#> [1] Inf
#> 
```
