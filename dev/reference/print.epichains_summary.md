# Print an `<epichains_summary>` object

Prints a summary of the `<epichains_summary>` object. In particular, it
prints the number of chains simulated, and the range of the statistic,
represented as the maximum (`max_stat`) and minimum (`min_stat`). If the
minimum or maximum is infinite, it is represented as `>= stat_threshold`
where `stat_threshold` is the value of the censoring limit. See
`?epichains_summary()` for the definition of `stat_threshold`.

## Usage

``` r
# S3 method for class 'epichains_summary'
print(x, ...)
```

## Arguments

- x:

  An `<epichains_summary>` object.

- ...:

  Not used.

## Value

Invisibly returns an `<epichains_summary>`. Called for side-effects.

## Author

James M. Azam

## Examples

``` r
# Using a Poisson offspring distribution and simulating from an infinite
# population up to chain size 10.
set.seed(32)
chain_summary_print_eg <- simulate_chain_stats(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  lambda = 2
)
chain_summary_print_eg # Print the object
#> `epichains_summary` object 
#> 
#>  [1] Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf
#> 
#>  Simulated sizes: 
#> 
#> Max: >=10
#> Min: >=10
```
