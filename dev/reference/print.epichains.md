# Print an `<epichains>` object

Print an `<epichains>` object

## Usage

``` r
# S3 method for class 'epichains'
print(x, ...)
```

## Arguments

- x:

  An `<epichains>` object.

- ...:

  Other parameters passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

Invisibly returns an `<epichains>`. Called for side-effects.

## Author

James M. Azam

## Examples

``` r
# Using a Poisson offspring distribution and simulating from an infinite
# population up to chain size 10.
set.seed(32)
chains_pois_offspring <- simulate_chains(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  lambda = 2
)
chains_pois_offspring # Print the object
#> `<epichains>` object
#> 
#> < epichains head (from first known infector) >
#> 
#>    chain infector infectee generation time
#> 11     1        1        2          2    3
#> 12     1        1        3          2    3
#> 13     2        1        2          2    3
#> 14     2        1        3          2    3
#> 15     3        1        2          2    3
#> 16     3        1        3          2    3
#> 
#> 
#> Number of chains: 10
#> Number of infectors (known): 9
#> Number of generations: 4
#> Use `as.data.frame(<object_name>)` to view the full output in the console.
```
