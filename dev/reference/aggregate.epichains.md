# Aggregate cases in `<epichains>` objects by "generation" or "time", if present

This function provides a quick way to create a time series of cases over
generation or time (if `generation_time` was specified) from simulated
`<epichains>` objects.

## Usage

``` r
# S3 method for class 'epichains'
aggregate(x, by = c("time", "generation"), ...)
```

## Arguments

- x:

  An `<epichains>` object.

- by:

  The variable to aggregate by; A character string with options "time"
  and "generation".

- ...:

  Not used.

## Value

A `<data.frame>` object of cases by `by`.

## Author

James M. Azam

## Examples

``` r
set.seed(32)
chains <- simulate_chains(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  lambda = 2
)
chains
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

# Aggregate cases per time
cases_per_time <- aggregate(chains, by = "time")
head(cases_per_time)
#>   time cases
#> 1    0    10
#> 2    3    25
#> 3    6    61
#> 4    9    33

# Aggregate cases per generation
cases_per_gen <- aggregate(chains, by = "generation")
head(cases_per_gen)
#>   generation cases
#> 1          1    10
#> 2          2    25
#> 3          3    61
#> 4          4    33
```
