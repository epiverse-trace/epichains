# `head` and `tail` method for `<epichains>` class

`head` and `tail` method for `<epichains>` class

## Usage

``` r
# S3 method for class 'epichains'
head(x, ...)

# S3 method for class 'epichains'
tail(x, ...)
```

## Arguments

- x:

  An `<epichains>` object.

- ...:

  Further arguments passed to or from other methods.

## Value

An object of class `<data.frame>`.

## Details

This returns the top rows of an `<epichains>` object, starting from the
first known infectors.

To view the full output, use `as.data.frame(<object_name>)`.

## Author

James M. Azam

## Examples

``` r
set.seed(32)
chains_pois_offspring <- simulate_chains(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  lambda = 2
)
head(chains_pois_offspring)
#>    chain infector infectee generation time
#> 11     1        1        2          2    3
#> 12     1        1        3          2    3
#> 13     2        1        2          2    3
#> 14     2        1        3          2    3
#> 15     3        1        2          2    3
#> 16     3        1        3          2    3
set.seed(32)
chains_pois_offspring <- simulate_chains(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  lambda = 2
)
tail(chains_pois_offspring)
#>     chain infector infectee generation time
#> 124    10        3        6          4    9
#> 125    10        4        7          4    9
#> 126    10        4        8          4    9
#> 127    10        4        9          4    9
#> 128    10        4       10          4    9
#> 129    10        4       11          4    9
```
