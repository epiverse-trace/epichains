# Density of the Borel distribution

Density of the Borel distribution

## Usage

``` r
dborel(x, mu, log = FALSE)
```

## Arguments

- x:

  A numeric vector of quantiles.

- mu:

  A non-negative number for the poisson mean.

- log:

  Logical; if TRUE, probabilities p are given as log(p).

## Value

A numeric vector of the borel probability density.

## Author

Sebastian Funk James M. Azam

## Examples

``` r
set.seed(32)
dborel(1:5, 1)
#> [1] 0.36787944 0.13533528 0.07468060 0.04884170 0.03509347
```
