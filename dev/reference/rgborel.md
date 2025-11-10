# Generate random numbers from a Gamma-Borel mixture distribution

Generate random numbers from a Gamma-Borel mixture distribution

## Usage

``` r
rgborel(n, size, prob, mu, censor_at = Inf)
```

## Arguments

- n:

  Number of random variates to generate.

- size:

  The dispersion parameter (often called `k` in ecological
  applications); A positive number.

- prob:

  Probability of success (in the parameterisation with `prob`, see also
  [`NegBinomial`](https://rdrr.io/r/stats/NegBinomial.html)); A number
  between 0 and 1.

- mu:

  Mean; A positive number.

- censor_at:

  A stopping criterion; `<numeric>`. Defaults to `Inf`. A value above
  which the simulation ends and the random number is set to `Inf` (as a
  form of censoring).
  [`rborel()`](https://epiverse-trace.github.io/epichains/dev/reference/rborel.md)
  simulates chain sizes using
  [`simulate_chain_stats()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chain_stats.md)
  with a Poisson offspring distribution, so if `mu >= 1`, the simulation
  could proceed unendingly. This parameter is used to prevent this.

## Value

Numeric vector of random numbers

## Author

Sebastian Funk James M. Azam

## Examples

``` r
set.seed(32)
rgborel(n = 5, size = 0.3, mu = 1, censor_at = 5)
#> [1]   1   1 Inf Inf   1
```
