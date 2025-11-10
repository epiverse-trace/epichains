# Log-likelihood of the size of chains with Negative-Binomial offspring distribution

Log-likelihood of the size of chains with Negative-Binomial offspring
distribution

## Usage

``` r
.nbinom_size_ll(x, size, prob, mu)
```

## Arguments

- x:

  A numeric vector of chain sizes.

- size:

  The dispersion parameter (often called `k` in ecological
  applications); A positive number.

- prob:

  Probability of success (in the parameterisation with `prob`, see also
  [`NegBinomial`](https://rdrr.io/r/stats/NegBinomial.html)); A number
  between 0 and 1.

- mu:

  Mean; A positive number.

## Value

A numeric vector of log-likelihood values.

## Author

Sebastian Funk James M. Azam
