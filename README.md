
# *epichains*: Methods for analysing the size and length of transmission chains from branching process models

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/epiverse-trace/epichains)
[![R-CMD-check](https://github.com/epiverse-trace/epichains/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/epichains/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiverse-trace/epichains/branch/main/graphs/badge.svg)](https://codecov.io/github/epiverse-trace/epichains)
![GitHub
contributors](https://img.shields.io/github/contributors/epiverse-trace/epichains)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

*epichains* is an R package to simulate and analyse the size and length
of branching processes with a given offspring distribution. These models
are often used in infectious disease epidemiology, where the chains
represent chains of transmission, and the offspring distribution
represents the distribution of secondary infections caused by an
infected individual.

# Installation

The latest development version of the *epichains* package can be
installed via

``` r
pak::pkg_install("epiverse-trace/epichains")
```

To load the package, use

``` r
library("epichains")
```

# Quick start

At the heart of the package are the `chain_ll()` and `chain_sim()`
functions.

## Calculating log-likelihoods

The `chain_ll()` function calculates the log-likelihood of a
distribution of chain sizes or lengths given an offspring distribution
and its associated parameters.

For example, if we have observed a distribution of chains of sizes
$1, 1, 4, 7$, we can calculate the log-likelihood of this observed chain
by assuming the offspring per generation is Poisson distributed with a
mean number (which can be interpreted as the reproduction number
$\mathcal{R_0}$) of $0.5$.

To do this, we run

``` r
set.seed(13)
chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
chain_ll(x = chain_sizes, offspring = "pois", stat = "size", lambda = 0.5)
#> [1] -8.607196
```

The first argument of `chain_ll()` is the chain size (or length, in
number of generations that a chain lasted) distribution to analyse. The
second argument, `offspring`, specifies the offspring distribution. This
is given as a function used to generate random offspring. It can be any
probability distribution implemented in `R`, that is, one that has a
corresponding function for generating random numbers beginning with the
letter `r`. In the case of the example above, since random Poisson
numbers are generated in `R` using a function called `rpois()`, the
string to pass to the `offspring` argument is `"pois"`.

The third argument, `stat`, determines whether to analyse chain sizes
(`"size"`, the default if this argument is not specified) or lengths
(`"length"`). Lastly, any named arguments not recognised by `chain_ll()`
are interpreted as parameters of the corresponding probability
distribution, here `lambda = 0.5` as the mean of the Poisson
distribution (see the `R` help page for the [Poisson
distribution](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Poisson.html)
for more information).

### Imperfect observations

By default, `chain_ll()` assumes perfect observation, where
`obs_prob = 1` (See `?chain_ll`), meaning that all transmission events
are observed and recorded in the data. If observations are imperfect,
`chain_ll()` provides the argument, `obs_prob`, for specifying the
probability of observation. This probability is used to determine the
likelihood of observing the specified chain sizes or lengths. In the
case of imperfect observation, true chain sizes or lengths are simulated
repeatedly (the number of times given by the `nsim_obs` argument), and
the likelihood calculated for each of these simulations.

For example, if the probability of observing each case is
`obs_prob = 0.30`, we use

``` r
chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
ll <- chain_ll(chain_sizes, "pois", "size", obs_prob = 0.3, lambda = 0.5,
               nsim_obs = 10)
ll
#>  [1] -26.54167 -23.26117 -24.33027 -20.80310 -30.76152 -26.46751 -23.79326
#>  [8] -19.14490 -32.08875 -22.23401
```

This returns `10` likelihood values (because `nsim_obs = 10`), which can
be averaged to come up with an overall likelihood estimate.

To find out about usage of the `chain_ll()` function, you can use the
`R` help file

``` r
?chain_ll
```

### How `chain_ll()` works

If the probability distribution of chain sizes or lengths has an
analytical solution, this will be used. `chain_ll()` currently supports
the Poisson and negative binomial size distribution and the Poisson and
geometric length distribution.

If an analytical solution does not exist, simulations are used to
approximate this probability distributions ([using a linear
approximation to the cumulative
distribution](https://en.wikipedia.org/wiki/Empirical_distribution_function)
for unobserved sizes/lengths). In that case, an extra argument
`nsim_offspring` must be passed to `chain_ll()` to specify the number of
simulations to be used for this approximation.

For example, to get offspring drawn from a binomial distribution with
probability `prob = 0.5`, we run

``` r
chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
chain_ll(chain_sizes, "binom", "size", size = 1, prob = 0.5,
         nsim_offspring = 100)
#> [1] -Inf
```

## Simulating branching processes

To simulate a branching process, we use the `chain_sim()` function. This
function follows the same syntax as `chain_ll()`.

Below, we are simulating $5$ chains, assuming the offspring are
generated using a Poisson distribution with mean, `lambda = 0.5`. By
default, `chain_sim()` returns a vector of chain sizes/lengths. If we
instead want to return a tree of infectees and infectors, we need to
specify a function for the serial interval and set `tree = TRUE` (see
next section).

``` r
chain_sim(n = 5, offspring = "pois", stat = "size", lambda = 0.5)
#> [1] 2 6 2 1 2
```

### Simulating trees

To simulate a tree of transmission chains, we specify the serial
interval generation function and set `tree = TRUE` as follows:

``` r
set.seed(13)
serial_interval <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}
chains_df <- chain_sim(
  n = 5, offspring = "pois", lambda = 0.5, stat = "length",
  infinite = 100, serial = serial_interval, tree = TRUE
)
head(chains_df)
#>   n id ancestor generation       time
#> 1 1  1       NA          1 0.00000000
#> 2 2  1       NA          1 0.00000000
#> 3 3  1       NA          1 0.00000000
#> 4 4  1       NA          1 0.00000000
#> 5 5  1       NA          1 0.00000000
#> 6 1  2        1          2 0.04771887
```

## Package vignettes

Specific use cases of *epichains* can be found in the [online
documentation as package
vignettes](https://epiverse-trace.github.io/epichains/), under
“Articles”.

## Reporting bugs

To report a bug please open an
[issue](https://github.com/epiverse-trace/epichains/issues/new/choose).

## Contribute

We welcome contributions to enhance the package’s functionalities. If
you wish to do so, please follow the [package contributing
guide](https://github.com/epiverse-trace/epichains/blob/main/.github/CONTRIBUTING.md).

## Code of conduct

Please note that the *epichains* project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citing this package

``` r
citation("epichains")
#> To cite package epichains in publications use:
#> 
#>   Sebastian Funk, Flavio Finger, and James M. Azam (2023). epichains:
#>   Analysing transmission chain statistics using branching process
#>   models, website: https://github.com/epiverse-trace/epichains/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {epichains: Analysing transmission chain statistics using branching process models},
#>     author = {{Sebastian Funk} and {Flavio Finger} and {James M. Azam}},
#>     year = {2023},
#>     url = {https://github.com/epiverse-trace/epichains/},
#>   }
```
