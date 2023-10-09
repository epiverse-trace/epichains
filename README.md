
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->

# *epichains*: Methods for simulating and analysing the size and length of transmission chains from branching process models <img src="man/figures/epichains_logo.png" align="right" height="130" />

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/epiverse-trace/epichains)
[![R-CMD-check](https://github.com/epiverse-trace/epichains/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/epichains/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiverse-trace/epichains/branch/main/graphs/badge.svg)](https://codecov.io/github/epiverse-trace/epichains)
![GitHub
contributors](https://img.shields.io/github/contributors/epiverse-trace/epichains)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

*epichains* is an R package to simulate, analyse, and visualize the size
and length of branching processes with a given offspring distribution.
These models are often used in infectious disease epidemiology, where
the chains represent chains of transmission, and the offspring
distribution represents the distribution of secondary infections caused
by an infected individual.

*epichains* re-implements
[epichains](%22https://github.com/epiverse-trace/epichains/%22) by
providing bespoke functions and data structures that allow easy
manipulation and interoperability with other Epiverse packages, for
example,
[superspreading](%22https://github.com/epiverse-trace/superspreading/%22)
and
[epiparameter](%22https://github.com/epiverse-trace/epiparameter/%22),
and potentially some existing packages for handling transmission chains,
for example, [epicontacts](https://github.com/reconhub/epicontacts).

*epichains* is developed at the [Centre for the Mathematical Modelling
of Infectious
Diseases](https://www.lshtm.ac.uk/research/centres/centre-mathematical-modelling-infectious-diseases)
at the London School of Hygiene and Tropical Medicine as part of the
[Epiverse Initiative](https://data.org/initiatives/epiverse/).

## Installation

The latest development version of the *epichains* package can be
installed via

``` r
# check whether {pak} is installed
if (!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/epichains")
```

To load the package, use

``` r
library("epichains")
```

## Quick start

*epichains* provides four main functions:

- `simulate_tree()`: simulates transmission chains using an initial
  number of cases and information on the offspring distribution. This
  function returns an object with columns that track information on who
  infected whom, the generation of infection, and optionally, the time
  of infection.

- `simulate_summary()`: simulates a vector of transmission chain sizes
  or lengths using an initial number of cases and information on the
  offspring distribution. This function only returns a vector of
  realized chain size or length.

- `simulate_tree_from_pop()`: simulates transmission chains given an
  initial population size and information on the offspring distribution.
  You can also specify a given level of pre-existing immunity. This
  function returns an object with columns that track information on who
  infected whom, the generation of infection, and the time of infection.

- `likelihood()`: calculates the loglikelihood (or likelihood, depending
  on the value of `log`) of observing a vector of transmission chain
  sizes or lengths.

The objects returned by the `simulate_*()` functions can be summarised
with `summary()` and aggregated into a `<data.frame>` of cases per time
or generation with `aggregate()`. Aggregated results can also be passed
on to `plot()` with its own arguments to customize the resulting plots.

Each of the listed functionalities is demonstrated in detail in the
[“Getting Started”
vignette](https://epiverse-trace.github.io/epichains/articles/epichains.html).

## Package vignettes

The theory behind the models provided here can be found in the [theory
vignette](https://epiverse-trace.github.io/epichains/articles/theoretical_background.html).

We have also collated a bibliography of branching process applications
in epidemiology. These can be found in the [literature
vignette](https://epiverse-trace.github.io/epichains/articles/branching_process_literature.html).

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
