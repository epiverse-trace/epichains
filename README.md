
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->

# *epichains*: Methods for simulating and analysing the size and length of transmission chains from branching process models <img src="man/figures/logo.svg" align="right" height="130" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/epichains/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/epichains/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiverse-trace/epichains/branch/main/graphs/badge.svg)](https://codecov.io/github/epiverse-trace/epichains)
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
[bpmodels](https://github.com/epiverse-trace/bpmodels/) by providing
bespoke functions and data structures that allow easy manipulation and
interoperability with other Epiverse-TRACE packages, for example,
[superspreading](https://github.com/epiverse-trace/superspreading/) and
[epiparameter](https://github.com/epiverse-trace/epiparameter/), and
potentially some existing packages for handling transmission chains, for
example, [epicontacts](https://github.com/reconhub/epicontacts).

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

*epichains* provides two main functions:

- `simulate_chains()`: simulates transmission chains using a simple
  branching process model that accepts an index number of cases that
  seed the outbreak, a distribution of offspring per case, and a chain
  statistic to track (size or length/duration). It optionally accepts
  other population related inputs such as the population size (defaults
  to Inf) and percentage of the population initially immune (defaults to
  0). This function returns an object with columns that track
  information on who infected whom, the generation of infection and, if
  a generation time function is specified, the time of infection.

- `simulate_summary()`: provides a performant version of
  `simulate_chains()` that only tracks and return a vector of realized
  chain sizes or lengths/durations for each index case without details
  of the infection tree.

- `likelihood()`: calculates the loglikelihood (or likelihood, depending
  on the value of `log`) of observing a vector of transmission chain
  sizes or lengths.

The objects returned by the `simulate_*()` functions can be summarised
with `summary()`. Running `summary()` on the output of
`simulate_chains()` will return the same output as `simulate_summary()`
using the same inputs.

Objects returned from `simulate_chains()` can be aggregated into a
`<data.frame>` of cases per time or generation with the function
`aggregate()`. The aggregated results can also be passed on to `plot()`
with its own arguments to customize the resulting plots.

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

## Related R packages

As far as we know, below are the existing R packages for simulating
branching processes and transmission chains.

- [bpmodels](https://github.com/epiverse-trace/bpmodels): provides
  methods for analysing the size and length of transmission chains from
  branching process models. `{epichains}` is intended to supersede
  `{bpmodels}`.

- [ringbp](https://github.com/epiforecasts/ringbp): a branching process
  model, parameterised to the 2019-nCoV outbreak, and used to quantify
  the potential effectiveness of contact tracing and isolation of cases.

- [covidhm](https://github.com/biouea/covidhm): code for simulating
  COVID-19 dynamics in a range of scenarios across a real-world social
  network. The model is conceptually based on `{ringbp}`.

- [epicontacts](https://github.com/reconhub/epicontacts): provides
  methods for handling, analysing, and visualizing transmission chains
  and contact-tracing data/linelists.

- [simulist](https://epiverse-trace.github.io/simulist/): uses a
  branching process model to simulate individual-level infectious
  disease outbreak data, including line lists and contact tracing data.
  This package is part of the Epiverse-TRACE Initiative.

- [superspreading](https://epiverse-trace.github.io/superspreading/):
  provides a set of functions to estimate and understand
  individual-level variation in transmission of infectious diseases from
  data on secondary cases. These are useful for understanding the role
  of superspreading in the spread of infectious diseases and for
  informing public health interventions.

- [earlyR](https://github.com/reconhub/earlyR): estimates the
  reproduction number (R), in the early stages of an outbreak. The model
  requires a specified serial interval distribution, characterised by
  the mean and standard deviation of the (Gamma) distribution, and data
  on daily disease incidence, including only confirmed and probable
  cases.

- [projections](https://github.com/reconhub/projections): uses data on
  daily incidence, the serial interval (time between onsets of infectors
  and infectees) and the reproduction number to simulate plausible
  epidemic trajectories and project future incidence. It relies on a
  branching process where daily incidence follows a Poisson or a
  Negative Binomial distribution governed by a force of infection.

- [simulacr](https://github.com/reconhub/simulacr): simulates outbreaks
  for specified values of reproduction number, incubation period,
  duration of infectiousness, and optionally reporting delays. Outputs a
  linelist stored as a `data.frame` with the class `outbreak`, including
  information on transmission chains; the output can be converted to
  `<epicontacts>` objects for visualisation.

- [outbreakr](https://sites.google.com/site/therepiproject/r-pac/outbreaker):
  implements a Bayesian approach for reconstructing outbreak data from
  pathogen genome sequences. It also implements a tool for outbreak
  simulation.

- [outbreakr2](https://github.com/reconhub/outbreaker2): a Bayesian
  framework for integrating epidemiological and genetic data to
  reconstruct transmission trees of densely sampled outbreaks. It
  re-implements, generalises and replaces the model of outbreaker, and
  uses a modular approach which enables fine customisation of priors,
  likelihoods and parameter movements.

- [o2geosocial](https://github.com/alxsrobert/o2geosocial): integrates
  geographical and social contact data to reconstruct transmission
  chains. It combines the age group, location, onset date and genotype
  of cases to infer their import status, and their likely infector.

- [nosoi](https://github.com/slequime/nosoi): simulates agent-based
  transmission chains by taking into account the influence of multiple
  variables on the transmission process (e.g. dual-host systems (such as
  arboviruses), within-host viral dynamics, transportation, population
  structure), alone or taken together, to create complex but relatively
  intuitive epidemiological simulations.

- [TransPhylo](https://xavierdidelot.github.io/TransPhylo/index.html):
  reconstructs infectious disease transmission using genomic data.

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
#> To cite package 'epichains' in publications use:
#> 
#>   Azam J, Finger F, Funk S (2024). _epichains: Simulating and Analysing Transmission Chain Statistics Using
#>   Branching Process Models_. R package version 0.0.0.9999, https://epiverse-trace.github.io/epichains/,
#>   <https://github.com/epiverse-trace/epichains>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {epichains: Simulating and Analysing Transmission Chain Statistics Using
#> Branching Process Models},
#>     author = {James M. Azam and Flavio Finger and Sebastian Funk},
#>     year = {2024},
#>     note = {R package version 0.0.0.9999, 
#> https://epiverse-trace.github.io/epichains/},
#>     url = {https://github.com/epiverse-trace/epichains},
#>   }
```
