
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
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/epichains/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/epichains?branch=main)
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
[bpmodels](https://github.com/epiforecasts/bpmodels/) by providing
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

Install the released version of the package:

``` r
install.packages("epiverse-trace/epichains")
```

The latest development version of the *epichains* package can be
installed via

``` r
# check whether {remotes} is installed
if (!require("remotes")) install.packages("remotes")
remotes::install_github("epiverse-trace/epichains")
```

If this fails, try using the `pak` R package via

``` r
# check whether {pak} is installed
if (!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/epichains")
```

If both of these options fail, please [file an
issue](https://github.com/epiverse-trace/epichains/issues) with a full
log of the error messages. Here is an [example of an issue reporting an
installation
failure](https://github.com/epiverse-trace/epichains/issues/262). This
will help us to improve the installation process.

To load the package, use

``` r
library("epichains")
```

## Quick start

*epichains* provides three main functions:

- `simulate_chains()`: simulates transmission chains using a simple
  branching process model that accepts an index number of cases that
  seed the outbreak, a distribution of offspring per case, and a chain
  statistic to track (size or length/duration). It optionally accepts
  other population related inputs such as the population size (defaults
  to Inf) and percentage of the population initially immune (defaults to
  0). This function returns an object with columns that track
  information on who infected whom, the generation of infection and, if
  a generation time function is specified, the time of infection.

- `simulate_chain_stats()`: provides a performant version of
  `simulate_chains()` that only tracks and return a vector of realized
  chain sizes or lengths/durations for each index case without details
  of the infection tree.

- `likelihood()`: calculates the loglikelihood (or likelihood, depending
  on the value of `log`) of observing a vector of transmission chain
  sizes or lengths.

The objects returned by the `simulate_*()` functions can be summarised
with `summary()`. Running `summary()` on the output of
`simulate_chains()` will return the same output as
`simulate_chain_stats()` using the same inputs.

Objects returned from `simulate_chains()` can be aggregated into a
`<data.frame>` of cases per time or generation with the function
`aggregate()`.

The simulated `<epichains>` object can be plotted in various ways using
`plot()`. See the plotting section in `vignette("epichains")` for two
use cases.

### Simulation

For the simulation functionality, let’s look at a simple example where
we simulate a transmission chain with $20$ index cases, a constant
generation time of $3$, and a poisson offspring distribution with mean
$1$. We are tracking the chain “size” statistic and will cap all chain
sizes at $25$ cases. We will then look at the summary of the simulation,
and aggregate it into cases per generation.

``` r
set.seed(32)
# Simulate chains
sim_chains <- simulate_chains(
  n_chains = 20,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 25,
  generation_time = function(n) {
    rep(3, n)
  }, # constant generation time of 3
  lambda = 1 # mean of the Poisson distribution
)
# View the head of the simulation
head(sim_chains)
#>    chain infector infectee generation time
#> 21     1        1        2          2    3
#> 22     2        1        2          2    3
#> 23     3        1        2          2    3
#> 24     3        1        3          2    3
#> 25     4        1        2          2    3
#> 26     6        1        2          2    3

# Summarise the simulation
summary(sim_chains)
#> `epichains_summary` object 
#> 
#>  [1]   5  17   4   8   1  16   9 Inf   5  18   5   1 Inf  24   1  14  19   2   4
#> [20]  14
#> 
#>  Simulated sizes: 
#> 
#> Max: >=25
#> Min: 1

# Aggregate the simulation into cases per generation
chains_agregegated <- aggregate(sim_chains, by = "generation")

# view the time series of cases per generation
chains_agregegated
#>    generation cases
#> 1           1    20
#> 2           2    26
#> 3           3    36
#> 4           4    43
#> 5           5    31
#> 6           6    25
#> 7           7    20
#> 8           8     9
#> 9           9     3
#> 10         10     1
#> 11         11     1
#> 12         12     1
#> 13         13     1
```

### Inference

Let’s look at the following example where we estimate the log-likelihood
of observing a hypothetical `chain_lengths` dataset.

``` r
set.seed(32)
# randomly generate 20 chain lengths between 1 to 40
chain_lengths <- sample(1:40, 20, replace = TRUE)
chain_lengths
#>  [1]  6 11 20  9 40 33 39 27  6 12 39 35  9 25  6 15 12  6 37 35

# estimate loglikelihood of the observed chain sizes
likelihood_eg <- likelihood(
  chains = chain_lengths,
  statistic = "length",
  offspring_dist = rpois,
  lambda = 0.99
)
# Print the estimate
likelihood_eg
#> [1] -104.2917
```

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

<details>
<summary>
Click to expand
</summary>

- [bpmodels](https://github.com/epiforecasts/bpmodels/): provides
  methods for analysing the size and length of transmission chains from
  branching process models. `{epichains}` supersedes `{bpmodels}`, which
  has been retired.

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

</details>

## Reporting bugs

To report a bug please open an
[issue](https://github.com/epiverse-trace/epichains/issues/new/choose).

## Contribute

Contributions to {epichains} are welcomed. Please follow the [package
contributing
guide](https://github.com/epiverse-trace/.github/blob/main/CONTRIBUTING.md).

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
#>   Azam J, Funk S, Finger F (2024). _epichains: Simulating and Analysing
#>   Transmission Chain Statistics Using Branching Process Models_. R
#>   package version 0.1.1, https://epiverse-trace.github.io/epichains/,
#>   <https://github.com/epiverse-trace/epichains>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {epichains: Simulating and Analysing Transmission Chain Statistics Using
#> Branching Process Models},
#>     author = {James M. Azam and Sebastian Funk and Flavio Finger},
#>     year = {2024},
#>     note = {R package version 0.1.1, 
#> https://epiverse-trace.github.io/epichains/},
#>     url = {https://github.com/epiverse-trace/epichains},
#>   }
```
