# Changelog

## epichains (development version)

## epichains 0.1.1.9000

### Package

- The package now has websites for the [development version on
  GitHub](https://epiverse-trace.github.io/epichains/dev/) and [CRAN
  version](https://epiverse-trace.github.io/epichains/). By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#293](https://github.com/epiverse-trace/epichains/issues/293) and
  self-reviewed.
- Fixed an issue where equations in the pkgdown function reference were
  not being rendered.

### Documentation

- The README now has badges for CRAN monthly and total downloads as well
  as the package’s Zenodo DOI.
- The theory vignette now has a references section. By
  [@Degoot-AM](https://github.com/Degoot-AM) in
  [\#316](https://github.com/epiverse-trace/epichains/issues/316).

### Internal changes

- Checking functions previously named `check_*()` have been renamed to
  `assert_*()` to align with naming conventions used in the
  [checkmate](https://mllg.github.io/checkmate/) package.
- The `simulate_*()` functions now ensure integer operations are safely
  maintained in computations to ensure memory and computational
  efficiency.

## epichains 0.1.1

CRAN release: 2024-10-14

This is the first CRAN release of
[epichains](https://github.com/epiverse-trace/epichains).

### Package

- Removed the `as_is = TRUE` setting in vignette YAML headers to avoid
  partial incompatibility with incoming pkgdown versions.

## epichains 0.1.0

We are excited to announce the first minor release of
[epichains](https://github.com/epiverse-trace/epichains).

[epichains](https://github.com/epiverse-trace/epichains) re-implements
[{bpmodels}](https://github.com/epiforecasts/bpmodels/), focusing on a
unified simulation framework using branching processes to simulate
transmission chains data. The framework incorporates susceptible
depletion and pre-existing immunity and provides dedicated data
structures for handling and analysing transmission chains in both
tabular and vector formats. The goal is to provide seamless
interoperability with other packages within the Epiverse-TRACE
Initiative and the broader epidemiological tool ecosystem.

### New Features

#### Documentation

- **Dedicated Website:** Explore all features and documentation on the
  [epichains website](https://epiverse-trace.github.io/epichains/).
- **Help:** Each function comes with extensive documentation. We welcome
  your feedback and suggestions for improvements.
- **Vignettes:** This release comes with five detailed vignettes:
  - **Getting Started:** A quick guide to the key functions.
  - **Modelling Disease Control Interventions:** Learn how to model
    various intervention strategies.
  - **Projecting Infectious Disease Incidence:** A case study on
    projecting COVID-19 incidence.
  - **Literature:** A curation of literature on branching process
    applications in epidemiology.
  - **Theoretical Background:** A deep dive into the theoretical
    background of the functions in the package (Contributor
    documentation).
  - **Design principles**: The design principles of
    [epichains](https://github.com/epiverse-trace/epichains)
    (Contributor documentation).

#### Simulation

- **[`simulate_chains()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chains.md)**:
  Simulate independent transmission chains from a specified number of
  initial cases, incorporating susceptible depletion and pre-existing
  immunity.
- **[`simulate_chain_stats()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chain_stats.md)**:
  Generate a vector of chain sizes or lengths from a specified number of
  initial cases, incorporating susceptible depletion and pre-existing
  immunity.

#### Inference

- **[`likelihood()`](https://epiverse-trace.github.io/epichains/dev/reference/likelihood.md)**:
  Estimate the (log)likelihood of transmission chain sizes or lengths,
  with support for numeric vectors or `<epichains>` and
  `<epichains_summary>` objects.

#### Transmission Chain Data Manipulation

- **[`summary()`](https://rdrr.io/r/base/summary.html)**: Extract
  vectors of chain sizes or lengths from `<epichains>` objects.
- **[`aggregate()`](https://rdrr.io/r/stats/aggregate.html)**: Generate
  case time series by aggregating by generation or time of infection.
- **[`plot()`](https://rdrr.io/r/graphics/plot.default.html)**:
  Visualize individual transmission chains filtered by their id.
