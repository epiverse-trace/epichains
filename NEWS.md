# epichains 0.1.0

We are excited to announce the first minor release of `{epichains}`.

`{epichains}` re-implements [{bpmodels}](https://github.com/epiverse-trace/bpmodels), focusing on a unified simulation framework using branching processes to simulate transmission chains data. The framework incorporates susceptible depletion and pre-existing immunity and provides dedicated data structures for handling and analysing transmission chains in both tabular and vector formats. The goal is to provide seamless interoperability with other packages within the Epiverse-TRACE Initiative and the broader epidemiological tool ecosystem.

## New Features

### Documentation

- **Dedicated Website:** Explore all features and documentation on the [epichains website](https://epiverse-trace.github.io/epichains/).
- **Help:** Each function comes with extensive documentation. We welcome your feedback and suggestions for improvements.
- **Vignettes:** This release comes with five detailed vignettes:
  - **Getting Started:** A quick guide to the key functions.
  - **Modelling Disease Control Interventions:** Learn how to model various intervention strategies.
  - **Projecting Infectious Disease Incidence:** A case study on projecting COVID-19 incidence.
  - **Literature:** A curation of literature on branching process applications in epidemiology.
  - **Theoretical Background:** A deep dive into the theoretical background of the functions in the package (Contributor documentation).
  - **Design principles**: The design principles of `{epichains}` (Contributor documentation).

### Simulation

- **`simulate_chains()`**: Simulate independent transmission chains from a specified number of initial cases, incorporating susceptible depletion and pre-existing immunity.
- **`simulate_chain_stats()`**: Generate a vector of chain sizes or lengths from a specified number of initial cases, incorporating susceptible depletion and pre-existing immunity.

### Inference

- **`likelihood()`**: Estimate the (log)likelihood of transmission chain sizes or lengths, with support for numeric vectors or `<epichains>` and `<epichains_summary>` objects.

### Transmission Chain Data Manipulation

- **`summary()`**: Extract vectors of chain sizes or lengths from `<epichains>` objects.
- **`aggregate()`**: Generate case time series by aggregating by generation or time of infection.
- **`plot()`**: Visualize individual transmission chains filtered by their id.
