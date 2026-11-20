---
title: 'epichains: An R package for simulating and analysing transmission chain statistics using branching process models'
tags:
  - R
  - epidemiology
  - infectious disease modelling
  - branching processes
  - transmission chains
  - outbreak analytics
authors:
  - name: James M. Azam
    orcid: 0000-0001-5782-7330
    corresponding: true
    affiliation: 1
  - name: Flavio Finger
    orcid: 0000-0002-8613-5170
    affiliation: 2
  - name: Sebastian Funk
    orcid: 0000-0002-2842-3406
    affiliation: 1
affiliations:
  - name: Centre for the Mathematical Modelling of Infectious Diseases, London School of Hygiene & Tropical Medicine, London, United Kingdom
    index: 1
  - name: Epicentre, Médecins Sans Frontières, Paris, France
    index: 2
date: 20 November 2026
bibliography: paper.bib
---

# Summary

Branching processes model outbreaks as chains of transmission in which
each case infects a random number of others, drawn from an *offspring
distribution* [@jacob2010]. The *size* (total number of cases) and
*length* (number of generations) of these chains depend on the
reproduction number and on how unevenly transmission is spread across
individuals, so they can be used both to project outbreaks and to estimate
transmission parameters from data on clusters of cases
[@farrington2003; @blumberg2013; @blumberg2013a].

`epichains` is an R package with two components. For *simulation*, it
generates transmission chains from a branching process with any
user-supplied offspring distribution, optionally with generation times, a
finite population, and pre-existing immunity. For *inference*, it computes
the likelihood of observed chain sizes or lengths, accounting for censored
chains and imperfect case ascertainment, so that transmission parameters
such as the reproduction number and the dispersion parameter can be
estimated from cluster size data.

# Statement of need

Data on the sizes or durations of transmission clusters are routinely
collected for pathogens that cause limited outbreaks, such as measles in
highly vaccinated populations, mpox, MERS-CoV, and zoonotic spillovers, and
for the early phase of emerging epidemics. Branching process models are the
standard tool for analysing such data: chain size distributions yield
estimates of the reproduction number and of the dispersion parameter $k$
that characterises superspreading [@lloyd-smith2005; @blumberg2013a],
including for supercritical processes when large chains are treated as
censored [@nishiura2012]. Run forward with generation times, the same
models produce stochastic projections and scenario analyses of
interventions [@pearson2020; @abbott2020]. Practitioners have nonetheless
often had to write their own chain size likelihoods and simulators,
typically for a single offspring distribution and without tests.
`epichains` gives epidemiologists, outbreak analysts, and methodologists a
tested, documented, and general implementation of both.

# State of the field

`bpmodels` [@bpmodels], the direct predecessor of `epichains`, provided
chain size and length likelihoods and simulation, but returned unstructured
outputs and did not model finite populations; it has now been retired.
Other R packages use branching processes for narrower purposes: `ringbp`
[@hellewell2020] assesses contact tracing and isolation for SARS-CoV-2,
`projections` [@projections] projects incidence from case counts without
tracking chains, `simulist` [@simulist] generates synthetic line lists and
contact data, and `superspreading` [@superspreading] computes epidemic risk
under heterogeneous transmission. Agent-based simulators such as `nosoi`
[@lequime2020] and transmission-tree reconstruction tools such as
`outbreaker2` [@campbell2018outbreaker2] and `TransPhylo` [@didelot2017]
address different questions. None of these combines general-purpose chain
simulation with likelihood-based inference from chain sizes or lengths.
`epichains` fills this gap. Its outputs convert directly to `epicontacts`
[@campbell2018epicontacts] objects, and offspring parameters from
`superspreading` or `epiparameter` [@epiparameter] can be passed straight
to its functions.

# Software design

The package exports two functions for simulation and one for inference
(\autoref{fig:workflow}).

![Workflow of `epichains`. The two simulation functions return
`<epichains>` and `<epichains_summary>` objects, which, like observed chain
sizes or lengths, can be passed to `likelihood()`.
\label{fig:workflow}](epichains-workflow.png)

## Simulation

`simulate_chains()` simulates chains generation by generation from a set of
index cases, optionally assigning infection times from a `generation_time`
function and thinning offspring by the proportion of the population still
susceptible. Each chain stops when it goes extinct, reaches
`stat_threshold`, or passes the time cut-off `tf`. It returns an
`<epichains>` data frame of infector--infectee pairs that stores the
simulation inputs as attributes and has `print()`, `summary()`, and
`aggregate()` methods. `simulate_chain_stats()` runs the same process but
tracks only the size or length of each chain, which is much faster.

## Inference

`likelihood()` uses closed-form chain size or length distributions where
they exist [@farrington2003; @blumberg2013] (Poisson, negative binomial,
and gamma-Borel mixture for sizes; Poisson and geometric for lengths) and
otherwise approximates them from chains simulated with
`simulate_chain_stats()`. Chains at or above `stat_threshold` are treated
as right-censored, and imperfect observation is handled by simulating
`nsim_obs` thinned datasets. Because `likelihood()` returns a single number
for a given set of parameters, it can be passed to an optimiser or used
inside a Bayesian sampler; `epichains` leaves the choice of estimation
method to the user.

## Dependencies and documentation

The package imports only `checkmate` [@checkmate] besides base R. Its
vignettes cover the theory, the design decisions, and applications such as
modelling interventions and projecting incidence.

# Example: estimating $R_0$ and $k$ from chain sizes

We simulate 200 chain sizes with $R_0 = 0.8$ and $k = 0.3$, stopping
chains at 50 cases and recording them as censored (`Inf`). We then maximise
the log-likelihood with `optim()` on the log scale and derive approximate
95% confidence intervals from the Hessian:

``` r
library(epichains)
set.seed(32)

chain_sizes <- simulate_chain_stats(
  n_chains = 200,
  statistic = "size",
  offspring_dist = rnbinom,
  stat_threshold = 50,
  mu = 0.8,
  size = 0.3
)

neg_loglik <- function(par) {
  -likelihood(
    chains = chain_sizes,
    statistic = "size",
    offspring_dist = rnbinom,
    mu = exp(par[["log_R0"]]),
    size = exp(par[["log_k"]])
  )
}

fit <- optim(c(log_R0 = 0, log_k = 0), neg_loglik, hessian = TRUE)
se <- sqrt(diag(solve(fit$hessian)))

estimates <- exp(cbind(
  estimate = fit$par,
  lower = fit$par - 1.96 * se,
  upper = fit$par + 1.96 * se
))
rownames(estimates) <- c("R0", "k")
round(estimates, 2)
#>    estimate lower upper
#> R0     0.72  0.62  0.84
#> k      0.39  0.22  0.71
```

Both intervals contain the true values.

# Research impact statement

`epichains` is distributed on CRAN [@epichains] and archived on Zenodo.
Its predecessor `bpmodels` was used for early stochastic projections of
COVID-19 [@pearson2020; @abbott2020], and `epichains` is now the maintained
implementation of these methods within the Epiverse-TRACE ecosystem of
outbreak analytics tools.

`epichains` has been cited in at least five publications since its release.
These include analyses of the transmissibility and outbreak risk of
measles in the United States [@bidari2025], reviews of epidemiological
parameters for H5N1 influenza [@ward2025] and for pathogens with pandemic
potential [@ward2026], and two software packages for epidemic modelling:
`modelSSE`, which characterises superspreading from contact tracing data
[@zhao2025], and `gemlib`, a probabilistic programming library for epidemic
models [@morariu2025]. <!-- TODO: add reverse dependencies, CRAN download
counts, or use in training materials or outbreak response, if relevant. -->

# AI usage disclosure

<!-- TODO: confirm and edit. -->

Generative AI (Claude, Anthropic) was used to produce an initial draft of
this paper from the package documentation and source code. All text was
reviewed and edited by the authors, who take responsibility for its
content. <!-- TODO: state whether AI tools were used in writing the
package code, tests, or documentation, and if so, how. -->

# Acknowledgements

We thank Zhian N. Kamvar, Hugo Gruson, Chris Hartgerink, Karim Mané, Pratik
Gupte, and Joshua W. Lambert for contributions to and reviews of the
package. `epichains` is developed as part of the Epiverse-TRACE initiative.
<!-- TODO: add funder(s) and grant numbers. -->

# References
