# epichains 0.1.9999

## Documentation

* A vignette outlining how to simulate interventions has been added

# epichains 0.1.0

## Package name change

* `epichains` is a re-implementation of `bpmodels` with a focus on providing
  a dedicated class of data structures for easy manipulation and interoperability
  with other new tools in the pipeline.

### Functions

* `simulate_tree()`: simulate transmission trees from a given number of chains.
* `simulate_tree_from_pop()`: simulate transmission trees from a given 
  population size and initial immunity.
* `simulate_summary()`: simulate a vector of observed transmission chains 
  sizes/lengths from a given number of chains.
* `likelihood()`: estimate the likelihood/loglikelihood of observing
  chains of given sizes/lengths.

### Classes

* An `epichains` class, which inherits from `data.frame` with attributes for
  tracking:
  - `chains`: number of chains simulated
  - `chain_type`:
    - `chains_tree`, if returned from `simulate_tree()` or 
      `simulate_tree_from_pop()`
    - `chains_summary`, if returned from `simulate_summary()`.
  - `track_pop`: whether the susceptible population is tracked or not.
* An `epichains_aggregate_df` class, which inherits from `data.frame` with
  attributes for tracking:
  - `chain_type`: as defined above, and
  - `aggregated_over`: the variable(s) over which aggregation was done: "time",
  "generation" or "both". Useful for easy plotting with the `plot` method (see
  methods section below).

### Methods

* `print()`
* `summary()`
* `aggregate()`

# bpmodels 0.2.1

## Minor functionality change

* `chain_sim()` now throws a warning, instead of an error, when `tree` is set 
to `FALSE` with `serial` also specified. We assume that providing a serial 
interval means you want the tree of transmissions to be simulated, 
so `chain_sim()` internally sets `tree = TRUE` and throws a warning explaining 
what happened. This behaviour should not break any simulations with previous 
versions with `bpmodels`, but if it does, please submit an issue. 
To remove the warning, the user should explicitly set `tree = TRUE` when 
they specify `serial`. 

# bpmodels 0.2.0

## Documentation

* `chain_sim()`'s help file has been updated with more details. In particular,
we describe in detail how to specify the `serial` argument as a function. We 
have also added more examples.

* A new vignette describing how to project COVID-19 incidence with `chain_sim()`
has been added and can be accessed on the 
[bpmodels website](https://epiverse-trace.github.io/bpmodels/) under "Articles".

* The README's "quick start" section has been updated with what was 
previously the introduction vignette.

# bpmodels 0.1.9999

* faster, vectorised chain simulations

# bpmodels 0.1.0

* initial release
