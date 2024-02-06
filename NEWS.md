# epichains 0.0.9999

* `epichains` is a re-implementation of `bpmodels` with a focus on providing
a dedicated class of data structures for easy manipulation and interoperability
with other new tools in the Epiverse and existing ecosystem.

* The `offspring_dist` argument has been changed to no longer accept a character
  string, but instead now accepts a function (#25, #167, #188).
  This expands the number of possible functions a user can use and is no longer 
  restricted to distributions with a corresponding random number generator function
  starting with `r...()`. For example:
  
  ```r
  # This was not possible in earlier versions of this package.
  my_poisson_wrapper <- function(n) {
    rpois(n, lambda = 0.9)
  }
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = my_poisson_wrapper,
    statistic = "length"
  )

  In adding this functionality an `rgborel` function was also added; this is because of a difference from previous behaviour, where the ll function name could be informed from the string passed and now it needs a function with that name; the function itself is not actually called when estimating the likelihood if the corresponding ll function exists so this could, in principle, be an empty dummy function; however, the function is included for documentation/clarity purposes, as well as for simulations.

## Documentation

* A vignette outlining how to simulate interventions has been added.

## Functions

* `simulate_tree()`: simulate transmission trees from a given number of initial
cases.
* `simulate_tree_from_pop()`: simulate transmission trees from a given 
population size and initial immunity.
* `simulate_summary()`: simulate a vector of observed transmission chains 
sizes/lengths from a given number of chains.
* `likelihood()`: estimate the likelihood/loglikelihood of observing
chains of given sizes/lengths.

## Methods

* Use `print()` and `summary()` to obtain more insightful results compared to
previously in {bpmodels}.
* Use `aggregate()` to aggregate results from `simulate_tree()` and
`simulate_tree_from_pop()` into cases by "generation" and "time" (if time is
simulated)

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
