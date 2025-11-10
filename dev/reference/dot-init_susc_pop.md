# Adjust next generation vector to match susceptible population size

Calculates the initial susceptible population size given the total
population size, the percent immune, and the number of index cases. This
function is used internally, and input checking is not performed here,
only in the context where it is used. Using it directly is not
recommended.

## Usage

``` r
.init_susc_pop(pop, percent_immune, index_cases)
```

## Arguments

- pop:

  Population size; An `<Integer>`. Used alongside `percent_immune` to
  define the susceptible population. Defaults to `Inf`.

- percent_immune:

  Percent of the population immune to infection at the start of the
  simulation; A `<numeric>` between 0 and 1. Used alongside `pop` to
  initialise the susceptible population. Defaults to 0.

## Value

Initial susceptible population size; A numeric coercible to integer.
