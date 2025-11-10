# Sample all possible offspring for the next generation

Sample next generation of offspring using offspring distribution and
associated parameters. This function is used internally, and input
checking is not performed here, only in the context where it is used.
Using it directly is not recommended.

## Usage

``` r
.sample_possible_offspring(
  offspring_func,
  offspring_func_pars,
  n_offspring,
  chains
)
```

## Arguments

- offspring_func:

  A function to sample offspring.

- offspring_func_pars:

  A list of parameters for the offspring function.

- n_offspring:

  A numeric vector of the number of offspring per chain.

- chains:

  Numeric indices of chains/infectors being simulated

## Value

A numeric vector of the number of offspring per chain.
