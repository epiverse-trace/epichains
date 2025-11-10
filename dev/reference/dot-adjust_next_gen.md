# Adjust new offspring if it exceeds the susceptible population size

This function is used internally, and input checking is not performed
here, only in the context where it is used. Using it directly is not
recommended.

## Usage

``` r
.adjust_next_gen(next_gen, susc_pop)
```

## Arguments

- next_gen:

  A numeric vector of next generation offspring.

- susc_pop:

  The susceptible population size; A number coercible to integer.

## Value

A numeric vector of the adjusted next generation offspring.
