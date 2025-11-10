# Determine and update the chain statistic being tracked

Determine and update the chain statistic being tracked

## Usage

``` r
.update_chain_stat(stat_type, stat_latest, n_offspring)
```

## Arguments

- stat_type:

  Chain statistic (size/length) to update; A character string. Must be
  one of 'size' or 'length'.

- stat_latest:

  The latest chain statistic numeric vector to be updated.

- n_offspring:

  A vector of offspring per chain. A numeric vector (coercible to
  integer).

## Value

A vector of chain statistics (size/length). A numeric vector coercible
to integer.
