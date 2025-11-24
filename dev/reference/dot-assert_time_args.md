# Check inputs that control time events

This function checks the time-related inputs, i.e., start time of each
chain, `t0`, the end time of the simulation, `tf`, and the generation
time, generation_time. It also checks that the generation_time argument
is specified if `tf` is specified as these go hand-in-hand.

## Usage

``` r
.assert_time_args(tf_specified, tf, generation_time, t0)
```

## Arguments

- tf_specified:

  `<logical>`; Whether the `tf` argument is specified. Only makes sense
  in the context where this function is called, i.e., in
  [`simulate_chains()`](https://epiverse-trace.github.io/epichains/dev/reference/simulate_chains.md).
  If `tf` is specified, generation_time must be specified.

- tf:

  A number for the cut-off for the infection times (if generation time
  is given); Defaults to `Inf`.

- generation_time:

  The generation time function; the name of a user-defined named or
  anonymous function with only one argument `n`, representing the number
  of generation times to sample.

- t0:

  Start time (if generation time is given); either a single value or a
  vector of same length as `n_chains` (number of initial cases) with
  corresponding initial times. Defaults to 0, meaning all cases started
  at time 0.

## Value

`NULL`; called for side effects
