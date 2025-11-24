# Check that the `statistic` and `stat_threshold` arguments are valid

The function treats these two arguments as related and checks them in
one place to remove repeated checks in several places in the package.

## Usage

``` r
.assert_statistic_args(statistic, stat_threshold)
```

## Arguments

- statistic:

  The chain statistic to track as the stopping criteria for each chain
  being simulated when `stat_threshold` is not `Inf`; A `<string>`. It
  can be one of:

  - "size": the total number of cases produced by a chain before it goes
    extinct.

  - "length": the total number of generations reached by a chain before
    it goes extinct.

- stat_threshold:

  A stopping criterion for individual chain simulations; a positive
  number coercible to integer. When any chain's cumulative statistic
  reaches or surpasses `stat_threshold`, that chain ends. Defaults to
  `Inf`. For example, if `statistic = "size"` and `stat_threshold = 10`,
  then any chain that produces 10 or more cases will stop. Note that
  setting `stat_threshold` does not guarantee that all chains will stop
  at the same value.

## Value

NULL; called for side effects
