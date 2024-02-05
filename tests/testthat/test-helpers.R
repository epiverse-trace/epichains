test_that("update_chain_stat works correctly", {
  stat_latest <- 1
  n_offspring <- 2
  expect_identical(
    .update_chain_stat(
      stat_type = "size",
      stat_latest = stat_latest,
      n_offspring = n_offspring
    ),
    stat_latest + n_offspring
  )
  expect_identical(
    .update_chain_stat(
      stat_type = "length",
      stat_latest = stat_latest,
      n_offspring = n_offspring
    ),
    stat_latest + pmin(1, n_offspring)
  )
  expect_error(
    .update_chain_stat(
      stat_type = "foo",
      stat_latest = stat_latest,
      n_offspring = n_offspring
    ),
    "stat_type must be 'size' or 'length'"
  )
})

test_that("get_statistic_func works correctly", {
  expect_identical(
    .get_statistic_func(chain_statistic = "size"),
    rbinom_size
  )
  expect_identical(
    .get_statistic_func(chain_statistic = "length"),
    rgen_length
  )
  expect_error(
    .get_statistic_func(chain_statistic = "foo"),
    "chain_statistic must be 'size' or 'length'"
  )
})

test_that(".init_susc_pop works correctly", {
  expect_identical(
    .init_susc_pop(
      pop = 10,
      percent_immune = 0.5,
      index_cases = 1
    ),
    4
  )
  expect_identical(
    .init_susc_pop(
      pop = 0,
      percent_immune = 0.5,
      index_cases = 1
    ),
    0
  )
  expect_identical(
    .init_susc_pop(
      pop = 10,
      percent_immune = 0,
      index_cases = 0
    ),
    10
  )
  expect_length(
    .init_susc_pop(
      pop = 10,
      percent_immune = 0,
      index_cases = 1
    ),
    1
  )
  expect_type(
    .init_susc_pop(
      pop = 10,
      percent_immune = 0,
      index_cases = 1
    ),
    "double"
  )
})

test_that(".init_susc_pop works correctly", {
  expect_length(
    .sample_possible_offspring(
      offspring_func = "rpois",
      offspring_func_pars = list(lambda = 1),
      n_offspring = 10,
      chains = 1
    ), 10
  )
  expect_error(
    .sample_possible_offspring(
      offspring_func = "rnorm",
      offspring_func_pars = list(mean = 0, sd = 1),
      n_offspring = 10,
      chains = 1
    ),
    "Offspring distribution must return integers"
  )
})
  )
})