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
