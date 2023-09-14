test_that("construct_offspring_ll_name works correctly", {
  expect_identical(
    construct_offspring_ll_name(
      offspring_dist = "pois",
      chain_statistic = "size"
    ),
    "pois_size_ll"
  )
})

test_that("update_chain_stat works correctly", {
  stat_latest <- 1
  n_offspring <- 2
  expect_identical(
    update_chain_stat(
      stat_type = "size",
      stat_latest = stat_latest,
      n_offspring = n_offspring
    ),
    stat_latest + n_offspring
  )
  expect_identical(
    update_chain_stat(
      stat_type = "length",
      stat_latest = stat_latest,
      n_offspring = n_offspring
    ),
    stat_latest + pmin(1, n_offspring)
  )
})

test_that("get_offspring_func works correctly", {
  pois_offspring_func <- get_offspring_func(
    offspring_dist = "pois",
    n = n,
    susc = susc,
    pop = pop,
    mean_offspring = mean_offspring,
    disp_offspring = disp_offspring
  )
  expect_snapshot(body(pois_offspring_func))
  nbinom_offspring_func <- get_offspring_func(
    offspring_dist = "nbinom",
    n = n,
    susc = susc,
    pop = pop,
    mean_offspring = mean_offspring,
    disp_offspring = disp_offspring
  )
  expect_snapshot(body(nbinom_offspring_func))
})

test_that("get_offspring_func throws errors", {
  expect_error(
    get_offspring_func(
      offspring_dist = "ss",
      n = n,
      susc = susc,
      pop = pop,
      mean_offspring = mean_offspring,
      disp_offspring = disp_offspring
    ),
    "must either be"
  )
})

test_that("get_statistic_func works correctly", {
  expect_identical(
    get_statistic_func(chain_statistic = "size"),
    rbinom_size
  )
  expect_identical(
    get_statistic_func(chain_statistic = "length"),
    rgen_length
  )
})
