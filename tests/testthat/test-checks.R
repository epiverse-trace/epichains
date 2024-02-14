test_that("Checks work", {
  expect_error(
    .check_offspring_func_valid(rrpois),
    "not found"
  )
  expect_error(
    .check_generation_time_valid("a"),
    "Must be a function"
  )
  expect_error(
    .check_generation_time_valid(function(x) rep("a", 10)),
    "numeric"
  )
  expect_error(
    .check_generation_time_valid(function(x) 3),
    "Must have length"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_chains",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1,
      generation_time_specified = FALSE,
      t0 = 0,
      tf_specified = TRUE,
      tf = 10
    ),
    "If `tf` is specified, `generation_time` must be specified too."
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_summary",
      index_cases = 0,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1
    ),
    "Must be >= 1."
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_summary",
      index_cases = 10,
      statistic = "a",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1
    ),
    "Must be element of set"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_summary",
      index_cases = 10,
      statistic = "size",
      offspring_dist = r,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1
    ),
    "object 'r' not found"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_summary",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = -1,
      pop = 10,
      percent_immune = 0.1
    ),
    "Element 1 is not >= 0"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_summary",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 1,
      pop = -1,
      percent_immune = 0.1
    ),
    "Element 1 is not >= 1"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_summary",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 1,
      pop = 10,
      percent_immune = -0.1
    ),
    "Element 1 is not >= 0"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_chains",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1,
      generation_time_specified = TRUE,
      generation_time = NULL,
      t0 = 0,
      tf_specified = TRUE,
      tf = 10
    ),
    "Must be a function, not 'NULL'"
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_chains",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1,
      generation_time_specified = TRUE,
      generation_time = function(x) rep(3, 10),
      t0 = -1,
      tf_specified = TRUE,
      tf = Inf
    ),
    "Element 1 is not >= 0."
  )
  expect_error(
    .check_sim_args(
      func_name = "simulate_chains",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1,
      generation_time_specified = TRUE,
      generation_time = function(x) rep(3, 10),
      t0 = 0,
      tf_specified = TRUE,
      tf = -1
    ),
    "Element 1 is not >= 0."
  )
  expect_no_error(
    .check_sim_args(
      func_name = "simulate_chains",
      index_cases = 10,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      pop = 10,
      percent_immune = 0.1,
      generation_time_specified = TRUE,
      generation_time = function(x) rep(3, 10),
      t0 = 0,
      tf_specified = TRUE,
      tf = 10
    )
  )
  expect_no_error(
    .check_statistic_args(
      statistic = "size",
      stat_max = 10
    )
  )
})
