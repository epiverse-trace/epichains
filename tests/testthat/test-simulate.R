#' Define global variables and options for simulations
generation_time_fn <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}

test_that("Simulators work", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_tree(
    ntrees = 2,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_tree(
    ntrees = 10,
    statistic = "size",
    offspring_dist = "pois",
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    ntrees = 2,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 0.9
  )
  #' Expectations
  expect_length(
    chain_summary_raw,
    2
  )
  expect_gte(
    nrow(tree_sim_raw),
    2
  )
  expect_gte(
    nrow(tree_sim_raw2),
    2
  )
  expect_gte(
    nrow(susc_outbreak_raw),
    1
  )
  expect_gte(
    nrow(susc_outbreak_raw2),
    1
  )
  expect_true(
    all(
      simulate_tree(
        ntrees = 10,
        statistic = "size",
        offspring_dist = "pois",
        stat_max = 10,
        generation_time = generation_time_fn,
        lambda = 2,
        tf = 5
      )$time < 5
    )
  )
})

test_that("simulate_tree throws errors", {
  expect_error(
    simulate_tree(
      ntrees = 2,
      offspring_dist = "s",
      statistic = "length",
      lambda = 0.9
    ),
    "does not exist"
  )
  expect_error(
    simulate_tree(
      ntrees = 2,
      offspring_dist = "lnorm",
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "must return integers"
  )
  expect_error(
    simulate_tree(
      ntrees = 2,
      offspring_dist = s,
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "not found"
  )
  expect_error(
    simulate_tree(
      ntrees = 2,
      offspring_dist = "pois",
      statistic = "size",
      lambda = 0.9,
      generation_time = c(1, 2)
    ),
    "must be a function"
  )
  expect_error(
    simulate_tree(
      ntrees = 2,
      offspring_dist = c(1, 2),
      statistic = "length",
      lambda = 0.9
    ),
    "character string"
  )
  expect_error(
    simulate_tree(
      ntrees = 2,
      offspring_dist = "pois",
      statistic = "size",
      lambda = 0.9,
      tf = 5
    ),
    "must be specified"
  )
})

test_that("simulate_summary throws errors", {
  expect_error(
    simulate_summary(
      ntrees = 2,
      offspring_dist = "s",
      statistic = "length",
      lambda = 0.9
    ),
    "does not exist"
  )
  expect_error(
    simulate_summary(
      ntrees = 2,
      offspring_dist = "lnorm",
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "must return integers"
  )
  expect_error(
    simulate_summary(
      ntrees = 2,
      offspring_dist = s,
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "not found"
  )
  expect_error(
    simulate_summary(
      ntrees = 2,
      offspring_dist = c(1, 2),
      statistic = "length",
      lambda = 0.9
    ),
    "character string"
  )
})

test_that("simulate_tree_from_pop throws errors", {
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "binom",
      offspring_mean = 0.5,
      generation_time = generation_time_fn
    ),
    "should be one of"
  )
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "nbinom",
      mu = 0.5,
      size = 0.9,
      generation_time = generation_time_fn
    ),
    "> 1"
  )
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = p,
      offspring_mean = 0.5,
      offspring_disp = 0.9,
      generation_time = generation_time_fn
    ),
    "not found"
  )
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "nbinom",
      offspring_mean = 0.5,
      generation_time = generation_time_fn
    ),
    "must be specified"
  )
})

test_that("simulate_tree is numerically correct", {
  set.seed(12)
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_tree(
    ntrees = 2,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 0.9
  )
  #' summarise the results
  tree_sim_summary <- summary(tree_sim_raw)
  #' Expectations
  expect_identical(
    tree_sim_summary$chains_run,
    2.00
  )
  expect_identical(
    tree_sim_summary$unique_infectors,
    2L
  )
  expect_identical(
    tree_sim_summary$max_generation,
    3L
  )
  expect_identical(
    tree_sim_raw$infectee_id,
    c(1L, 2L, 2L, 2L, 2L, 2L, 2L)
  )
  expect_identical(
    tree_sim_raw$sim_id,
    c(1, 1, 2, 3, 4, 5, 6)
  )
  expect_identical(
    tree_sim_raw$infector_id,
    c(NA, NA, 1, 1, 2, 2, 2)
  )
  expect_identical(
    tree_sim_raw$generation,
    c(1L, 1L, 2L, 2L, 3L, 3L, 3L)
  )
  #' Expectations for intervention simulation
  expect_identical(
    tree_sim_summary$chains_run,
    2.0
  )
  expect_identical(
    tree_sim_summary$unique_infectors,
    2L
  )
  expect_identical(
    tree_sim_summary$max_generation,
    3L
  )
  expect_identical(
    tree_sim_raw$infectee_id,
    c(1L, 2L, 2L, 2L, 2L, 2L, 2L)
  )
  expect_identical(
    tree_sim_raw$sim_id,
    c(1, 1, 2, 3, 4, 5, 6)
  )
  expect_identical(
    tree_sim_raw$infector_id,
    c(NA, NA, 1, 1, 2, 2, 2)
  )
  expect_identical(
    tree_sim_raw$generation,
    c(1L, 1L, 2L, 2L, 3L, 3L, 3L)
  )
})

test_that("simulate_summary is numerically correct", {
  set.seed(12)
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    ntrees = 2,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 0.9
  )
  #' Summarise the results
  chain_summary_summaries <- summary(chain_summary_raw)
  #' Expectations
  expect_identical(
    chain_summary_summaries$chains_run,
    2.00
  )
  expect_identical(
    chain_summary_summaries$max_chain_stat,
    3.00
  )
  expect_identical(
    chain_summary_summaries$min_chain_stat,
    1.00
  )
  expect_identical(
    as.vector(chain_summary_raw),
    c(1.00, 3.00)
  )
})

test_that("simulate_tree_from_pop is numerically correct", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Summarise the results
  susc_outbreak_summary <- summary(susc_outbreak_raw)
  #' Expectations
  expect_identical(
    susc_outbreak_summary$unique_infectors,
    0L
  )
  expect_identical(
    susc_outbreak_summary$max_time,
    0.00
  )
  expect_identical(
    susc_outbreak_summary$max_generation,
    1L
  )
  expect_null(susc_outbreak_summary$chains_run)
  expect_identical(
    susc_outbreak_raw$sim_id,
    1L
  )
  expect_identical(
    susc_outbreak_raw$infector_id,
    NA_integer_
  )
  expect_identical(
    susc_outbreak_raw$generation,
    1L
  )
  expect_identical(
    susc_outbreak_raw$time,
    0.00
  )
})
