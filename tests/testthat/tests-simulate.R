# Define global variables and options for simulations
set.seed(12)
serial_func <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}

test_that("Simulators return epichains objects", {
  expect_s3_class(
    simulate_tree(
      nchains = 10,
      offspring_dist = "pois",
      lambda = 2,
      statistic = "size",
      stat_max = 10
    ),
    "epichains"
  )
  expect_s3_class(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "nbinom",
      offspring_mean = 0.5,
      offspring_disp = 1.1,
      serials_dist = function(x) 3
    ),
    "epichains"
  )
  expect_s3_class(
    simulate_summary(
      nchains = 10,
      offspring_dist = "pois",
      lambda = 2,
      stat_max = 10
    ),
    "epichains"
  )
})

test_that("Simulators work", {
  expect_length(
    simulate_summary(
      nchains = 2,
      statistic = "size",
      offspring_dist = "pois",
      lambda = 0.5
    ),
    2
  )
  expect_gte(
    nrow(
      simulate_tree(
        nchains = 2,
        offspring_dist = "pois",
        statistic = "length",
        lambda = 0.9
      )
    ),
    2
  )
  expect_gte(
    nrow(
      simulate_tree_from_pop(
        pop = 100,
        offspring_dist = "pois",
        offspring_mean = 0.9,
        serials_dist = serial_func
      )
    ),
    1
  )
})

test_that("simulate_tree throws errors", {
  expect_error(
    simulate_tree(
      nchains = 2,
      offspring_dist = "s",
      statistic = "length",
      lambda = 0.9
    ),
    "does not exist"
  )
  expect_error(
    simulate_tree(
      nchains = 2,
      offspring_dist = "lnorm",
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "must return integers"
  )
  expect_error(
    simulate_tree(
      nchains = 2,
      offspring_dist = s,
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "not found"
  )
  expect_error(
    simulate_tree(
      nchains = 2,
      offspring_dist = "pois",
      statistic = "size",
      lambda = 0.9,
      serials_dist = c(1, 2)
    ),
    "must be a function"
  )
  expect_error(
    simulate_tree(
      nchains = 2,
      offspring_dist = c(1, 2),
      statistic = "length",
      lambda = 0.9
    ),
    "character string"
  )
  expect_error(
    simulate_tree(
      nchains = 2,
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
      nchains = 2,
      offspring_dist = "s",
      statistic = "length",
      lambda = 0.9
    ),
    "does not exist"
  )
  expect_error(
    simulate_summary(
      nchains = 2,
      offspring_dist = "lnorm",
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "must return integers"
  )
  expect_error(
    simulate_summary(
      nchains = 2,
      offspring_dist = s,
      statistic = "length",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "not found"
  )
  expect_error(
    simulate_summary(
      nchains = 2,
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
      serials_dist = serial_func
    ),
    "should be one of"
  )
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "nbinom",
      offspring_mean = 0.5,
      offspring_disp = 0.9,
      serials_dist = serial_func
    ),
    "> 1"
  )
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = p,
      offspring_mean = 0.5,
      offspring_disp = 0.9,
      serials_dist = serial_func
    ),
    "not found"
  )
})

test_that("simulate_tree_from_pop throws warnings", {
  expect_warning(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "pois",
      offspring_mean = 3,
      offspring_disp = 1,
      serials_dist = serial_func
    ),
    "not used for poisson offspring"
  )
})

test_that("simulate_tree is numerically correct", {
  set.seed(12)
  tree_sim_summary <- summary(
    simulate_tree(
      nchains = 2,
      offspring_dist = "pois",
      statistic = "length",
      lambda = 0.9
    )
  )
  expect_equal(
    tree_sim_summary$chains_ran,
    2
  )
  expect_equal(
    tree_sim_summary$unique_ancestors,
    2
  )
  expect_equal(
    tree_sim_summary$max_generation,
    3
  )
})

test_that("simulate_summary is numerically correct", {
  set.seed(12)
  chain_summary_sim <- summary(
    simulate_summary(
      nchains = 2,
      offspring_dist = "pois",
      statistic = "length",
      lambda = 0.9
    )
  )
  expect_equal(
    chain_summary_sim$max_chain_stat,
    3
  )
  expect_equal(
    chain_summary_sim$min_chain_stat,
    1
  )
})

test_that("simulate_tree_from_pop is numerically correct", {
  set.seed(12)
  susc_outbreak_summary <- summary(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "pois",
      offspring_mean = 0.9,
      serials_dist = serial_func
    )
  )
  expect_equal(
    susc_outbreak_summary$unique_ancestors,
    0
  )
  expect_equal(
    susc_outbreak_summary$max_time,
    0
  )
  expect_equal(
    susc_outbreak_summary$max_generation,
    1
  )
  expect_equal(
    susc_outbreak_summary$chains_ran,
    NULL
  )
})
