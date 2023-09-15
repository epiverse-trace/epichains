# Define global variables and options for simulations
set.seed(12)
serial_func <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}

# simulate_tree()
tree_sim_raw <- simulate_tree(
  nchains = 2,
  offspring_dist = "pois",
  statistic = "length",
  lambda = 0.9
)

tree_sim_summary <- summary(tree_sim_raw)

# simulate_summary()
chain_summary_raw <- simulate_summary(
  nchains = 2,
  offspring_dist = "pois",
  statistic = "length",
  lambda = 0.9
)

chain_summary_sim <- summary(chain_summary_raw)

# simulate_tree_from_pop()
susc_outbreak_raw <- simulate_tree_from_pop(
  pop = 100,
  offspring_dist = "pois",
  offspring_mean = 0.9,
  serials_dist = serial_func
)

susc_outbreak_raw2 <- simulate_tree_from_pop(
  pop = 100,
  offspring_dist = "nbinom",
  offspring_mean = 1,
  offspring_disp = 1.1,
  serials_dist = serial_func
)

susc_outbreak_summary <- summary(susc_outbreak_raw)

test_that("Simulators return epichains objects", {
  expect_s3_class(
    tree_sim_raw,
    "epichains"
  )
  expect_s3_class(
    susc_outbreak_raw,
    "epichains"
  )
  expect_s3_class(
    chain_summary_raw,
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
    nrow(tree_sim_raw),
    2)
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
        nchains = 10,
        statistic = "size",
        offspring_dist = "pois",
        stat_max = 10,
        serials_dist = function(x) 3,
        lambda = 2,
        tf = 5
      )$time < 5
    )
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
  set.seed(123)
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
  expect_error(
    simulate_tree_from_pop(
      pop = 100,
      offspring_dist = "nbinom",
      offspring_mean = 0.5,
      serials_dist = serial_func
    ),
    "must be specified"
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
  expect_identical(
    tree_sim_summary$chains_ran,
    2.00
  )
  expect_identical(
    tree_sim_summary$unique_ancestors,
    2L
  )
  expect_identical(
    tree_sim_summary$max_generation,
    3L
  )
})

test_that("simulate_summary is numerically correct", {
  expect_identical(
    chain_summary_sim$max_chain_stat,
    3.00
  )
  expect_identical(
    chain_summary_sim$min_chain_stat,
    1.00
  )
  expect_identical(
    as.vector(chain_summary_raw),
    c(2.00, 1.00)
  )
})

test_that("simulate_tree_from_pop is numerically correct", {
  expect_identical(
    susc_outbreak_summary$unique_ancestors,
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
  expect_null(susc_outbreak_summary$chains_ran)
})
