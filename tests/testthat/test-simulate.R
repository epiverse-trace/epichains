#' Define global variables and options for simulations
generation_time_fn <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}

test_that("simulate_summary has expected shape", {
  set.seed(12)
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
})

test_that("simulate_chains has expected shape", {
  # Simulate a tree of infections in an infinite population and with
  # no generation time
  set.seed(32) # this seed gives a sizeable outcome
  sim_chains_raw <- simulate_chains(
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "length",
    stat_max = 10,
    lambda = 2
  )
  # Simulate a tree of infections in an infinite population and with
  # generation time
  set.seed(32) # this seed gives a sizeable outcome
  sim_chains_raw_gt <- simulate_chains(
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  # Simulate a tree of infections in a finite population and with
  # generation time
  set.seed(32) # this seed gives a sizeable outcome
  sim_chains_small_susc <- simulate_chains(
    pop = 100,
    percent_immune = 0.50,
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 2,
    generation_time = generation_time_fn
  )
  # Simulate a tree of infections in an infinite population and with
  # a generation time terminated at time 5 (i.e., tf = 5)
  set.seed(32)
  sim_chains_max_tf <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = "pois",
    lambda = 2,
    stat_max = 10,
    generation_time = generation_time_fn,
    tf = 5
  )
  # Check the class
  sim_objs <- list(
    sim_chains_raw,
    sim_chains_raw_gt,
    sim_chains_small_susc,
    sim_chains_max_tf
  )
  expect_identical(
    unlist(
      lapply(
        sim_objs,
        class
      )
    ),
    rep(c("epichains_tree", "data.frame"), times = 4)
  )
  # Check column names
  expect_named(
    sim_chains_raw,
    c("infectee_id", "sim_id", "infector_id", "generation"),
    ignore.order = TRUE
  )
  # Don't expect "time" as a column name
  expect_false(
    ("time" %in% names(sim_chains_raw))
  )
  #' Cols of sim_chains_raw_gt have an extra column "time"
  expect_named(
    sim_chains_raw_gt,
    c("infectee_id", "sim_id", "infector_id", "generation", "time"),
    ignore.order = TRUE
  )
  # If simulated from a finite population, expect "susc_pop" as a column name
  expect_named(
    sim_chains_small_susc,
    c("infectee_id", "sim_id", "infector_id", "generation", "time", "susc_pop")
  )
  expect_identical(
    nrow(sim_chains_raw),
    11824L
  )
  # check that the infectors are the same as what was input
  expect_identical(
    unique(sim_chains_raw$infectee_id),
    as.integer(1:10)
  )
  # check column types of sim_chains_small_susc as it has the complete set of
  # columns
  expect_identical(
    apply(
      sim_chains_small_susc,
      MARGIN = 2,
      class,
      simplify = FALSE
    ),
    list(
      infectee_id = "numeric",
      sim_id = "numeric",
      infector_id = "numeric",
      generation = "numeric",
      time = "numeric",
      susc_pop = "numeric"
    )
  )
  # tf = 5, so all generation times should be less than 5
  expect_true(
    all(
      sim_chains_max_tf$time < 5
    )
  )
})

test_that("simulate_chains throws errors", {
  expect_error(
    simulate_chains(
      index_cases = 0,
      offspring_dist = "pois",
      statistic = "length",
      lambda = 0.9
    ),
    "Assertion on 'index_cases' failed: Must be >= 1"
  )
  expect_error(
    simulate_chains(
      index_cases = 0.1,
      offspring_dist = "pois",
      statistic = "length",
      lambda = 0.9
    ),
    "Must be of type 'count', not 'double'"
  )
  expect_error(
    simulate_chains(
      index_cases = 1,
      offspring_dist = "pois",
      statistic = "length",
      lambda = 0.9,
      pop = 0
    ),
    "not >= 1"
  )
  expect_error(
    simulate_chains(
      index_cases = 2,
      statistic = "length",
      offspring_dist = "s",
      lambda = 0.9
    ),
    "object 'rs' not found"
  )
  expect_error(
    simulate_chains(
      index_cases = 2,
      statistic = "length",
      offspring_dist = "lnorm",
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "must return integers"
  )
  expect_error(
    simulate_chains(
      index_cases = 2,
      statistic = "length",
      offspring_dist = s,
      meanlog = 0.9,
      sdlog = 0.9
    ),
    "not found"
  )
  expect_error(
    simulate_chains(
      index_cases = 2,
      statistic = "size",
      offspring_dist = "pois",
      lambda = 0.9,
      generation_time = c(1, 2)
    ),
    "Must be a function"
  )
  expect_error(
    simulate_chains(
      index_cases = 2,
      statistic = "length",
      offspring_dist = c(1, 2),
      lambda = 0.9
    ),
    "Must be of type"
  )
  expect_error(
    simulate_chains(
      index_cases = 2,
      statistic = "size",
      offspring_dist = "pois",
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
    "not found"
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
    "Must be of type"
  )
})

test_that("simulate_chains is numerically correct", {
  set.seed(32) # this seed gives a sizeable outcome
  sim_chains_small_susc <- simulate_chains(
    pop = 100,
    percent_immune = 0.50,
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 2,
    generation_time = generation_time_fn
  )
  #' summarise the results
  sim_chains_summary <- summary(sim_chains_small_susc)
  #' Expectations
  expect_identical(
    sim_chains_summary$ntrees,
    10.00
  )
  expect_identical(
    sim_chains_summary$unique_infectors,
    5L
  )
  expect_identical(
    sim_chains_summary$max_generation,
    5L
  )
  expect_identical(
    max(sim_chains_small_susc$sim_id),
    7
  )
})

test_that("simulate_chains produces expected snapshots", {
  set.seed(32) # this seed gives a sizeable outcome
  sim_chains_finite_pop <- simulate_chains(
    pop = 100,
    percent_immune = 0.50,
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 2,
    generation_time = generation_time_fn
  )
  set.seed(32)
  sim_chains_inf_susc <- simulate_chains(
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "size",
    stat_max = 100,
    lambda = 2,
    generation_time = generation_time_fn
  )
  # Simulate a tree of infections in a small population so that
  # we encounter the case where we have more potential offspring than
  # susceptible individuals
  set.seed(32) # this seed gives a sizeable outcome
  sim_chains_small_pop <- simulate_chains(
    pop = 11,
    percent_immune = 0,
    index_cases = 10,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 2
  )
  #' snapshots
  expect_snapshot(sim_chains_finite_pop)
  expect_snapshot(sim_chains_inf_susc)
  expect_snapshot(sim_chains_small_pop)
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
    chain_summary_summaries$ntrees,
    2.00
  )
  expect_identical(
    chain_summary_summaries$max_stat,
    3.00
  )
  expect_identical(
    chain_summary_summaries$min_stat,
    1.00
  )
  expect_identical(
    as.vector(chain_summary_raw),
    c(1.00, 3.00)
  )
})
