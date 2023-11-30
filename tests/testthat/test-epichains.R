#' Define global variables and options for simulations
serial_func <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}

test_that("Simulators return epichains objects", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
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
  expect_s3_class(
    tree_sim_raw,
    "epichains"
  )
  expect_s3_class(
    tree_sim_raw2,
    "epichains"
  )
  expect_s3_class(
    susc_outbreak_raw,
    "epichains"
  )
  expect_s3_class(
    susc_outbreak_raw2,
    "epichains"
  )
  expect_s3_class(
    chain_summary_raw,
    "epichains"
  )
})

test_that("print.epichains works for simulation functions", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
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
  expect_snapshot(susc_outbreak_raw)
  expect_snapshot(susc_outbreak_raw2)
  expect_snapshot(tree_sim_raw)
  expect_snapshot(tree_sim_raw2)
  expect_snapshot(chain_summary_raw)
})

test_that("summary.epichains works as expected", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    ntrees = 2,
    offspring_dist = "pois",
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate case where all the chain statistics are Inf
  set.seed(11223)
  epichains_summary_all_infs <- simulate_summary(
    ntrees = 10,
    statistic = "size",
    offspring_dist = "pois",
    stat_max = 10,
    lambda = 3
  )
  #' Expectations
  expect_named(
    summary(tree_sim_raw),
    c(
      "chains_run",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(tree_sim_raw2),
    c(
      "chains_run",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(susc_outbreak_raw),
    c(
      "chains_run",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(susc_outbreak_raw2),
    c(
      "chains_run",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(chain_summary_raw),
    c(
      "chains_run",
      "max_chain_stat",
      "min_chain_stat"
    )
  )
  expect_true(
    is.infinite(
      summary(epichains_summary_all_infs)$min_chain_stat
    )
  )
  expect_true(
    is.infinite(
      summary(epichains_summary_all_infs)$max_chain_stat
    )
  )
})

test_that("validate_epichains works", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
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
  expect_invisible(
    validate_epichains(susc_outbreak_raw)
  )
  expect_invisible(
    validate_epichains(susc_outbreak_raw2)
  )
  expect_invisible(
    validate_epichains(tree_sim_raw)
  )
  expect_invisible(
    validate_epichains(tree_sim_raw2)
  )
  expect_invisible(
    validate_epichains(chain_summary_raw)
  )
  expect_error(
    validate_epichains(mtcars),
    "must have an epichains class"
  )
})

test_that("is_chains_tree works", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
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
  expect_true(
    is_chains_tree(susc_outbreak_raw)
  )
  expect_true(
    is_chains_tree(susc_outbreak_raw2)
  )
  expect_true(
    is_chains_tree(tree_sim_raw)
  )
  expect_true(
    is_chains_tree(tree_sim_raw2)
  )
  expect_false(
    is_chains_tree(chain_summary_raw)
  )
})

test_that("is_chains_summary works", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
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
  expect_true(
    is_chains_summary(chain_summary_raw)
  )
  expect_false(
    is_chains_summary(susc_outbreak_raw)
  )
  expect_false(
    is_chains_summary(susc_outbreak_raw2)
  )
  expect_false(
    is_chains_summary(tree_sim_raw)
  )
  expect_false(
    is_chains_summary(tree_sim_raw2)
  )
})

test_that("aggregate.epichains method returns correct objects", {
  set.seed(12)
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_tree(
    ntrees = 10,
    statistic = "size",
    offspring_dist = "pois",
    stat_max = 10,
    generation_time = function(x) 3,
    lambda = 2
  )
  #' Create aggregates
  aggreg_by_gen <- aggregate(
    tree_sim_raw2,
    grouping_var = "generation"
  )
  aggreg_by_time <- aggregate(
    tree_sim_raw2,
    grouping_var = "time"
  )
  #' Expectations for <epichains_aggregate_df> class inheritance
  expect_true(
    is_epichains_aggregate_df(aggreg_by_gen)
  )
  expect_true(
    is_epichains_aggregate_df(aggreg_by_time)
  )
  expect_named(
    aggreg_by_gen,
    c("generation", "cases")
  )
  expect_named(
    aggreg_by_time,
    c("time", "cases")
  )
})

test_that("aggregate.epichains method throws errors", {
  expect_error(
    aggregate(
      simulate_tree(
        ntrees = 10,
        statistic = "size",
        offspring_dist = "pois",
        stat_max = 10,
        lambda = 2
      ),
      grouping_var = "time"
    ),
    "Object must have a time column"
  )
})

test_that("aggregate.epichains method is numerically correct", {
  set.seed(12)
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_tree(
    ntrees = 10,
    statistic = "size",
    offspring_dist = "pois",
    stat_max = 10,
    lambda = 2
  )
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_tree(
    ntrees = 10,
    statistic = "size",
    offspring_dist = "pois",
    stat_max = 10,
    generation_time = function(x) 3,
    lambda = 2
  )
  #' Create aggregates
  aggreg_by_gen <- aggregate(
    tree_sim_raw,
    grouping_var = "generation"
  )
  aggreg_by_time <- aggregate(
    tree_sim_raw2,
    grouping_var = "time"
  )
  expect_identical(
    aggreg_by_gen$cases,
    c(10L, 12L, 19L, 26L, 14L)
  )
  expect_identical(
    aggreg_by_time$cases,
    c(10L, 17L, 38L, 38L, 12L)
  )
})

test_that("head and tail print output as expected", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
    lambda = 2
  )
  expect_snapshot(head(susc_outbreak_raw))
  expect_snapshot(head(susc_outbreak_raw2))
  expect_snapshot(head(tree_sim_raw))
  expect_snapshot(head(tree_sim_raw2))
  expect_snapshot(tail(susc_outbreak_raw))
  expect_snapshot(tail(susc_outbreak_raw2))
  expect_snapshot(tail(tree_sim_raw))
  expect_snapshot(tail(tree_sim_raw2))
})

test_that("head and tail return data.frames", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "pois",
    lambda = 0.9,
    generation_time = serial_func
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_tree_from_pop(
    pop = 100,
    offspring_dist = "nbinom",
    mu = 1,
    size = 1.1,
    generation_time = serial_func
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
    generation_time = function(x) 3,
    lambda = 2
  )
  #' Expectations
  expect_s3_class(
    head(susc_outbreak_raw),
    "data.frame"
  )
  expect_s3_class(
    head(susc_outbreak_raw2),
    "data.frame"
  )
  expect_s3_class(
    head(tree_sim_raw),
    "data.frame"
  )
  expect_s3_class(
    head(tree_sim_raw2),
    "data.frame"
  )
  expect_s3_class(
    tail(susc_outbreak_raw),
    "data.frame"
  )
  expect_s3_class(
    tail(susc_outbreak_raw2),
    "data.frame"
  )
  expect_s3_class(
    tail(tree_sim_raw),
    "data.frame"
  )
  expect_s3_class(
    tail(tree_sim_raw2),
    "data.frame"
  )
})
