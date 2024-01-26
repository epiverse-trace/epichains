#' Define global variables and options for simulations
generation_time_fn <- function(n) {
  rlnorm(n, meanlog = 0.58, sdlog = 1.58)
}

test_that("Simulators return epichains objects", {
  set.seed(12)
  #' Simulate an outbreak from a finite population with pois offspring
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    lambda = 0.9,
    statistic = "size",
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a finite population with nbinom offspring
  susc_outbreak_raw2 <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rnbinom,
    statistic = "size",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections in an infinite population and with
  #' no generation time
  tree_sim_raw <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections in an infinite population and
  #' with generation times
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Expectations
  expect_s3_class(
    tree_sim_raw,
    "epichains_tree"
  )
  expect_s3_class(
    tree_sim_raw2,
    "epichains_tree"
  )
  expect_s3_class(
    susc_outbreak_raw,
    "epichains_tree"
  )
  expect_s3_class(
    susc_outbreak_raw2,
    "epichains_tree"
  )
  expect_s3_class(
    chain_summary_raw,
    "epichains_summary"
  )
})

test_that("print.epichains_tree works for simulation functions", {
  set.seed(32)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rnbinom,
    statistic = "size",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections with generation times
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = rpois,
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

test_that("summary.epichains_tree works as expected", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rnbinom,
    statistic = "size",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate case where all the chain statistics are Inf
  set.seed(11223)
  epichains_summary_all_infs <- simulate_summary(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    lambda = 3
  )
  #' Expectations
  expect_named(
    summary(tree_sim_raw),
    c(
      "index_cases",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(tree_sim_raw2),
    c(
      "index_cases",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(susc_outbreak_raw),
    c(
      "index_cases",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(susc_outbreak_raw2),
    c(
      "index_cases",
      "max_time",
      "unique_infectors",
      "max_generation"
    )
  )
  expect_named(
    summary(chain_summary_raw),
    c(
      "index_cases",
      "max_stat",
      "min_stat"
    )
  )
  expect_true(
    is.infinite(
      summary(epichains_summary_all_infs)$min_stat
    )
  )
  expect_true(
    is.infinite(
      summary(epichains_summary_all_infs)$max_stat
    )
  )
})

test_that("validate_epichains_tree works", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population (pois)
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rnbinom,
    statistic = "size",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Expectations
  expect_invisible(
    validate_epichains_tree(susc_outbreak_raw)
  )
  expect_invisible(
    validate_epichains_tree(susc_outbreak_raw2)
  )
  expect_invisible(
    validate_epichains_tree(tree_sim_raw)
  )
  expect_invisible(
    validate_epichains_tree(tree_sim_raw2)
  )
  expect_invisible(
    validate_epichains_summary(chain_summary_raw)
  )
})

test_that("is_chains_tree works", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rnbinom,
    statistic = "size",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Expectations
  expect_true(
    is_epichains_tree(susc_outbreak_raw)
  )
  expect_true(
    is_epichains_tree(susc_outbreak_raw2)
  )
  expect_true(
    is_epichains_tree(tree_sim_raw)
  )
  expect_true(
    is_epichains_tree(tree_sim_raw2)
  )
  expect_false(
    is_epichains_tree(chain_summary_raw)
  )
})

test_that("is_chains_summary works", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak from a susceptible population (nbinom)
  susc_outbreak_raw2 <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rnbinom,
    statistic = "size",
    mu = 1,
    size = 1.1,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections without serials
  tree_sim_raw <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Simulate a tree of infections with serials
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Simulate chain statistics
  chain_summary_raw <- simulate_summary(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Expectations
  expect_true(
    is_epichains_summary(chain_summary_raw)
  )
  expect_false(
    is_epichains_summary(susc_outbreak_raw)
  )
  expect_false(
    is_epichains_summary(susc_outbreak_raw2)
  )
  expect_false(
    is_epichains_summary(tree_sim_raw)
  )
  expect_false(
    is_epichains_summary(tree_sim_raw2)
  )
})

test_that("aggregate.epichains_tree method returns correct objects", {
  set.seed(32)
  #' Simulate transmission chains in an infinite population
  chain_sim <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Create aggregates
  aggreg_by_gen <- aggregate(
    chain_sim,
    by = "generation"
  )
  aggreg_by_time <- aggregate(
    chain_sim,
    by = "time"
  )
  #' Expectations for aggregated <epichains_tree>
  expect_named(
    aggreg_by_gen,
    c("generation", "cases")
  )
  expect_named(
    aggreg_by_time,
    c("time", "cases")
  )
})

test_that("aggregate.epichains_tree method throws errors", {
  expect_error(
    aggregate(
      simulate_chains(
        index_cases = 10,
        statistic = "size",
        offspring_dist = rpois,
        stat_max = 10,
        lambda = 2
      ),
      by = "time"
    ),
    "Object must have a time column"
  )
})

test_that("aggregate.epichains_tree method is numerically correct", {
  set.seed(12)
  #' Simulate a tree of infections in an infinite population and without
  #' generation times
  tree_sim_raw <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    lambda = 2
  )
  #' Simulate a tree of infections in an infinite population and with
  #' generation times
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  #' Create aggregates
  aggreg_by_gen <- aggregate(
    tree_sim_raw,
    by = "generation"
  )
  aggreg_by_time <- aggregate(
    tree_sim_raw2,
    by = "time"
  )
  expect_identical(
    aggreg_by_gen$cases,
    c(10L, 12L, 19L, 26L, 14L)
  )
  expect_identical(
    aggreg_by_time$cases,
    as.integer(c(10, rep(1, 111)))
  )
})

test_that("head and tail print output as expected", {
  set.seed(12)
  #' Simulate an outbreak from a susceptible population
  susc_outbreak_raw <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate a tree of infections in an infinite population
  tree_sim_raw2 <- simulate_chains(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rpois,
    stat_max = 10,
    generation_time = generation_time_fn,
    lambda = 2
  )
  expect_snapshot(head(susc_outbreak_raw))
  expect_snapshot(head(tree_sim_raw2))
  expect_snapshot(tail(susc_outbreak_raw))
  expect_snapshot(tail(tree_sim_raw2))
})

test_that("head and tail return data.frames", {
  set.seed(12)
  #' Simulate an outbreak from a finite population and with generation times
  outbreak_finite_pop <- simulate_chains(
    pop = 100,
    index_cases = 10,
    offspring_dist = rpois,
    statistic = "size",
    lambda = 0.9,
    generation_time = generation_time_fn
  )
  #' Simulate an outbreak in an infinite population and
  #' without generation times
  outbreak_infinite_pop <- simulate_chains(
    index_cases = 2,
    offspring_dist = rpois,
    statistic = "length",
    lambda = 0.9
  )
  #' Expectations
  expect_s3_class(
    head(outbreak_finite_pop),
    "data.frame"
  )
  expect_s3_class(
    head(outbreak_infinite_pop),
    "data.frame"
  )
  expect_s3_class(
    tail(outbreak_finite_pop),
    "data.frame"
  )
  expect_s3_class(
    tail(outbreak_infinite_pop),
    "data.frame"
  )
})
