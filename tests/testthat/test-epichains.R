set.seed(12)
epichains_summary <- simulate_summary(
  nchains = 10,
  statistic = "size",
  offspring_dist = "pois",
  stat_max = 10,
  lambda = 2
)
epichains_tree <- simulate_tree(
  nchains = 10,
  statistic = "size",
  offspring_dist = "pois",
  stat_max = 10,
  lambda = 2
)
epichains_tree2 <- simulate_tree(
  nchains = 10,
  statistic = "size",
  offspring_dist = "pois",
  stat_max = 10,
  serials_dist = function(x) 3,
  lambda = 2
)

aggreg_by_gen <- aggregate(
  epichains_tree,
  grouping_var = "generation"
)
aggreg_by_time <- aggregate(
  epichains_tree2,
  grouping_var = "time"
)

aggreg_by_both <- aggregate(
  epichains_tree2,
  grouping_var = "both"
)

set.seed(11223)
epichains_summary_all_infs <- simulate_summary(
  nchains = 10,
  statistic = "size",
  offspring_dist = "pois",
  stat_max = 10,
  lambda = 3
)

test_that("print.epichains works for simulate_summary output", {
  expect_snapshot(epichains_summary)
})

test_that("print.epichains works for simulate_tree output", {
  expect_snapshot(epichains_tree)
})

test_that("print.epichains works for simulate_tree output", {
  expect_snapshot(epichains_tree2)
})

test_that("summary.epichains works as expected", {
  expect_named(
    summary(epichains_summary),
    c(
      "chain_ran",
      "max_chain_stat",
      "min_chain_stat"
    )
  )
  expect_named(
    summary(epichains_tree2),
    c(
      "chains_ran",
      "max_time",
      "unique_ancestors",
      "max_generation"
    )
  )
  expect_named(
    summary(epichains_tree2),
    c(
      "chains_ran",
      "max_time",
      "unique_ancestors",
      "max_generation"
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
  expect_invisible(
    validate_epichains(epichains_summary)
  )
  expect_invisible(
    validate_epichains(epichains_tree)
  )
  expect_invisible(
    validate_epichains(epichains_tree2)
  )
})

test_that("is_chains_tree works", {
  expect_true(
    is_chains_tree(epichains_tree)
  )
  expect_true(
    is_chains_tree(epichains_tree2)
  )
  expect_false(
    is_chains_tree(epichains_summary)
  )
})

test_that("is_chains_summary works", {
  expect_true(
    is_chains_tree(epichains_tree)
  )
  expect_true(
    is_chains_tree(epichains_tree2)
  )
  expect_false(
    is_chains_tree(epichains_summary)
  )
})

test_that("is_epichains_aggregate_df works", {
  expect_true(
    is_epichains_aggregate_df(aggreg_by_gen)
  )
  expect_true(
    is_epichains_aggregate_df(aggreg_by_time)
  )
  expect_true(
    is_epichains_aggregate_df(aggreg_by_both)
  )
  expect_false(
    is_epichains_aggregate_df(epichains_tree)
  )
})

test_that("validate_epichains throws errors", {
  expect_error(
    validate_epichains(mtcars),
    "must have an epichains class"
  )
})

test_that("head and tail methods work", {
  expect_snapshot(head(epichains_tree))
  expect_snapshot(head(epichains_tree2))
  expect_snapshot(tail(epichains_tree))
  expect_snapshot(tail(epichains_tree2))
})

test_that("aggregate method work", {
  expect_named(
    aggreg_by_gen,
    c("generation", "cases")
  )
  expect_named(
    aggreg_by_time,
    c("time", "cases")
  )
  expect_identical(
    as.vector(
      vapply(aggreg_by_both, names, FUN.VALUE = character(2))
    ),
    c("time", "cases", "generation", "cases")
  )
  expect_s3_class(
    aggreg_by_gen,
    "epichains_aggregate_df"
  )
  expect_s3_class(
    aggreg_by_time,
    "epichains_aggregate_df"
  )
  expect_s3_class(
    aggreg_by_both,
    "epichains_aggregate_df"
  )
  expect_error(
    aggregate(epichains_summary),
    "attribute"
  )
})

test_that("aggregate method is numerically correct", {
  expect_identical(
    aggreg_by_gen$cases,
    c(10L, 17L, 38L, 38L, 12L)
  )
  expect_identical(
    aggreg_by_time$cases,
    c(10L, 21L, 48L, 60L)
  )
})
