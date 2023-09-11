test_that("Helper functions work correctly", {
  expect_equal(
    construct_offspring_ll_name(
      offspring_dist = "pois",
      chain_statistic = "size"
      ),
    "pois_size_ll"
  )
})
