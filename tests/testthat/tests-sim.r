test_that("Simulators output epichains objects", {
  expect_s3_class(
    simulate_tree(nchains = 10,
                  offspring_dist = "pois",
                  lambda = 2,
                  statistic = "size",
                  stat_max = 10
                  ),
    "epichains"
    )
  expect_s3_class(
    simulate_tree_from_pop(pop = 100,
                           offspring_dist = "nbinom",
                           offspring_mean = 0.5,
                           offspring_disp = 1.1,
                           serial_sampler = function(x) 3
    ),
    "epichains"
  )
  expect_s3_class(
    simulate_vect(n = 10,
                  offspring_dist = "pois",
                  lambda = 2,
                  stat_max = 10
    ),
    "epichains"
  )
})
