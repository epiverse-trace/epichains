test_that("Simulators output epichains objects", {
  expect_s3_class(
    simulate_tree(nchains = 10,
                  offspring_sampler = "pois",
                  lambda = 2,
                  chain_statistic = "size",
                  chain_stat_max = 10
                  ),
    "epichains"
    )
  expect_s3_class(
    simulate_tree_from_pop(pop = 100,
                           offspring_sampler = "nbinom",
                           mean_offspring = 0.5,
                           disp_offspring = 1.1,
                           serial_sampler = function(x) 3
    ),
    "epichains"
  )
  expect_s3_class(
    simulate_vect(n = 10,
                  offspring_sampler = "pois",
                  lambda = 2,
                  chain_stat_max = 10
    ),
    "epichains"
  )
})
