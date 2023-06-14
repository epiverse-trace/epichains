chains <- c(1, 1, 4, 7)
test_that("Analytical size or length distributions are implemented", {
  expect_true(all(pois_size_ll(chains, lambda = 0.5) < 0))
  expect_true(all(nbinom_size_ll(chains, mu = 0.5, size = 0.2) < 0))
  expect_true(all(nbinom_size_ll(chains, prob = 0.5, size = 0.2) < 0))
  expect_true(all(gborel_size_ll(chains, prob = 0.5, size = 0.2) < 0))
  expect_true(all(gborel_size_ll(chains, prob = 0.5, size = 0.2) < 0))
  expect_true(all(pois_length_ll(chains, lambda = 0.5) < 0))
  expect_true(all(geom_length_ll(chains, prob = 0.5) < 0))
})


