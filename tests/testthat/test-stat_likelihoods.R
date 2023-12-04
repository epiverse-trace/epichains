set.seed(1231)
chains <- c(1, 1, 4, 7)
test_that("Analytical chain size distributions are numerically correct", {
  expect_identical(
    round(
      nbinom_size_ll(
        x = chains,
        mu = 0.5,
        size = 0.2
      ),
      5
    ),
    c(-0.25055, -0.25055, -3.79542, -4.83785)
  )
  expect_identical(
    round(
      nbinom_size_ll(
        x = chains,
        prob = 0.5,
        size = 0.2
      ),
      5
    ),
    c(-0.13863, -0.13863, -4.41775, -6.19443)
  )
  expect_identical(
    round(
      gborel_size_ll(
        x = chains,
        mu = 0.5,
        size = 0.2
      ),
      5
    ),
    c(-0.25055, -0.25055, -4.58222, -5.83390)
  )
  expect_identical(
    round(
      gborel_size_ll(
        x = chains,
        prob = 0.5,
        size = 0.2
      ),
      5
    ),
    c(-0.13863, -0.13863, -4.80803, -6.13400)
  )
  expect_identical(
    round(
      pois_size_ll(
        x = chains,
        lambda = 0.2
      ),
      5
    ),
    c(-0.20000, -0.20000, -4.64748, -7.90633)
  )
})

test_that("Analytical chain lengths distributions are numerically correct", {
  expect_identical(
    round(
      pois_length_ll(
        x = chains,
        lambda = 0.5
      ),
      5
    ),
    c(-0.50000, -0.50000, -3.13243, -5.26702)
  )
  expect_identical(
    round(
      geom_length_ll(
        x = chains,
        prob = 0.5
      ),
      5
    ),
    c(-1.09861, -1.09861, -4.06260, -6.22657)
  )
})

test_that("Generic offspring log-likelihoods are calculated", {
  expect_true(
    all(
      offspring_ll(
        x = chains,
        offspring_dist = "pois",
        nsim_offspring = 100,
        statistic = "size",
        lambda = 0.82
      ) < 0
    )
  )
  expect_length(
    offspring_ll(
      x = chains,
      offspring_dist = "pois",
      nsim_offspring = 100,
      statistic = "size",
      lambda = 0.82
    ),
    4
  )
})

test_that("Errors are thrown", {
  expect_error(
    nbinom_size_ll(
      x = chains,
      mu = 0.5,
      size = 0.2,
      prob = 0.1
    ),
    "both specified"
  )
  expect_error(
    gborel_size_ll(
      x = chains,
      mu = 0.5,
      size = 0.2,
      prob = 0.1
    ),
    "both specified"
  )
})

test_that("Likelihood function returns the right object classes", {
  expect_type(
    likelihood(
      chains = chains,
      statistic = "size",
      offspring_dist = "pois",
      nsim_obs = 100,
      lambda = 0.5,
      obs_prob = 0.5,
      individual = TRUE
    ),
    "list"
  )
  expect_type(
    likelihood(
      chains = chains,
      statistic = "size",
      offspring_dist = "pois",
      nsim_obs = 3,
      lambda = 0.5,
      obs_prob = 0.5,
      individual = FALSE
    ),
    "double"
  )
  expect_type(
    likelihood(
      chains = chains,
      statistic = "size",
      offspring_dist = "pois",
      nsim_obs = 3,
      lambda = 0.5,
      obs_prob = 1,
      individual = TRUE
    ),
    "list"
  )
  expect_type(
    likelihood(
      chains = chains,
      statistic = "size",
      offspring_dist = "pois",
      nsim_obs = 100,
      lambda = 0.5,
      obs_prob = 1,
      individual = FALSE
    ),
    "double"
  )
})
