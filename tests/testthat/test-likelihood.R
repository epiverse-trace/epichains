chains <- c(1, 1, 4, 7)
test_that(
  "Likelihoods can be calculated",
  {
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "pois",
        lambda = 0.5
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "pois",
        lambda = 0.5,
        exclude = 1
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "pois",
        lambda = 0.5,
        stat_max = 5
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "pois",
        lambda = 0.5,
        obs_prob = 0.5,
        nsim_obs = 1
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "pois",
        lambda = 0.5,
        stat_max = 5,
        obs_prob = 0.5,
        nsim_obs = 1
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = "binom",
        size = 1,
        prob = 0.5
      ),
      0
    )
    expect_gte(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = "binom",
        size = 1,
        prob = 0.5,
        log = FALSE
      ),
      0
    )
    expect_gte(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = "binom",
        size = 1,
        prob = 0.5,
        individual = FALSE,
        log = FALSE
      ),
      0
    )
  }
)

test_that("Likelihoods are numerically correct", {
  expect_identical(
    round(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "pois",
        lambda = 0.5
      ), 5
    ),
    -8.6072
  )
  expect_identical(
    round(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "nbinom",
        mu = 0.5,
        size = 0.2
      ), 5
    ),
    -9.13437
  )
  expect_identical(
    round(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = "gborel",
        prob = 0.5,
        size = 0.2
      ), 5
    ),
    -11.21929
  )
  expect_identical(
    round(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = "pois",
        lambda = 0.5
      ), 5
    ),
    -9.39945
  )
  expect_identical(
    round(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = "geom",
        prob = 0.5
      ), 5
    ),
    -12.48639
  )
})

test_that("Errors are thrown", {
  expect_error(
    likelihood(
      chains = chains,
      offspring_dist = list(),
      statistic = "size",
      lambda = 0.5
    ),
    "Must be of type"
  )
  expect_error(
    likelihood(
      chains = chains,
      offspring_dist = "pois",
      statistic = "size",
      lambda = 0.5,
      obs_prob = 3
    ),
    "is not <= 1"
  )
  expect_error(
    likelihood(
      chains = chains,
      offspring_dist = "pois",
      statistic = "size",
      lambda = 0.5,
      obs_prob = 0.5
    ),
    "must be specified"
  )
  expect_error(
    likelihood(
      chains = chains,
      statistic = "size",
      offspring_dist = "pois",
      nsim_obs = 100,
      lambda = 0.5,
      log = "s"
    ),
    "Must be of type"
  )
})
