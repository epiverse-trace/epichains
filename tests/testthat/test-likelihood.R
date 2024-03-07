test_that("Likelihoods can be calculated", {
  chains <- c(1, 1, 4, 7)
  set.seed(12)
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = rpois,
        lambda = 0.5
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = rpois,
        lambda = 0.5,
        exclude = 1
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = rpois,
        lambda = 0.5,
        stat_threshold = 5
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = rpois,
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
        offspring_dist = rpois,
        lambda = 0.5,
        stat_threshold = 5,
        obs_prob = 0.5,
        nsim_obs = 1
      ),
      0
    )
    expect_lt(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = rbinom,
        size = 1,
        prob = 0.5
      ),
      0
    )
    expect_gte(
      likelihood(
        chains = chains,
        statistic = "length",
        offspring_dist = rbinom,
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
        offspring_dist = rbinom,
        size = 1,
        prob = 0.5,
        individual = FALSE,
        log = FALSE
      ),
      0
    )
  }
)

test_that("likelihood() works with epichains and epichains_summary objects", {
    # Simulate an <epichains_tree> object
    set.seed(32)
    chains_tree_eg <- simulate_chains(
      index_cases = 10,
      pop = 100,
      percent_immune = 0,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      generation_time = function(n) rep(3, n),
      lambda = 0.9
    )
    # Simulate an <epichains_summary> object
    set.seed(32)
    chains_summary_eg <- simulate_summary(
      index_cases = 10,
      pop = 100,
      percent_immune = 0,
      statistic = "size",
      offspring_dist = rpois,
      stat_max = 10,
      lambda = 0.9
    )
    # Use the simulated <epichains_tree> object to calculate likelihood
    expect_equal(
      likelihood(
        chains = chains_tree_eg,
        statistic = "size",
        offspring_dist = rpois,
        lambda = 0.9,
        stat_max = 10
      ),
      -23.538996774
    )
    # Use the simulated <epichains_summary> object to calculate likelihood
    expect_equal(
      likelihood(
        chains = chains_summary_eg,
        statistic = "size",
        offspring_dist = rpois,
        lambda = 0.9,
        stat_max = 10
      ),
      -23.538997
    )
})

test_that("Likelihoods are numerically correct", {
  chains <- c(1, 1, 4, 7)
  set.seed(12)
  expect_identical(
    round(
      likelihood(
        chains = chains,
        statistic = "size",
        offspring_dist = rpois,
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
        offspring_dist = rnbinom,
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
        offspring_dist = rgborel,
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
        offspring_dist = rpois,
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
        offspring_dist = rgeom,
        prob = 0.5
      ), 5
    ),
    -12.48639
  )
})

test_that("Errors are thrown", {
  chains <- c(1, 1, 4, 7)
  set.seed(12)
  expect_error(
    likelihood(
      chains = chains,
      offspring_dist = list(),
      statistic = "size",
      lambda = 0.5
    ),
    "Must be a function"
  )
  expect_error(
    likelihood(
      chains = chains,
      offspring_dist = rpois,
      statistic = "size",
      lambda = 0.5,
      obs_prob = 3
    ),
    "is not <= 1"
  )
  expect_error(
    likelihood(
      chains = chains,
      offspring_dist = rpois,
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
      offspring_dist = rpois,
      nsim_obs = 100,
      lambda = 0.5,
      log = "s"
    ),
    "Must be of type"
  )
  expect_error(
    likelihood(
      chains = as.factor(chains),
      statistic = "size",
      offspring_dist = rpois,
      lambda = 0.5
    ),
    "Must be of type"
  )
})
