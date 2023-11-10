test_that("Reparametrized distributions work", {
  expect_length(
    rnbinom_mean_disp(
      n = 5,
      mn = 4,
      disp = 2
    ),
    5
  )
})

test_that("Log-probabilities work", {
  expect_length(
    complementary_logprob(x = 0),
    1
  )
  expect_length(
    complementary_logprob(x = -Inf),
    1
  )
  expect_length(
    complementary_logprob(x = -0.1),
    1
  )
})

test_that("Chain lengths sampler works", {
  expect_length(
    rgen_length(
      n = 1,
      x = c(1, 2, 3),
      prob = 0.3
    ),
    3
  )
})

test_that("Chain sizes sampler works", {
  expect_length(
    rbinom_size(
      n = 1,
      x = c(1, 2, 3),
      prob = 0.3
    ),
    3
  )
})

test_that("Reparametrized distributions are numerically correct", {
  set.seed(12)
  expect_identical(
    rnbinom_mean_disp(
      n = 5,
      mn = 4,
      disp = 2
    ),
    c(0, 2, 5, 2, 3)
  )
})

test_that("Log-probabilities are numerically correct", {
  expect_identical(
    complementary_logprob(x = 0),
    -Inf
  )
  expect_identical(
    complementary_logprob(x = -Inf),
    0
  )
  expect_lt(
    complementary_logprob(x = -0.1),
    0
  )
})

test_that("Chain lengths sampler is numerically correct", {
  set.seed(12)
  expect_identical(
    rgen_length(
      n = 1,
      x = c(1, 2, 3),
      prob = 0.3
    ),
    c(8, 9, 10)
  )
})

test_that("Chain sizes sampler is numerically correct", {
  set.seed(12)
  expect_identical(
    rbinom_size(
      n = 1,
      x = c(1, 2, 3),
      prob = 0.3
    ),
    c(1, 2, 3)
  )
})

test_that("Reparametrized distributions throw warnings", {
  expect_warning(
    rnbinom_mean_disp(
      n = 5,
      mn = 4,
      disp = 0.9
    ),
    "not >= 1"
  )
})

test_that("Log-probabilities throw warnings", {
  expect_warning(
    complementary_logprob(0.1),
    "is not <= 0"
  )
  expect_warning(
    complementary_logprob(Inf),
    "is not <= 0"
  )
})
