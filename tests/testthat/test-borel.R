test_that("We can calculate probabilities and sample", {
  set.seed(32)
  expect_gt(dborel(1, 0.5), 0)
  expect_identical(dborel(1, 0.5, log = TRUE), -0.5)
  expect_length(rborel(2, 0.9), 2)
  expect_length(rgborel(2, 0.5, 0.9), 2)
  expect_type(
    rborel(2, 0.9), "double"
  )
  expect_type(
    rgborel(2, 0.5, 0.9), "double"
  )
  expect_type(
    dborel(1, 0.5), "double"
  )
  expect_identical(
    rgborel(n = 5, size = 0.3, mu = 1, censor_at = 5),
    c(1, 1, Inf, 1, 1)
  )
  expect_error(
    rgborel(n = 5, size = 0.3, mu = 1, prob = 1, censor_at = 5),
    "'prob' and 'mu' both specified"
  )
})

test_that("Errors are thrown", {
  expect_error(dborel(0, 0.5), "is not >= 1")
})
