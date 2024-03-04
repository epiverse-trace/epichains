set.seed(32)
test_that("We can calculate probabilities and sample", {
  expect_gt(dborel(1, 0.5), 0)
  expect_identical(dborel(1, 0.5, log = TRUE), -0.5)
  expect_length(rborel(2, 0.9), 2)
  expect_length(rgborel(2, 0.5, 0.9), 2)
  expect_true(
    class(rborel(2, 0.9)) == "numeric"
  )
  expect_true(
    class(rgborel(2, 0.5, 0.9)) == "numeric"
  )
  expect_true(
    class(dborel(1, 0.5)) == "numeric"
  )
})

test_that("Errors are thrown", {
  expect_error(dborel(0, 0.5), "is not >= 1")
})
