test_that("We can calculate probabilities and sample", {
  expect_gt(dborel(1, 0.5), 0)
  expect_identical(dborel(1, 0.5, log = TRUE), -0.5)
  expect_length(rborel(2, 0.9), 2)
})

test_that("Errors are thrown", {
  expect_error(dborel(0, 0.5), "is not >= 1")
})
