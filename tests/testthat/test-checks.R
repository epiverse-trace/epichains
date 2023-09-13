test_that("Checks work", {
  expect_error(
    check_offspring_valid(1),
    "character string"
  )
  expect_error(
    check_offspring_func_valid("rrpois"),
    "does not exist"
  )
  expect_error(
    check_serial_valid("a"),
    "must be a function"
  )
  expect_error(
    check_nchains_valid(1.1),
    "less than"
  )
})
