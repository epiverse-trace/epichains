test_that("Checks work", {
  expect_error(
    check_offspring_func_valid("rrpois"),
    "not found"
  )
  expect_error(
    check_generation_time_valid("a"),
    "Must be a function"
  )
  expect_error(
    check_generation_time_valid(function(x) rep("a", 10)),
    "numeric"
  )
  expect_error(
    check_generation_time_valid(function(x) 3),
    "Must have length"
  )
})
