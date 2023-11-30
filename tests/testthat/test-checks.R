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
    check_generation_time_valid("a"),
    "must be a function"
  )
  expect_error(
    check_generation_time_valid(function(x) rep("a", 10)),
    "numeric"
  )
  expect_error(
    check_generation_time_valid(function(x) 3),
    "vector of length"
  )
  expect_error(
    check_ntrees_valid(1.1),
    "less than"
  )
})
