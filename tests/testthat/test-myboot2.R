library(testthat)

test_that("myboot2 produces valid output", {
  set.seed(123) # For reproducibility

  # Test with mean as the function
  result <- myboot2(iter = 1000, x = rnorm(100, mean = 5, sd = 2), fun = mean, alpha = 0.05)

  # Check structure of the result
  expect_named(result, c("ci", "fun", "x"))
  expect_type(result$ci, "double")
})
