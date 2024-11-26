library(testthat)

test_that("myci produces valid 95% confidence intervals", {
  set.seed(123) # For reproducibility

  # Test with a simple example
  result <- myci(c(1, 2, 3, 4, 5))
  expect_type(result, "double")
  expect_length(result, 2) # CI should have two bounds
  expect_lt(result[1], result[2]) # Lower bound should be less than upper bound

  # Compare with a manual calculation
  x <- c(1, 2, 3, 4, 5)
  t <- qt(1 - 0.05 / 2, length(x) - 1)
  expected_ci <- mean(x) + c(-1, 1) * t * sd(x) / sqrt(length(x))
  expect_equal(result, expected_ci, tolerance = 1e-8)

  # Test with a larger sample
  x <- rnorm(100, mean = 10, sd = 5)
  result <- myci(x)
  expect_type(result, "double")
  expect_length(result, 2)
  expect_lt(result[1], result[2])
})
