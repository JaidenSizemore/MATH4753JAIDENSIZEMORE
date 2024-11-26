# Load the necessary package for testing
library(testthat)

# Test file for the ntickets function
test_that("ntickets function works correctly", {

  # Set known parameters for the test
  N <- 200
  gamma <- 0.02
  p <- 0.95

  # Run the function with the test parameters
  result <- ntickets(N = N, gamma = gamma, p = p)

  # Check if the result contains all necessary keys: nd, nc, N, p, gamma
  expect_named(result, c("nd", "nc", "N", "p", "gamma"))

  # Check if nd and nc are numeric
  expect_type(result$nd, "integer")
  expect_type(result$nc, "double")

  # Test that nd is greater than N and nc is within the expected range
  expect_gt(result$nd, N)  # nd should be greater than N
  expect_lt(result$nc, N + N * 0.1)  # nc should be less than N + 10% of N

  # Check that the values for N, p, and gamma are numeric
  expect_type(result$N, "double")
  expect_type(result$p, "double")
  expect_type(result$gamma, "double")

  # Check that the discrete and continuous plots are generated correctly
  # The plots are visually inspected, so we won't check the exact plot details, but ensure the function runs without errors
  # Running the function should generate two plots, so we will use `expect_silent` to ensure no errors
  expect_silent(ntickets(N = N, gamma = gamma, p = p))

  # If you have specific expectations based on domain knowledge, you can also include specific value checks
  # These would require manually calculating the expected nd and nc values based on the parameters (N, gamma, p)
  # For example:
  # expect_equal(result$nd, expected_nd_value, tolerance = 0.1)
  # expect_equal(result$nc, expected_nc_value, tolerance = 0.1)
})
