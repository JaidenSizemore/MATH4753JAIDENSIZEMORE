library(testthat)

test_that("mybin produces valid outputs", {
  # Test with default parameters
  result <- mybin(iter = 100, n = 10, p = 0.5)

  # Check that the result is a named vector of proportions
  expect_type(result, "double")
  expect_equal(sum(result), 1) # Proportions should sum to 1

  # Test with other valid inputs
  result <- mybin(iter = 200, n = 15, p = 0.3)
  expect_type(result, "double")
  expect_equal(sum(result), 1) # Proportions should sum to 1

  # Test edge cases
  result <- mybin(iter = 1, n = 1, p = 0.5)
  expect_type(result, "double")
  expect_equal(length(result), 2) # Should have two levels (0 and 1)

  # Test if large iter doesn't crash
  expect_no_error(mybin(iter = 1000, n = 20, p = 0.7))
})
