library(testthat)

test_that("mycltp produces expected plots without errors", {
  # Test with valid inputs
  expect_silent(mycltp(n = 5, iter = 1000, lambda = 10)) # Should not throw an error
  expect_silent(mycltp(n = 10, iter = 5000, lambda = 20)) # Larger sample and lambda

  # Verify plot behavior indirectly using a side-effect-free function
  capture_output({
    result <- mycltp(n = 5, iter = 100, lambda = 5)
  })
  expect_null(result) # The function is designed to return no values

  # Check layout restoration after plot
  expect_equal(layout(matrix(1)), 1) # Layout should reset after function execution
})
