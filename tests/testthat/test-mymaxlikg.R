library(testthat)

test_that("mymaxlikg computes the maximum likelihood and plots correctly", {
  # Define a simple log-likelihood function for testing
  logbin2 <- function(theta) {
    log(dbinom(3, prob = theta, size = 6)) + log(dbinom(5, prob = theta, size = 10))
  }

  # Test with valid inputs
  theta_seq <- seq(0, 1, length.out = 100)
  expect_silent(mymaxlikg(lfun = logbin2, theta = theta_seq))

  # Verify that the function returns the theta corresponding to the maximum likelihood
  result <- capture.output({
    max_theta <- mymaxlikg(lfun = logbin2, theta = theta_seq)
  })
  expect_true(max_theta >= 0 && max_theta <= 1) # Theta should be in the range [0, 1]

  # Test with a narrower theta range
  theta_seq <- seq(0.3, 0.7, length.out = 100)
  expect_silent(mymaxlikg(lfun = logbin2, theta = theta_seq))

  # Check the returned theta value for the narrower range
  result <- capture.output({
    max_theta <- mymaxlikg(lfun = logbin2, theta = theta_seq)
  })
  expect_true(max_theta >= 0.3 && max_theta <= 0.7) # Theta should be in the range [0.3, 0.7]
})
