library(testthat)

test_that("myncurve generates correct plot and calculates lower tail probability", {
  # Test with standard normal distribution
  x <- 1.5
  mean <- 0
  sd <- 1

  expect_output(area <- myncurve(x, mean, sd), "0.9332")

  # Verify the calculated area (lower tail probability)
  expected_area <- pnorm(x, mean = mean, sd = sd)
  expect_equal(area, round(expected_area, 4))

  # Test with a shifted mean and standard deviation
  x <- 5
  mean <- 10
  sd <- 3

  expect_output(area <- myncurve(x, mean, sd), "0.0478")

  # Verify the calculated area
  expected_area <- pnorm(x, mean = mean, sd = sd)
  expect_equal(area, round(expected_area, 4))
})
