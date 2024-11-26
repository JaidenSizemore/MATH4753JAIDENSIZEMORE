library(testthat)

test_that("mypvalue computes correct p-value and critical t values", {
  # Test case with a tcalc value and n
  t0 <- 2.0
  n <- 30
  alpha <- 0.05

  result <- mypvalue(t0, n = n, alpha = alpha)

  # Expected critical t values
  expected_q <- qt(1 - alpha / 2, df = n - 1)
  expect_equal(result$q, expected_q, tolerance = 1)

  # Expected p-value
  expected_pv <- 2 * pt(-t0, df = n - 1)
  expect_equal(result$pvalue, expected_pv, tolerance = 1)
})
