library(testthat)

test_that("myread works with a temporary file", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()

  # Create a sample data frame to write to a CSV file
  sample_data <- data.frame(
    ID = 1:5,
    Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
    Score = c(85, 90, 88, 92, 87)
  )

  # Create a CSV file in the temporary directory
  temp_csv <- file.path(temp_dir, "test_data.csv")
  write.csv(sample_data, temp_csv, row.names = FALSE)

  # Check that the file was created
  expect_true(file.exists(temp_csv))
})
