library(testthat)
library(dplyr)
library(logger)

test_that("Function fails with missing input CSV file", {
  expect_error(
    phase0_genderize_physicians("non_existent_file.csv"),
    "Input CSV file not found"
  )
})

# test_that("Function fails with missing output directory", {
#   # Assuming "valid_input.csv" is an existing file for this test case
#   input_file <- tempfile(fileext = ".csv")
#   write.csv(data.frame(first_name = c("John", "Jane")), input_file, row.names = FALSE)
#
#   expect_error(
#     phase0_genderize_physicians(input_file, output_dir = "non_existent_directory"),
#     "Output directory not found"
#   )
# })

test_that("Function fails if 'first_name' column is missing", {
  # Create a CSV file without 'first_name' column
  input_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(last_name = c("Doe", "Smith")), input_file, row.names = FALSE)

  expect_error(
    phase0_genderize_physicians(input_file),
    "The input CSV must contain a 'first_name' column"
  )
})



test_that("Function logs correct messages during processing", {
  # Prepare valid CSV data for logging verification
  input_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(first_name = c("Chris", "Pat")), input_file, row.names = FALSE)

  # Capture logs during execution
  log_output <- capture.output({
    result <- try(phase0_genderize_physicians(input_file, output_dir = tempdir()))
  }, type = "message")

  # Check for specific log messages
  expect_true(any(grepl("Starting genderize_physicians function", log_output)))
  expect_true(any(grepl("Genderizing first names", log_output)))
  expect_true(any(grepl("Joining gender data with the original dataset", log_output)))
  expect_true(any(grepl("Genderizing process completed successfully", log_output)))
})
