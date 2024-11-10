library(testthat)
library(dplyr)
library(readr)
library(mockery)

# Source the function file if the function is in an external script
# source("path/to/your_function_script.R")

# Mock the npi::npi_search function for testing purposes
mock_npi_search <- function(first_name, last_name, limit) {
  data.frame(
    npi = sample(1000000000:9999999999, 1),
    first_name_searched = first_name,
    last_name_searched = last_name,
    stringsAsFactors = FALSE
  )
}

# Sample dataframe for testing
sample_names <- data.frame(
  first = c("Tyler", "Matthew", "John"),
  last = c("Muffly", "Smith", "Doe"),
  stringsAsFactors = FALSE
)

# Test to validate that input dataframe has 'first' and 'last' columns
test_that("Function validates input columns", {
  invalid_df <- data.frame(name = c("Tyler", "Matthew"))
  expect_error(phase0_search_batch_npi(invalid_df), "The dataframe must have 'first' and 'last' columns.")
})

# Test that the function processes a valid dataframe correctly
test_that("Processes valid dataframe correctly", {
  mockery::stub(phase0_search_batch_npi, 'npi::npi_search', mock_npi_search)
  result <- phase0_search_batch_npi(sample_names, limit = 5)
  expect_true(nrow(result) > 0)
  expect_true("npi" %in% colnames(result))
  expect_true("first_name_searched" %in% colnames(result))
  expect_true("last_name_searched" %in% colnames(result))
})

# Test if the function saves the output file correctly when write_csv_path is specified
test_that("Saves output file correctly when write_csv_path is specified", {
  temp_file <- tempfile(fileext = ".csv")
  mockery::stub(phase0_search_batch_npi, 'npi::npi_search', mock_npi_search)

  # Run function with file-saving enabled
  result <- phase0_search_batch_npi(sample_names, limit = 5, write_csv_path = temp_file)

  # Check that the file was created and contents are as expected
  expect_true(file.exists(temp_file))
  saved_data <- read_csv(temp_file, show_col_types = FALSE)
  expect_equal(ncol(saved_data), ncol(result))
  expect_true("npi" %in% colnames(saved_data))
})

# # Test handling of no results from the API
# test_that("Handles no results from the API gracefully", {
#   # Mock npi::npi_search to return NULL for no results
#   mockery::stub(phase0_search_batch_npi, 'npi::npi_search', function(...) NULL)
#
#   # Run function and expect an empty result
#   result <- phase0_search_batch_npi(sample_names, limit = 5)
#   expect_true(nrow(result) == 0)
# })

# # Test handling of API errors without stopping function execution
# test_that("Handles API errors without stopping", {
#   # Mock npi::npi_search to throw an error
#   mockery::stub(phase0_search_batch_npi, 'npi::npi_search', function(...) stop("API error"))
#
#   # Run function, it should complete without results due to errors
#   expect_error_free(result <- phase0_search_batch_npi(sample_names, limit = 5))
#   expect_true(nrow(result) == 0)
# })

