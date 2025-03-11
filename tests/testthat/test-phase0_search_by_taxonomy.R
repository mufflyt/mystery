library(testthat)
library(dplyr)
library(readr)
library(mockery)
library(tibble)

# Test cases
test_that("Handles no matching data", {
  cat("Running test: Handles no matching data\n")
  taxonomy <- "Nonexistent Taxonomy"

  # Stub the npi_search and npi_flatten functions
  stub(search_by_taxonomy, "npi::npi_search", function(...) {
    cat("Mock npi_search called\n")
    NULL
  })

  stub(search_by_taxonomy, "npi::npi_flatten", function(result) {
    cat("Mock npi_flatten called\n")
    result
  })

  result <- search_by_taxonomy(taxonomy)

  expect_equal(nrow(result), 0)
})


# Mock the npi::npi_search function to return consistent test data
mock_npi_search <- function(first_name, last_name, limit) {
  data.frame(
    npi = sample(1000000000:9999999999, 1),
    queried_first_name = first_name,
    queried_last_name = last_name,
    stringsAsFactors = FALSE
  )
}

# Sample dataframe for testing
sample_name_data <- tibble(
  first = c("Tyler", "Matthew", "John"),
  last = c("Muffly", "Smith", "Doe")
)

# Test: Input validation for 'first' and 'last' columns
test_that("Function validates input columns", {
  invalid_df <- tibble(name = c("Tyler", "Matthew"))
  expect_error(phase0_search_batch_npi(invalid_df), "The dataframe must have 'first' and 'last' columns.")
})

# Test: Processes a valid dataframe correctly
test_that("Processes valid dataframe correctly", {
  # Mock the npi_search function
  mockery::stub(phase0_search_batch_npi, "npi::npi_search", mock_npi_search)

  result <- phase0_search_batch_npi(sample_name_data, max_results = 5)
  expect_true(nrow(result) > 0)
  expect_true("npi" %in% colnames(result))
  expect_true("queried_first_name" %in% colnames(result))
  expect_true("queried_last_name" %in% colnames(result))
})


# Test: Saves the output file correctly when csv_path is specified
test_that("Saves output file correctly when csv_path is specified", {
  temp_file <- tempfile(fileext = ".csv")
  mockery::stub(phase0_search_batch_npi, "npi::npi_search", mock_npi_search)

  # Run function with file-saving enabled
  result <- phase0_search_batch_npi(sample_name_data, max_results = 5, csv_path = temp_file)

  # Check that the file was created and contents are as expected
  expect_true(file.exists(temp_file))
  saved_data <- read_csv(temp_file, show_col_types = FALSE)
  expect_equal(ncol(saved_data), ncol(result))
  expect_true("npi" %in% colnames(saved_data))
})
