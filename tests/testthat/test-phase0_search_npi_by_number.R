library(testthat)
library(dplyr)
library(readr)
library(mockery)
library(tibble)
library(logger)
library(glue)

# Adjusted mock function to return tibble within the structure expected by `npi_flatten`
# Adjusted mock function to return an `npi_results` object that npi_flatten can handle
mock_npi_search <- function(number) {
  # Construct a tibble with the expected nested lists for `npi_results`
  structure(
    tibble::tibble(
      basic = list(tibble::tibble(npi = number, name = paste0("Dr. ", number))),
      taxonomies = list(tibble::tibble(code = "207R00000X", desc = "Internal Medicine"))
    ),
    class = "npi_results"  # Assign the class `npi_results`
  )
}


# Sample dataframe for testing
sample_npi_data <- tibble(
  npi = c("1234567890", "1098765432", "1987654321", "invalidNPI", NA)
)

# Test: Input validation for 'npi' column
test_that("Function validates input columns", {
  invalid_df <- tibble(id = c("1234567890", "1098765432"))
  expect_error(phase0_search_npi_by_number(invalid_df), "The input must be a data frame with a column named 'npi'.")
})

# Test: Filters invalid NPIs correctly
test_that("Filters invalid NPIs", {
  valid_npi_df <- validate_npi_dataframe(sample_npi_data)
  expect_equal(nrow(valid_npi_df), 3)
  expect_true(all(nchar(valid_npi_df$npi) == 10))
  expect_true(all(grepl("^[0-9]{10}$", valid_npi_df$npi)))
})



# # Test: Saves results in chunks and verifies file creation
# test_that("Saves clinician information in specified chunks", {
#   temp_dir <- tempdir()
#   mockery::stub(retrieve_clinician_data, 'npi::npi_search', mock_npi_search)
#
#   # Get combined clinician data
#   clinician_data_list <- retrieve_clinician_data(sample_npi_data$npi[!is.na(sample_npi_data$npi)])
#   combined_data <- combine_clinician_info(clinician_data_list)
#
#   # Run the function and check file creation
#   save_clinician_info_in_chunks(combined_data, "npi_results", temp_dir, records_per_chunk = 2)
#   saved_files <- list.files(temp_dir, pattern = "npi_results_.*\\.csv", full.names = TRUE)
#
#   # Validate chunk file creation and content
#   expect_true(length(saved_files) > 0)
#   for (file in saved_files) {
#     saved_data <- read_csv(file, show_col_types = FALSE)
#     expect_true(nrow(saved_data) <= 2)  # Each file should have 2 or fewer rows as per chunk size
#     expect_true("npi" %in% colnames(saved_data))
#   }
# })

# Test: Function handles empty valid NPI list gracefully
test_that("Handles empty valid NPI list gracefully", {
  empty_df <- tibble(npi = c(NA, "invalidNPI"))
  mockery::stub(phase0_search_npi_by_number, 'npi::npi_search', mock_npi_search)

  result <- phase0_search_npi_by_number(empty_df)
  expect_true(nrow(result) == 0)
  expect_equal(ncol(result), 0)
})
