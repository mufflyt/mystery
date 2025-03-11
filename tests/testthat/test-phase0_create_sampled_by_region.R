library(testthat)
library(dplyr)
library(logger)

# Unit Tests for phase0_create_sampled_by_region ----------------------------------------

test_that("Function completes successfully and outputs a data frame", {
  # Create sample input data
  taxonomy_and_aaos_tbl <- tibble::tibble(
    npi = 1:20,
    state_code = c(rep("CO", 10), rep("CA", 10)),
    phone_number = c(rep("12345", 10), rep("67890", 10)),
    first = c(rep("John", 10), rep("Jane", 10)),
    last = c(rep("Doe", 10), rep("Smith", 10))
  )

  census_bureau_tbl <- tibble::tibble(
    State = c("colorado", "california"),
    Region = c("West", "West"),
    Division = c("Mountain", "Pacific")
  )

  # Run the function
  result <- phase0_create_sampled_by_region(taxonomy_and_aaos_tbl, census_bureau_tbl)

  # Expectations
  expect_true(is.data.frame(result), "The function should return a data frame")
  expect_true(nrow(result) > 0, "Result should contain rows")
  expect_true("state_code" %in% colnames(result), "Result should have 'state_code' column")
  expect_true("Division" %in% colnames(result), "Result should include Census 'Division'")
  expect_true("Insurance" %in% colnames(result), "Result should include 'Insurance' column")

  logger::log_info("Basic function completion test passed.")
})

test_that("Helper functions work as expected within the pipeline", {
  # Sample input for helper function tests
  test_data <- tibble::tibble(
    state_code = c("CO", "CA", "NY"),
    phone_number = c("12345", NA, "56789"),
    first = c("John", "Jane", NA),
    last = c("Doe", NA, "Smith")
  )

  census_data <- tibble::tibble(
    State = c("colorado", "california", "new york"),
    Region = c("West", "West", "Northeast"),
    Division = c("Mountain", "Pacific", "Mid-Atlantic")
  )

  # Test add_full_state_names
  full_names <- add_full_state_names(test_data)
  expect_true("state_code" %in% colnames(full_names), "state_code column should exist after adding full names")
  expect_true(all(full_names$state_code %in% c("Colorado", "California", "New York")), "State names should be converted to full names")

  # Test filter_valid_entries
  valid_entries <- filter_valid_entries(test_data)
  expect_true(nrow(valid_entries) == 1, "Only complete entries with valid data should remain")

  # Test lowercase_state_codes
  lowercased <- lowercase_state_codes(test_data)
  expect_true(all(tolower(lowercased$state_code) == lowercased$state_code), "state_code should be lowercase")

  # Test join_census_regions
  joined_data <- join_census_regions(lowercased, census_data)
  expect_true("Region" %in% colnames(joined_data), "Region column should be added by join")
  expect_true("Division" %in% colnames(joined_data), "Division column should be added by join")

  logger::log_info("Helper function tests passed.")
})

test_that("Sampled data contains correct number of records per division", {
  # Sample data to test sampling function
  sample_data <- tibble::tibble(
    Division = rep(c("Mountain", "Pacific"), each = 20),
    npi = 1:40
  )

  # Test sampling 14 per division
  sampled <- sample_by_division(sample_data)
  division_counts <- sampled %>% dplyr::count(Division)

  expect_true(all(division_counts$n == 14), "Each division should contain exactly 14 sampled records")
  logger::log_info("Division sampling test passed.")
})

test_that("Insurance assignment alternates correctly", {
  # Sample data for insurance assignment
  insurance_data <- tibble::tibble(
    npi = 1:10
  )

  # Test assign_alternating_insurance
  insurance_assigned <- assign_alternating_insurance(insurance_data)
  expect_true(all(insurance_assigned$Insurance %in% c("Blue Cross/Blue Shield", "Medicaid")), "Insurance should alternate between two types")
  expect_true(sum(insurance_assigned$Insurance == "Blue Cross/Blue Shield") == 5, "Half of the rows should be assigned to Blue Cross/Blue Shield")
  expect_true(sum(insurance_assigned$Insurance == "Medicaid") == 5, "Half of the rows should be assigned to Medicaid")

  logger::log_info("Insurance assignment test passed.")
})

test_that("Final arrangement removes temporary columns and is ordered by state_code and NPI", {
  # Sample data for arranging columns
  arrange_data <- tibble::tibble(
    state_code = c("CA", "CO", "NY"),
    npi = c(3, 1, 2),
    temp_id = c(1, 2, 3)
  )

  arranged <- arrange_final_columns(arrange_data)

  expect_false("temp_id" %in% colnames(arranged)) # No additional message needed
  expect_equal(arranged$state_code, c("CA", "CO", "NY"), info = "Data should be ordered by state_code")
  expect_equal(arranged$npi, c(3, 1, 2), info = "Data should be ordered by NPI within state_code")

  logger::log_info("Final arrangement test passed.")
})


# passed
test_that("Final arrangement removes temporary columns and is ordered by state_code and NPI", {
  # Sample data for arranging columns
  arrange_data <- tibble::tibble(
    state_code = c("CA", "CO", "NY"),
    npi = c(3, 1, 2),
    temp_id = c(1, 2, 3)
  )

  arranged <- arrange_final_columns(arrange_data)

  expect_false("temp_id" %in% colnames(arranged), "Temporary ID column should be removed")
  expect_equal(arranged$state_code, c("CA", "CO", "NY"), info = "Data should be ordered by state_code")
  expect_equal(arranged$npi, c(3, 1, 2), info = "Data should be ordered by NPI within state_code")

  logger::log_info("Final arrangement test passed.")
})
