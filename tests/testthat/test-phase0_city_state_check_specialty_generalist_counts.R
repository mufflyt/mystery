library(testthat)
library(logger)

# Begin testing

test_that("Function identifies failing and successful city-state combinations", {
  # Sample test data with generalists and specialists
  data <- data.frame(
    city = c("CityA", "CityA", "CityB", "CityB", "CityC"),
    state_code = c("State1", "State1", "State2", "State2", "State3"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology", "General Dermatology", "General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Run the function
  result <- phase0_city_state_check_specialty_generalist_counts(
    data,
    min_generalists = 1,
    min_specialists = 1
  )

  # Expectations
  expect_type(result, "list")
  expect_true("failing_combinations" %in% names(result), "Result should contain 'failing_combinations'")
  expect_true("successful_combinations" %in% names(result), "Result should contain 'successful_combinations'")

  # Check that CityC is failing due to missing generalist
  expect_true(any(result$failing_combinations$city == "CityC"), "CityC should be in the failing combinations")

  # Check that CityA is successful (both generalist and specialist present)
  expect_true(any(result$successful_combinations$city == "CityA"), "CityA should be in the successful combinations")

  logger::log_info("Basic failing and successful combination test passed.")
})

test_that("Function handles missing required columns", {
  # Test data missing 'state_code' column
  data <- data.frame(
    city = c("CityA", "CityB"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Expect error due to missing column
  expect_error(
    phase0_city_state_check_specialty_generalist_counts(
      data,
      min_generalists = 1,
      min_specialists = 1
    ),
    "Missing required columns: state_code"
  )
  logger::log_info("Missing required columns test passed.")
})

test_that("Function correctly outputs CSV files for failing and successful combinations", {
  # Sample test data
  data <- data.frame(
    city = c("CityA", "CityA", "CityB", "CityB"),
    state_code = c("State1", "State1", "State2", "State2"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology", "General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Define temporary file paths for output
  temp_failing_path <- tempfile(fileext = ".csv")
  temp_successful_path <- tempfile(fileext = ".csv")

  # Run the function with custom output paths
  result <- phase0_city_state_check_specialty_generalist_counts(
    data,
    min_generalists = 1,
    min_specialists = 1,
    failing_csv_path = temp_failing_path,
    successful_csv_path = temp_successful_path
  )

  # Expectations
  expect_true(file.exists(temp_failing_path), "Failing combinations CSV file should be created")
  expect_true(file.exists(temp_successful_path), "Successful combinations CSV file should be created")

  # Check that CityA is successful in CSV file
  successful_csv <- read.csv(temp_successful_path, stringsAsFactors = FALSE)
  expect_true(any(successful_csv$city == "CityA"), "CityA should be in the successful CSV file")

  # Clean up
  unlink(temp_failing_path)
  unlink(temp_successful_path)
  logger::log_info("CSV output and logging test passed.")
})

test_that("Function returns accurate failing and successful combination counts", {
  # Sample test data
  data <- data.frame(
    city = c("CityA", "CityB"),
    state_code = c("State1", "State2"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Run the function
  result <- phase0_city_state_check_specialty_generalist_counts(
    data,
    min_generalists = 1,
    min_specialists = 1
  )

  # Expectations with `info` argument for additional messages
  expect_equal(nrow(result$failing_combinations), 2, info = "There should be 2 failing combinations")
  expect_equal(nrow(result$successful_combinations), 0, info = "There should be 0 successful combinations")

  logger::log_info("Combination count test passed.")
})
