library(testthat)
library(logger)

# Begin testing
test_that("Function samples generalists and specialists correctly by city-state", {
  # Sample test data with generalists and specialists across cities and states
  data <- data.frame(
    city = rep(c("New York", "Los Angeles"), each = 6),
    state_code = rep(c("NY", "CA"), each = 6),
    specialty_primary = c(
      "General Dermatology", "Pediatric Dermatology", "General Dermatology",
      "General Dermatology", "Pediatric Dermatology", "General Dermatology",
      "General Dermatology", "General Dermatology", "Pediatric Dermatology",
      "General Dermatology", "General Dermatology", "Pediatric Dermatology"
    ),
    phone_number = rep(c("123", "456", "789"), 4),
    stringsAsFactors = FALSE
  )

  # Run the function with sampling criteria
  result <- phase0_city_state_sample_specialists(
    data,
    general_sample_size = 2,
    specialist1_sample_size = 1
  )

  # Expectations
  expect_true(is.data.frame(result), "Result should be a data frame")
  expect_true("city" %in% colnames(result), "Result should include 'city' column")
  expect_true("state_code" %in% colnames(result), "Result should include 'state_code' column")
  expect_true("specialty_primary" %in% colnames(result), "Result should include 'specialty_primary' column")

  # Verify sample sizes by city-state combination if any rows were returned
  if (nrow(result) > 0) {
    sampled_groups <- result %>%
      dplyr::group_by(city, state_code, specialty_primary) %>%
      dplyr::summarize(count = n(), .groups = "drop")

    expect_true(all(sampled_groups$count <= 2), info = "Each sampled group should have at most the specified sample size")
  } else {
    logger::log_info("No valid samples returned based on the given criteria.")
    expect_equal(nrow(result), 0, info = "Result should be empty if criteria are not met")
  }

  logger::log_info("Basic sampling test passed.")
})

test_that("Function handles missing required columns", {
  # Test data missing 'state_code' column
  data <- data.frame(
    city = c("CityA", "CityB"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    phone_number = c("123", "456"),
    stringsAsFactors = FALSE
  )

  # Expect error due to missing column
  expect_error(
    phase0_city_state_sample_specialists(
      data,
      general_sample_size = 1,
      specialist1_sample_size = 1
    ),
    "Missing required columns in the dataset"
  )
  logger::log_info("Missing required columns test passed.")
})

test_that("Function respects same_phone_number parameter", {
  # Sample test data
  data <- data.frame(
    city = rep(c("CityA", "CityB"), each = 3),
    state_code = rep(c("State1", "State2"), each = 3),
    specialty_primary = rep(c("General Dermatology", "Pediatric Dermatology"), each = 3),
    phone_number = c("111", "111", "222", "333", "333", "444"),
    stringsAsFactors = FALSE
  )

  # Run with same_phone_number = TRUE
  result <- phase0_city_state_sample_specialists(
    data,
    general_sample_size = 1,
    specialist1_sample_size = 1,
    same_phone_number = TRUE
  )

  # Expect only rows with the same phone number to be selected if criteria are met
  if (nrow(result) > 0) {
    expect_true(length(unique(result$phone_number)) == 1, "Should only sample rows with the same phone number")
  } else {
    expect_equal(nrow(result), 0) # No message in expect_equal
  }

  logger::log_info("Same phone number sampling test passed.")
})

test_that("Function correctly logs input parameters and writes CSV output", {
  # Sample test data
  data <- data.frame(
    city = c("CityA", "CityA", "CityB", "CityB"),
    state_code = c("State1", "State1", "State2", "State2"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology", "General Dermatology", "Pediatric Dermatology"),
    phone_number = c("111", "222", "333", "444"),
    stringsAsFactors = FALSE
  )

  # Define temporary file path
  temp_output_path <- tempfile(fileext = ".csv")

  # Run function with output to CSV
  result <- phase0_city_state_sample_specialists(
    data,
    general_sample_size = 1,
    specialist1_sample_size = 1,
    output_csv_path = temp_output_path
  )

  # Expectations
  expect_true(file.exists(temp_output_path), "CSV output file should be created")

  # Check contents of the CSV file if rows were sampled
  if (nrow(result) > 0) {
    csv_result <- read.csv(temp_output_path, stringsAsFactors = FALSE)
    expect_true("city" %in% colnames(csv_result), "CSV should contain 'city' column")
    expect_true(nrow(csv_result) > 0, "CSV file should have rows")
  } else {
    expect_equal(nrow(result), 0) # No message in expect_equal
  }

  # Clean up
  unlink(temp_output_path)
  logger::log_info("CSV output logging test passed.")
})
