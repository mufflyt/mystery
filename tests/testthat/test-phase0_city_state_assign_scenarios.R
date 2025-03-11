library(testthat)
library(logger)

# Begin testing
test_that("Function assigns cases correctly to generalists and specialists", {
  # Test data with generalists and specialists
  data <- data.frame(
    city = c("CityA", "CityA", "CityB", "CityB"),
    state_code = c("State1", "State1", "State2", "State2"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology", "General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Run the function
  result <- phase0_city_state_assign_scenarios(data)

  # Expectations
  expect_true(is.data.frame(result), "Result should be a data frame")
  expect_true("case_assigned" %in% colnames(result), "Result should have a 'case_assigned' column")
  expect_true(any(result$case_assigned == "Case Alpha"), "Case Alpha should be assigned")
  expect_true(any(result$case_assigned == "Case Beta"), "Case Beta should be assigned")
  expect_true(any(result$case_assigned == "Case Gamma"), "Case Gamma should be assigned")

  # Corrected expectation for specialists having all cases assigned
  specialists <- result[result$specialty_primary == "Pediatric Dermatology", ]
  if (nrow(specialists) != 6) {
    fail("Each specialist should have a row for each assigned case")
  }

  logger::log_info("Basic case assignment test passed.")
})

test_that("Function works with empty input data", {
  # Empty test data
  data <- data.frame(
    city = character(),
    state_code = character(),
    specialty_primary = character(),
    stringsAsFactors = FALSE
  )

  # Run the function
  result <- phase0_city_state_assign_scenarios(data)

  # Expectations
  expect_equal(nrow(result), 0)
  expect_true("case_assigned" %in% colnames(result))
  logger::log_info("Empty input data test passed.")
})

test_that("Function assigns cases in order to generalists and all cases to specialists", {
  # Test data with multiple generalists and specialists
  data <- data.frame(
    city = rep("CityA", 6),
    state_code = rep("State1", 6),
    specialty_primary = c(
      "General Dermatology", "General Dermatology", "General Dermatology",
      "Pediatric Dermatology", "Pediatric Dermatology", "Pediatric Dermatology"
    ),
    stringsAsFactors = FALSE
  )

  # Run the function
  result <- phase0_city_state_assign_scenarios(data)

  # Check case assignment for generalists (distributed)
  generalists <- result[result$specialty_primary == "General Dermatology", ]
  expect_true(all(generalists$case_assigned %in% c("Case Alpha", "Case Beta", "Case Gamma")))
  expect_equal(length(unique(generalists$case_assigned)), 3)

  # Check case assignment for specialists (all cases)
  specialists <- result[result$specialty_primary == "Pediatric Dermatology", ]
  if (nrow(specialists) != 9) {
    fail("Each specialist should have a row for each case")
  }

  logger::log_info("Generalist and specialist case assignment distribution test passed.")
})

test_that("Function handles missing required columns", {
  # Test data missing 'state_code' column
  data <- data.frame(
    city = c("CityA", "CityB"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Expect error due to missing column
  expect_error(phase0_city_state_assign_scenarios(data), "Missing required columns: state_code")
  logger::log_info("Missing columns test passed.")
})

test_that("Output CSV is created correctly", {
  # Test data with generalists and specialists
  data <- data.frame(
    city = c("CityA", "CityA"),
    state_code = c("State1", "State1"),
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )

  # Define temporary file path for output
  temp_output_path <- tempfile(fileext = ".csv")

  # Run the function with custom output path
  result <- phase0_city_state_assign_scenarios(data, output_csv_path = temp_output_path)

  # Expectations
  expect_true(file.exists(temp_output_path))

  # Clean up
  unlink(temp_output_path)
  logger::log_info("Output CSV creation test passed.")
})
