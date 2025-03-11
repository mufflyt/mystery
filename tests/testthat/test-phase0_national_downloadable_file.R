######
# Load required packages
library(testthat)
library(logger)

# Begin testing
test_that("Basic functionality works with default parameters", {
  result <- phase0_national_downloadable_file()
  expect_true(is.data.frame(result), "Result should be a data frame")
  expect_true(ncol(result) > 0, "Data frame should have columns")
  expect_true(nrow(result) > 0, "Data frame should have rows")
  logger::log_info("Basic functionality test passed.")
})

test_that("Custom sleep time does not affect data retrieval", {
  result <- phase0_national_downloadable_file(sys_sleep = 2)
  expect_true(is.data.frame(result), "Result should be a data frame with custom sleep time")
  expect_true(ncol(result) > 0, "Data frame should have columns with custom sleep time")
  expect_true(nrow(result) > 0, "Data frame should have rows with custom sleep time")
  logger::log_info("Custom sleep time test passed.")
})


test_that("Single property, multiple values fetches data for all specified values", {
  result <- phase0_national_downloadable_file(
    sys_sleep = 1,
    property = "pri_spec",
    value = c("OBSTETRICS/GYNECOLOGY", "OTOLARYNGOLOGY")
  )

  expect_true(is.data.frame(result), "Result should be a data frame")
  expect_true(ncol(result) > 0, "Data frame should have columns")
  expect_true(nrow(result) > 0, "Data frame should have rows")

  # Check that at least one row exists for each specified specialty
  expect_true(any(result$pri_spec == "OBSTETRICS/GYNECOLOGY"), "Data should contain 'OBSTETRICS/GYNECOLOGY'")
  expect_true(any(result$pri_spec == "OTOLARYNGOLOGY"), "Data should contain 'OTOLARYNGOLOGY'")

  logger::log_info("Single property, multiple values test passed.")
})


test_that("Incorrect property or value throws an error", {
  # Expecting the specific error message raised when the CMS API request fails
  expect_error(
    phase0_national_downloadable_file(property = "invalid_property", value = "invalid_value"),
    "Failed to retrieve filtered data from the CMS API"
  )

  # Log success if the error handling test passes
  logger::log_info("Error handling for incorrect property/value test passed.")
})

test_that("Single property with multiple values retrieves relevant data", {
  result <- phase0_national_downloadable_file(
    property = "pri_spec",
    value = c("OBSTETRICS/GYNECOLOGY", "OTOLARYNGOLOGY")
  )
  expect_true(is.data.frame(result), "Result should be a data frame")
  expect_true(ncol(result) > 0, "Data frame should have columns")
  expect_true(nrow(result) > 0, "Data frame should have rows")
  expect_true(
    any(result$pri_spec == "OBSTETRICS/GYNECOLOGY") &&
      any(result$pri_spec == "OTOLARYNGOLOGY"),
    "Data should contain specified values"
  )
  logger::log_info("Single property, multiple values test passed.")
})


test_that("Output CSV path saves file correctly", {
  temp_dir <- tempdir()
  result <- phase0_national_downloadable_file(output_csv_path = temp_dir)
  saved_files <- list.files(temp_dir, pattern = "cms_data_\\d{14}\\.csv$")
  expect_true(length(saved_files) > 0, "Output file should be saved in the specified directory")
  logger::log_info("Output CSV saving test passed.")
})
