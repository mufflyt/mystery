# Load necessary libraries
library(testthat)
library(dplyr)
library(logger)

test_that("Function errors when 'data' is not a data frame", {
  expect_error(city_state_assign_scenarios(data = NULL), "Input 'data' must be a data frame.")
})

test_that("Function errors when required columns are missing", {
  data_missing_columns <- data.frame(city = "City", state_code = "State")
  expect_error(city_state_assign_scenarios(data = data_missing_columns), "Missing required columns")
})

test_that("Function runs with default parameters", {
  sample_data <- data.frame(
    city = "Sample City",
    state_code = "SC",
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )
  result <- city_state_assign_scenarios(data = sample_data)
  expect_s3_class(result, "data.frame")
})


test_that("Specialists are assigned all cases", {
  sample_data <- data.frame(
    city = "Sample City",
    state_code = "SC",
    specialty_primary = "Pediatric Dermatology",
    stringsAsFactors = FALSE
  )
  result <- city_state_assign_scenarios(data = sample_data)
  assigned_cases <- unique(result$case_assigned)
  expect_equal(sort(assigned_cases), sort(c("Alpha", "Beta", "Gamma")))
})

test_that("Generalists are assigned one case each", {
  sample_data <- data.frame(
    city = "Sample City",
    state_code = "SC",
    specialty_primary = rep("General Dermatology", 5),
    stringsAsFactors = FALSE
  )
  result <- city_state_assign_scenarios(data = sample_data)
  assigned_cases <- result$case_assigned
  expect_equal(length(assigned_cases), 5)
  expect_true(all(assigned_cases %in% c("Alpha", "Beta", "Gamma")))
})


test_that("Function creates output CSV file", {
  sample_data <- data.frame(
    city = "Sample City",
    state_code = "SC",
    specialty_primary = c("General Dermatology", "Pediatric Dermatology"),
    stringsAsFactors = FALSE
  )
  output_path <- tempfile(fileext = ".csv")
  result <- city_state_assign_scenarios(data = sample_data, output_csv_path = output_path)
  expect_true(file.exists(output_path))
})
