# Load necessary libraries
# install.packages("openxlsx")
# install.packages("emmeans")

library(testthat)
# Load necessary libraries for the tests
library(openxlsx)
library(dplyr)
library(fs)

# Test cases for the split_and_save function
test_that("Missing required columns handling", {
  # Creating a dataframe intentionally missing the required 'for_redcap', 'id', and 'doctor_id' columns
  test_data <- data.frame(
    not_for_redcap = 1:4,
    not_id = 1:4,
    stringsAsFactors = FALSE
  )

  # Testing if the function throws the correct error when required columns are missing
  expect_error(
    split_and_save(data_or_path = test_data, output_directory = tempdir(), lab_assistant_names = c("Alice", "Bob")),
    "The input data is missing the following columns: for_redcap, id, doctor_id",
    fixed = TRUE  # Ensures that the error message matches exactly
  )
})

test_that("Invalid file path handling", {
  expect_error(
    split_and_save(data_or_path = "nonexistent/path/data.csv", output_directory = tempdir(), lab_assistant_names = c("Alice", "Bob")),
    "File does not exist at the specified path",
    fixed = TRUE
  )
})

