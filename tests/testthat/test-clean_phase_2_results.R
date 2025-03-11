library(testthat)
library(dplyr)
library(readr)

# Test setup
data <- data.frame(
  physician_info_data = 1:10,
  contact_info_office = 11:20,
  study_inclusion_yes = rep(TRUE, 10),
  exclusion_reasons_text = letters[1:10],
  date_appointment = seq(as.Date("2021-01-01"), by = "days", length.out = 10),
  transfer_no = 1:10,
  duration_call_seconds = seq(10, 100, by = 10),
  duration_hold_seconds = seq(5, 50, by = 5),
  note_contents = letters[10:1],
  completing_person_name = LETTERS[1:10]
)

# Define the standard column names
required_strings <- c(
  "physician_info", "contact_info", "study_inclusion",
  "exclusion_reasons", "date_appointment", "transfer_no",
  "duration_call", "duration_hold", "note_contents", "completing_person"
)
standard_names <- c(
  "physician_info", "contact_office", "included_in_study",
  "exclusion_reasons", "appt_date", "transfer_count",
  "call_duration", "hold_duration", "notes", "completed_by"
)

# Test: Check the column renaming
test_that("Columns are renamed correctly", {
  renamed_data <- rename_columns_by_substring(data, required_strings, standard_names)
  expected_names <- c(
    "physician_info", "contact_office", "included_in_study",
    "exclusion_reasons", "appt_date", "transfer_count",
    "call_duration", "hold_duration", "notes", "completed_by"
  )
  expect_equal(names(renamed_data), expected_names)
})

# Test: Handle missing target strings
test_that("Handling missing target strings correctly", {
  non_existent_string <- c("non_existent_column")
  non_existent_name <- c("should_fail")
  expect_warning(
    rename_columns_by_substring(data, non_existent_string, non_existent_name),
    "No columns found containing 'non_existent_column'"
  )
})


# Test: Function stops when target and new names length mismatch
test_that("Error on mismatched lengths of strings and new names", {
  expect_error(
    rename_columns_by_substring(data, required_strings, standard_names[-1]),
    "target_strings and new_names must have the same length."
  )
})
