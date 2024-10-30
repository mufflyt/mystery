library(testthat)
library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(humaniformat)

test_that("clean_phase_1_results stops if required columns are missing", {
  df_missing_cols <- data.frame(names = c("John Doe", "Jane Doe"), stringsAsFactors = FALSE)
  expect_error(clean_phase_1_results(df_missing_cols), "required columns are missing")
})
