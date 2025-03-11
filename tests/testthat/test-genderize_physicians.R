library(testthat)
library(readr)
library(dplyr)
library(gender)
library(fs)

testthat::skip_if_not_installed("mockery")

# Create a temporary CSV file for testing
create_temp_csv <- function(data, file_name = "temp_physicians.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  return(temp_file)
}

# Sample data for testing
sample_data <- data.frame(
  first_name = c("John", "Jane", "Alex", "Sam", "Chris"),
  last_name = c("Doe", "Smith", "Brown", "Wilson", "Taylor"),
  stringsAsFactors = FALSE
)
