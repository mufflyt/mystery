library(readr)
library(dplyr)

# Load the raw data
medicaid_to_medicare_fee_index <- read_csv("data-raw/Medicaid-to-Medicare Fee Index_raw_data.csv") %>%
  # Clean column names to snake_case
  janitor::clean_names() %>%
  # Multiply all numeric columns by 100
  dplyr::mutate(across(where(is.numeric), ~ . * 100)) %>%
  # Add the additional columns
  dplyr::mutate(
    source = "Kaiser Family Foundation",
    source_url = "https://www.kff.org/medicaid/state-indicator/medicaid-to-medicare-fee-index/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
    date_time_created = "2019"
  )

# Print the processed dataset
print(medicaid_to_medicare_fee_index)
