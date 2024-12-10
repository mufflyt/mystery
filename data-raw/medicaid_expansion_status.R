# Load necessary libraries
library(tibble)

# Create the Medicaid Expansion Status dataset
medicaid_expansion_status <- tibble::tibble(
  Location = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  Medicaid_Expansion = c(
    "Non-expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Non-expansion state", "Non-expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Non-expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Non-expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Non-expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Non-expansion state", "Non-expansion state", "Non-expansion state",
    "Non-expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Expansion state",
    "Expansion state", "Expansion state", "Non-expansion state"
  )
)

# Add new columns to the tibble
medicaid_expansion_status <- medicaid_expansion_status %>%
  dplyr::mutate(
    source = "Kaiser Family Foundation",
    source_url = "https://www.kff.org/affordable-care-act/state-indicator/state-activity-around-expanding-medicaid-under-the-affordable-care-act/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
    date_time_created = "11-28-2024"
  )

# Print the updated tibble
print(medicaid_expansion_status)
