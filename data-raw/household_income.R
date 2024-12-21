# Load necessary libraries
library(tidycensus)
library(dplyr)
library(logger)
library(zipcodeR)

# Log the start of the process
log_info("Starting ZIP code income data retrieval")

# Step 1: Define parameters
year <- 2022 # Define the ACS year
survey <- "acs5" # Define the survey type (default to ACS 5-year)

# Validate the year input
if (!is.numeric(year) || year < 2009 || year > as.numeric(format(Sys.Date(), "%Y"))) {
  stop("Invalid 'year': Must be a numeric year between 2009 and the current year.")
}

# Step 2: Retrieve ACS data for median household income at the ZIP code level
log_info("Retrieving ACS data for median household income...")
income_data <- tidycensus::get_acs(
  geography = "zip code tabulation area",
  variables = "B19013_001", # Median household income variable
  year = year,
  survey = survey,
  output = "wide"
) %>%
  dplyr::rename(
    zip_code = GEOID,
    median_income = B19013_001E
  ) %>%
  dplyr::mutate(
    median_income = as.numeric(median_income)
  )

# Step 3: Attach state information using zipcodeR
log_info("Attaching state information to ZIP codes...")
zip_data <- zipcodeR::zip_code_db %>%
  dplyr::rename(zip_code = zipcode) %>%
  dplyr::select(zip_code, state)

income_data <- income_data %>%
  dplyr::left_join(zip_data, by = "zip_code")

# Step 4: Replace state abbreviations and non-US territories with full names
log_info("Converting state abbreviations and non-US territories to full names...")
territory_map <- c(
  "AS" = "American Samoa",
  "GU" = "Guam",
  "MP" = "Northern Mariana Islands",
  "PR" = "Puerto Rico",
  "VI" = "U.S. Virgin Islands"
)

income_data <- income_data %>%
  dplyr::mutate(
    state = case_when(
      state %in% state.abb ~ state.name[match(state, state.abb)], # US state abbreviations
      state %in% names(territory_map) ~ territory_map[state],    # Non-US territories
      TRUE ~ state # Unchanged if no match
    )
  )

# Step 5: Remove the NAME column
log_info("Removing the NAME column...")
income_data <- income_data %>%
  dplyr::select(-NAME)

# Step 6: Add metadata columns and reorder
log_info("Adding metadata columns...")
income_data <- income_data %>%
  dplyr::mutate(
    date_time_created = Sys.time(),  # Add the date and time column
    survey = survey                  # Add the survey column
  ) %>%
  dplyr::select(-year, everything(), year)  # Move year to the end

# Step 7: Validate that the final dataset has the necessary columns
required_columns <- c("zip_code", "state", "median_income", "year", "date_time_created", "survey")
if (!all(required_columns %in% colnames(household_income))) {
  stop("The final income data is missing required columns: ",
       paste(setdiff(required_columns, colnames(household_income)), collapse = ", "))
}

# Print a preview of the dataset
print(head(household_income))

#readr::write_rds(income_data, "/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/data/reference/income_data_2022.rds")
