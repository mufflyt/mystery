# Load necessary libraries
library(ggplot2) # For data visualization
library(dplyr) # For data manipulation
library(tidycensus) # To access Census data
library(tidyr) # For data tidying
library(logger) # For logging information

# Define variables for uninsured male populations by age group
uninsured_male_vars <- c(
  "B27001_005", # Male: Under 6 years
  "B27001_008", # Male: 6 to 18 years
  "B27001_011", # Male: 19 to 25 years
  "B27001_014", # Male: 26 to 34 years
  "B27001_017", # Male: 35 to 44 years
  "B27001_020", # Male: 45 to 54 years
  "B27001_023", # Male: 55 to 64 years
  "B27001_026", # Male: 65 to 74 years
  "B27001_029" # Male: 75 years and over
)

# Define variables for uninsured female populations by age group
uninsured_female_vars <- c(
  "B27001_033", # Female: Under 6 years
  "B27001_036", # Female: 6 to 18 years
  "B27001_039", # Female: 19 to 25 years
  "B27001_042", # Female: 26 to 34 years
  "B27001_045", # Female: 35 to 44 years
  "B27001_048", # Female: 45 to 54 years
  "B27001_051", # Female: 55 to 64 years
  "B27001_054", # Female: 65 to 74 years
  "B27001_057" # Female: 75 years and over
)

year <- 2022
geography <- "state"
survey <- "acs5"

# Function to fetch uninsured data for a given year
fetch_uninsured_data <- function(year) {
  log_info("Fetching uninsured data for {year}") # Log the year being fetched

  # Retrieve uninsured data for males
  uninsured_male_data <- tidycensus::get_acs(
    geography = geography, # State-level data
    variables = uninsured_male_vars, # Variables for uninsured males
    year = year, # Specify the year
    survey = survey # Use 5-year ACS estimates
  ) %>%
    dplyr::mutate(
      sex = "Male",
      year = year,
      survey = survey,
      date_time_created = Sys.time()
    ) %>%
    dplyr::left_join(uninsured_male_vars, by = c("variable" = "variable")) # Add variable descriptions

  # Retrieve uninsured data for females
  uninsured_female_data <- tidycensus::get_acs(
    geography = "state", # State-level data
    variables = uninsured_female_vars, # Variables for uninsured females
    year = year, # Specify the year
    survey = "acs5" # Use 5-year ACS estimates
  ) %>%
    dplyr::mutate(sex = "Female") # Add a column to indicate female sex

  # Combine male and female data into one dataset
  uninsured_data <- dplyr::bind_rows(uninsured_male_data, uninsured_female_data)

  log_info("Uninsured data fetched successfully for {year}") # Log success message
  return(uninsured_data) # Return the combined dataset
}

# Fetch uninsured data for the specified year
uninsured_data <- fetch_uninsured_data(year) # Call the function to get data

# Summarize uninsured data by state and sex
uninsured_summary <- uninsured_data %>%
  dplyr::group_by(NAME, sex) %>% # Group data by state and sex
  dplyr::summarize(
    uninsured_total = sum(estimate), # Sum estimates to get total uninsured
    .groups = "drop" # Drop grouping after summarizing
  )

# Retrieve total population data by state and sex
total_population_data <- tidycensus::get_acs(
  geography = "state", # State-level data
  variables = c(
    male_total = "B01001_002", # Total male population
    female_total = "B01001_026" # Total female population
  ),
  year = year, # Specify the year
  survey = "acs5" # Use 5-year ACS estimates
) %>%
  dplyr::mutate(
    sex = ifelse(variable == "male_total", "Male", "Female") # Assign sex based on variable
  ) %>%
  dplyr::select(NAME, sex, total_population = estimate) # Select relevant columns

# Combine uninsured and total population data
uninsured_rate_data <- uninsured_summary %>%
  dplyr::left_join(total_population_data, by = c("NAME", "sex")) %>% # Join datasets by state and sex
  dplyr::mutate(
    uninsured_rate = uninsured_total / total_population # Calculate uninsured rate
  ) %>%
  dplyr::select(
    State = NAME, # Rename NAME to State
    Sex = sex, # Keep sex as Sex
    Uninsured = uninsured_total, # Total uninsured
    Total_Population = total_population, # Total population
    Uninsured_Rate = uninsured_rate # Uninsured rate
  )

# Pivot data to wide format for easier comparison between sexes
uninsured_rate_summary <- uninsured_rate_data %>%
  tidyr::pivot_wider(
    names_from = Sex, # Pivot on Sex column
    values_from = c(Uninsured, Total_Population, Uninsured_Rate), # Values to spread
    names_glue = "{Sex}_{.value}" # Naming convention for new columns
  ) %>%
  dplyr::mutate(
    Uninsured_Male_Rate = scales::percent(Male_Uninsured_Rate, accuracy = 0.1), # Format male uninsured rate as percentage
    Uninsured_Female_Rate = scales::percent(Female_Uninsured_Rate, accuracy = 0.1) # Format female uninsured rate as percentage
  )

# Display the summarized data
print(uninsured_rate_summary)


####
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyr)
library(logger)

# Define variables for uninsured male populations by age group
uninsured_male_vars <- tibble::tibble(
  variable = c(
    "B27001_005", # Male: Under 6 years
    "B27001_008", # Male: 6 to 18 years
    "B27001_011", # Male: 19 to 25 years
    "B27001_014", # Male: 26 to 34 years
    "B27001_017", # Male: 35 to 44 years
    "B27001_020", # Male: 45 to 54 years
    "B27001_023", # Male: 55 to 64 years
    "B27001_026", # Male: 65 to 74 years
    "B27001_029" # Male: 75 years and over
  ),
  variable_description = c(
    "Male: Under 6 years",
    "Male: 6 to 18 years",
    "Male: 19 to 25 years",
    "Male: 26 to 34 years",
    "Male: 35 to 44 years",
    "Male: 45 to 54 years",
    "Male: 55 to 64 years",
    "Male: 65 to 74 years",
    "Male: 75 years and over"
  )
)

# Define variables for uninsured female populations by age group
uninsured_female_vars <- tibble::tibble(
  variable = c(
    "B27001_033", # Female: Under 6 years
    "B27001_036", # Female: 6 to 18 years
    "B27001_039", # Female: 19 to 25 years
    "B27001_042", # Female: 26 to 34 years
    "B27001_045", # Female: 35 to 44 years
    "B27001_048", # Female: 45 to 54 years
    "B27001_051", # Female: 55 to 64 years
    "B27001_054", # Female: 65 to 74 years
    "B27001_057" # Female: 75 years and over
  ),
  variable_description = c(
    "Female: Under 6 years",
    "Female: 6 to 18 years",
    "Female: 19 to 25 years",
    "Female: 26 to 34 years",
    "Female: 35 to 44 years",
    "Female: 45 to 54 years",
    "Female: 55 to 64 years",
    "Female: 65 to 74 years",
    "Female: 75 years and over"
  )
)

year <- 2022
geography <- "state"
survey <- "acs5"

# Function to fetch uninsured data for a given year
fetch_uninsured_data <- function(year) {
  log_info("Fetching uninsured data for {year}") # Log the year being fetched

  # Retrieve uninsured data for males
  uninsured_male_data <- tidycensus::get_acs(
    geography = geography,
    variables = uninsured_male_vars$variable,
    year = year,
    survey = survey
  ) %>%
    dplyr::mutate(
      sex = "Male",
      year = year,
      survey = survey,
      date_time_created = Sys.time()
    ) %>%
    dplyr::left_join(uninsured_male_vars, by = c("variable" = "variable"))

  # Retrieve uninsured data for females
  uninsured_female_data <- tidycensus::get_acs(
    geography = geography,
    variables = uninsured_female_vars$variable,
    year = year,
    survey = survey
  ) %>%
    dplyr::mutate(
      sex = "Female",
      year = year,
      survey = survey,
      date_time_created = Sys.time()
    ) %>%
    dplyr::left_join(uninsured_female_vars, by = c("variable" = "variable"))

  # Combine male and female data into one dataset
  uninsured_data <- dplyr::bind_rows(uninsured_male_data, uninsured_female_data)

  log_info("Uninsured data fetched successfully for {year}") # Log success message
  return(uninsured_data) # Return the combined dataset
}

# Fetch uninsured data for the specified year
uninsured_data <- fetch_uninsured_data(year) # Call the function to get data

# Summarize uninsured data by state and sex
uninsured_summary <- uninsured_data %>%
  dplyr::group_by(NAME, sex) %>%
  dplyr::summarize(
    uninsured_total = sum(estimate),
    .groups = "drop"
  )

# Retrieve total population data by state and sex
total_population_data <- tidycensus::get_acs(
  geography = geography,
  variables = c(
    male_total = "B01001_002",
    female_total = "B01001_026"
  ),
  year = year,
  survey = survey
) %>%
  dplyr::mutate(
    sex = ifelse(variable == "male_total", "Male", "Female")
  ) %>%
  dplyr::select(NAME, sex, total_population = estimate)

# Combine uninsured and total population data
# uninsured_rate_data <- uninsured_summary %>%
#   dplyr::left_join(total_population_data, by = c("NAME", "sex")) %>%
#   dplyr::mutate(
#     uninsured_rate = uninsured_total / total_population
#   ) %>%
#   dplyr::select(
#     State = NAME,
#     Sex = sex,
#     Uninsured = uninsured_total,
#     Total_Population = total_population,
#     Uninsured_Rate = uninsured_rate
#   )

# Pivot data to wide format for easier comparison between sexes
# uninsured_rate_summary <- uninsured_rate_data %>%
#   tidyr::pivot_wider(
#     names_from = Sex,
#     values_from = c(Uninsured, Total_Population, Uninsured_Rate),
#     names_glue = "{Sex}_{.value}"
#   ) %>%
#   dplyr::mutate(
#     Uninsured_Male_Rate = scales::percent(Male_Uninsured_Rate, accuracy = 0.1),
#     Uninsured_Female_Rate = scales::percent(Female_Uninsured_Rate, accuracy = 0.1)
#   )
#
# # Display the summarized data
# print(uninsured_rate_summary)

# Combine uninsured and total population data with metadata
uninsured_rate_data <- uninsured_summary %>%
  dplyr::left_join(total_population_data, by = c("NAME", "sex")) %>% # Join datasets by state and sex
  dplyr::distinct(NAME, sex, uninsured_total, total_population) %>% # Ensure no duplicates
  dplyr::mutate(
    uninsured_rate = uninsured_total / total_population # Calculate uninsured rate
  )

# Add metadata and descriptions
uninsured_rate_data <- uninsured_rate_data %>%
  dplyr::left_join(
    uninsured_data %>%
      dplyr::select(NAME, sex, year, survey, date_time_created, variable_description) %>%
      dplyr::distinct(NAME, sex, year, survey, date_time_created, variable_description),
    by = c("NAME", "sex")
  ) %>%
  dplyr::select(
    State = NAME,
    Sex = sex,
    Uninsured = uninsured_total,
    Total_Population = total_population,
    Uninsured_Rate = uninsured_rate,
    year,
    survey,
    date_time_created,
    variable_description
  )

# Pivot data to wide format for easier comparison between sexes
uninsured_rate_summary <- uninsured_rate_data %>%
  tidyr::pivot_wider(
    names_from = Sex, # Pivot on Sex column
    values_from = c(Uninsured, Total_Population, Uninsured_Rate), # Values to spread
    names_glue = "{Sex}_{.value}" # Naming convention for new columns
  ) %>%
  dplyr::mutate(
    Male_Uninsured_Rate = scales::percent(Male_Uninsured_Rate, accuracy = 0.1),
    Female_Uninsured_Rate = scales::percent(Female_Uninsured_Rate, accuracy = 0.1)
  )

# Display the final summarized data
View(uninsured_rate_summary)
