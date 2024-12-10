# Load necessary libraries
library(tidycensus)   # For accessing and analyzing U.S. Census and ACS data
library(tidyverse)    # For data manipulation and visualization
library(duckplyr)     # For fast and efficient data wrangling
library(logger)       # For logging messages during function execution

# Load ACS variable metadata for the 2022 1-year ACS dataset
acs_vars_2022 <- tidycensus::load_variables(dataset = "acs1", year = 2022)

# Filter to find variables related to Medicare coverage using the variable names
# B27006 is the Medicare coverage table
medicare_vars <- acs_vars_2022 %>%
  dplyr::filter(stringr::str_detect(name, "B27006"))  # Filter variable names containing "B27006"
print(medicare_vars, n = 100)  # Display the filtered variables (up to 100)

# Filter to find variables specifically labeled as "With Medicare"
# This further narrows down the results to individuals with Medicare coverage
medicare_vars <- acs_vars_2022 %>%
  dplyr::filter(stringr::str_detect(label, "With Medicare"))  # Filter labels containing "With Medicare"
print(medicare_vars, n = 100)  # Display the filtered variables (up to 100)

# Define the variable codes for males with Medicare coverage by age group
with_medicare_coverage_male <- c(
  "B27006_004", # Under 6 years
  "B27006_007", # 6 to 18 years
  "B27006_010", # 19 to 25 years
  "B27006_013", # 26 to 34 years
  "B27006_016", # 35 to 44 years
  "B27006_019", # 45 to 54 years
  "B27006_022", # 55 to 64 years
  "B27006_025", # 65 to 74 years
  "B27006_028"  # 75 years and over
)

# Define the variable codes for females with Medicare coverage by age group
with_medicare_coverage_female <- c(
  "B27006_032", # Under 6 years
  "B27006_035", # 6 to 18 years
  "B27006_038", # 19 to 25 years
  "B27006_041", # 26 to 34 years
  "B27006_044", # 35 to 44 years
  "B27006_047", # 45 to 54 years
  "B27006_050", # 55 to 64 years
  "B27006_053", # 65 to 74 years
  "B27006_056"  # 75 years and over
)

# Function to fetch Medicare coverage data for a given year
fetch_medicare_coverage_data <- function(year, survey = "acs5") {
  # Log the start of data fetching
  logger::log_info("Fetching Medicare coverage data for {year} using survey {survey}")

  # Load ACS variables for the specified year
  acs_vars <- tidycensus::load_variables(dataset = survey, year = year)

  # Filter variables for "With Medicare" coverage
  medicare_vars <- acs_vars %>%
    dplyr::filter(stringr::str_detect(label, "With Medicare"))  # Select only variables containing "With Medicare"

  # Define variables for males and females
  with_medicare_coverage_male <- medicare_vars %>%
    dplyr::filter(stringr::str_detect(label, "Male")) %>%
    dplyr::pull(name)

  with_medicare_coverage_female <- medicare_vars %>%
    dplyr::filter(stringr::str_detect(label, "Female")) %>%
    dplyr::pull(name)

  # Retrieve Medicare coverage data for males
  medicare_data_male <- tidycensus::get_acs(
    geography = "state",                 # Get data at the state level
    variables = with_medicare_coverage_male, # Variables for males by age group
    year = year,                         # Specify the year of the data
    survey = survey                      # Use the specified survey type
  )

  # Retrieve Medicare coverage data for females
  medicare_data_female <- tidycensus::get_acs(
    geography = "state",                 # Get data at the state level
    variables = with_medicare_coverage_female, # Variables for females by age group
    year = year,                         # Specify the year of the data
    survey = survey                      # Use the specified survey type
  )

  # Combine male and female data into a single dataframe
  medicare_data <- dplyr::bind_rows(
    medicare_data_male %>% dplyr::mutate(sex = "Male"),   # Add a column to identify male data
    medicare_data_female %>% dplyr::mutate(sex = "Female") # Add a column to identify female data
  )

  # Add variable descriptions from medicare_vars
  medicare_data <- medicare_data %>%
    dplyr::left_join(
      medicare_vars %>% dplyr::select(name, label), # Select variable names and descriptions
      by = c("variable" = "name")                  # Join on the variable column
    ) %>%
    dplyr::rename(variable_description = label)     # Rename the column for clarity

  # Add additional metadata columns
  medicare_data <- medicare_data %>%
    dplyr::mutate(
      year = year,                      # Add the year column
      survey = survey,                  # Add the survey type column
      date_time_created = Sys.time()    # Add the current date and time column
    ) %>%
    dplyr::rename(medicare_population = estimate)

  # Log the successful completion of data fetching
  logger::log_info("Data fetched successfully for {year} using survey {survey}")

  # Return the combined dataset
  return(medicare_data)
}

####
#######The 1-year ACS provides data for geographies with populations of 65,000 and greater.###########
####
medicare_coverage <- fetch_medicare_coverage_data(year = 2022, survey = "acs1")
print(head(medicare_coverage))
