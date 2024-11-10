# Load required libraries
library(tidycensus)
library(dplyr)
library(zipcodeR)
library(scales)
library(logger)
library(tidyverse)

# Vector of all state abbreviations, including DC and PR
state_abbreviations <- c(state.abb, "DC", "PR")

#' Physician Distribution by Household Income Quartile
#'
#' This function retrieves ACS data for household income at the ZIP code level and classifies physicians by income quartiles
#' within each state. It provides a summary of the distribution of physicians across income quartiles for each state
#' and an overall summary for the U.S.
#'
#' @param year Integer. The year of the ACS survey. Defaults to the latest ACS 5-year survey available.
#' @param physician_information_with_zip String. The file path to the physician information dataset. Must contain "ID" and "zip" columns.
#'
#' @return A tibble summarizing physician distribution by household income quartile across each state.
#' @examples
#' \dontrun{
#' # Example 1: Default behavior with specified ACS year and physician data file
#' physician_summary <- results_section_physician_by_household_income(
#'   year = 2022,
#'   physician_information_with_zip = "path/to/Phase_2.rds"
#' )
#'
#' # Example 2: Running with a different ACS year
#' physician_summary <- results_section_physician_by_household_income(
#'   year = 2020,
#'   physician_information_with_zip = "path/to/Phase_2.rds"
#' )
#'
#' # Example 3: Checking the output structure
#' print(physician_summary, n = nrow(physician_summary))
#' }
#' @importFrom tidycensus get_acs
#' @importFrom dplyr left_join filter mutate select rename group_by summarize case_when arrange
#' @import zipcodeR
#' @importFrom scales comma
#' @importFrom logger log_info
#' @export
results_section_physician_by_household_income <- function(
    year = 2022,
    physician_information_with_zip = "Phase_2.rds"
) {

  # Log inputs
  log_info("Starting physician distribution analysis for ACS survey year: {year}")
  log_info("Physician information file path: {physician_information_with_zip}")

  # Validate inputs
  if (!is.numeric(year) || year < 2009 || year > as.numeric(format(Sys.Date(), "%Y"))) {
    stop("Invalid 'year': Must be a numeric year between 2009 and the current year.")
  }
  if (!file.exists(physician_information_with_zip)) {
    stop("The file provided in 'physician_information_with_zip' does not exist.")
  }

  # Step 1: Retrieve ACS income data
  income_by_zip <- get_income_data(year)

  # Step 2: Load physician details
  physician_details <- load_physician_data(physician_information_with_zip)

  # Step 3: Calculate state-specific income ranges
  income_ranges_by_state <- calculate_income_ranges(income_by_zip)

  # Step 4: Assign physicians to income quartiles
  physician_distribution_by_quartile <- assign_income_quartiles(physician_details, income_by_zip, income_ranges_by_state)

  # Step 5: Add US-level summary for Q4
  physician_distribution_by_quartile <- add_us_summary(physician_distribution_by_quartile, income_by_zip, physician_details)

  # Log final output
  log_info("Completed physician distribution analysis. Returning final summary.")
  print(physician_distribution_by_quartile, n = nrow(physician_distribution_by_quartile))

  return(physician_distribution_by_quartile)
}

# Helper function to get income data
get_income_data <- function(year) {
  log_info("Retrieving ACS data for median household income by ZIP code for year: {year}")
  income_data <- tidycensus::get_acs(
    geography = "zcta",
    variables = "B19013_001",
    survey = "acs5",
    year = year,
    cache_table = TRUE
  ) %>%
    dplyr::select(-moe) %>%
    dplyr::rename(zip_code = GEOID, median_income = estimate) %>%
    dplyr::mutate(zip_code = as.character(zip_code)) %>%
    dplyr::filter(!is.na(median_income))

  log_info("Retrieved and processed income data with {nrow(income_data)} records.")
  return(income_data)
}

# Helper function to load physician data
load_physician_data <- function(file_path) {
  log_info("Loading physician data from file: {file_path}")
  physician_data <- readRDS(file_path) %>%
    dplyr::rename(id_number = ID, zip_code = zip) %>%
    dplyr::mutate(zip_code = as.character(zip_code)) %>%
    dplyr::mutate(state = exploratory::statecode(state, output = "alpha_code"))

  log_info("Loaded physician data with {nrow(physician_data)} records.")
  return(physician_data)
}

# Helper function to calculate state-specific income ranges
#' @noRd
calculate_income_ranges <- function(income_by_zip) {
  log_info("Calculating state-specific income ranges for each quartile.")
  zip_state_mapping <- zipcodeR::zip_code_db %>%
    dplyr::rename(zip_code = zipcode) %>%
    dplyr::select(zip_code, state)

  income_ranges <- income_by_zip %>%
    dplyr::inner_join(zip_state_mapping, by = "zip_code") %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(
      Q1_min = round(min(median_income[median_income <= quantile(median_income, 0.25, na.rm = TRUE)], na.rm = TRUE)),
      Q1_max = round(quantile(median_income, 0.25, na.rm = TRUE)),
      Q2_min = round(quantile(median_income, 0.25, na.rm = TRUE)),
      Q2_max = round(quantile(median_income, 0.50, na.rm = TRUE)),
      Q3_min = round(quantile(median_income, 0.50, na.rm = TRUE)),
      Q3_max = round(quantile(median_income, 0.75, na.rm = TRUE)),
      Q4_min = round(quantile(median_income, 0.75, na.rm = TRUE)),
      Q4_max = round(max(median_income, na.rm = TRUE))
    )
  log_info("Calculated income ranges for {nrow(income_ranges)} states.")
  return(income_ranges)
}

# Helper function to assign income quartiles to each physician and summarize
#' @noRd
assign_income_quartiles <- function(physician_details, income_by_zip, income_ranges_by_state) {
  log_info("Assigning income quartiles to each physician based on ZIP code income ranges.")
  physicians_with_income_ranges <- physician_details %>%
    dplyr::left_join(income_by_zip, by = "zip_code") %>%
    dplyr::left_join(income_ranges_by_state, by = "state") %>%
    dplyr::filter(!is.na(median_income)) %>%
    dplyr::mutate(
      income_quartile = case_when(
        median_income >= Q1_min & median_income <= Q1_max ~ "Q1 (Lowest)",
        median_income > Q2_min & median_income <= Q2_max ~ "Q2",
        median_income > Q3_min & median_income <= Q3_max ~ "Q3",
        median_income > Q4_min & median_income <= Q4_max ~ "Q4 (Highest)"
      ),
      income_range = case_when(
        income_quartile == "Q1 (Lowest)" ~ paste0("$", scales::comma(round(Q1_min)), " - $", scales::comma(round(Q1_max))),
        income_quartile == "Q2" ~ paste0("$", scales::comma(round(Q2_min)), " - $", scales::comma(round(Q2_max))),
        income_quartile == "Q3" ~ paste0("$", scales::comma(round(Q3_min)), " - $", scales::comma(round(Q3_max))),
        income_quartile == "Q4 (Highest)" ~ paste0("$", scales::comma(round(Q4_min)), " - $", scales::comma(round(Q4_max)))
      )
    ) %>%
    dplyr::filter(!is.na(income_quartile))

  physician_summary <- physicians_with_income_ranges %>%
    dplyr::group_by(state, income_quartile, income_range) %>%
    dplyr::summarize(physicians_in_quartile = n(), .groups = "drop") %>%
    dplyr::left_join(
      physician_details %>%
        dplyr::group_by(state) %>%
        dplyr::summarize(total_physicians = n(), .groups = "drop"),
      by = "state"
    ) %>%
    dplyr::mutate(
      percent_in_quartile = (physicians_in_quartile / total_physicians) * 100
    ) %>%
    dplyr::arrange(state, income_quartile)

  log_info("Completed physician distribution by income quartile.")
  return(physician_summary)
}

# Helper function to add US-level summary for affluent ZIP codes
#' @noRd
add_us_summary <- function(physician_distribution_by_quartile, income_by_zip, physician_details) {
  log_info("Calculating U.S.-level summary for affluent ZIP codes (Q4).")
  total_physicians_us <- nrow(physician_details)
  physicians_in_affluent_quartile_us <- nrow(physician_distribution_by_quartile %>%
                                               dplyr::filter(income_quartile == "Q4 (Highest)"))
  percent_in_affluent_quartile_us <- (physicians_in_affluent_quartile_us / total_physicians_us) * 100
  affluent_range_us <- paste0(
    "$", scales::comma(round(quantile(income_by_zip$median_income, 0.75, na.rm = TRUE))),
    " - $", scales::comma(round(max(income_by_zip$median_income, na.rm = TRUE)))
  )

  us_summary <- tibble(
    state = "US",
    income_quartile = "Q4 (Highest)",
    income_range = affluent_range_us,
    total_physicians = total_physicians_us,
    physicians_in_quartile = physicians_in_affluent_quartile_us,
    percent_in_quartile = percent_in_affluent_quartile_us
  )

  physician_distribution_with_us <- dplyr::bind_rows(physician_distribution_by_quartile, us_summary)
  log_info("Added U.S.-level summary to the distribution data.")
  return(physician_distribution_with_us)
}
