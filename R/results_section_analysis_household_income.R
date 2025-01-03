#' Analyze Physician Distribution by Household Income Quartile with Logging
#'
#' This function analyzes the distribution of physicians across household income quartiles
#' based on ZIP code data within each U.S. state. It logs all steps in the process, including
#' data retrieval, processing, and calculation of income quartiles.
#'
#' @param year Integer specifying the ACS survey year for household income data, defaults to 2022.
#' @param physician_information_with_zip String specifying the file path to the physician information dataset,
#' containing at least "ID" and "zip" columns.
#'
#' @return A tibble summarizing the distribution of physicians across household income quartiles
#' by state, including an overall summary for the U.S. in the highest income quartile.
#' @importFrom tidycensus get_acs
#' @importFrom dplyr left_join filter mutate select rename group_by summarize case_when arrange
#' @importFrom scales comma
#' @importFrom logger log_info
#'
#' @examples
#' \dontrun{
#' # Example 1: Standard analysis for ACS year 2022
#' physician_income_analysis <- results_section_analysis_household_income(
#'   year = 2022,
#'   physician_information_with_zip = "path/to/physician_data.rds"
#' )
#' print(physician_income_analysis)
#'
#' # Example 2: Specifying an earlier ACS year for historical analysis
#' physician_income_analysis <- results_section_analysis_household_income(
#'   year = 2020,
#'   physician_information_with_zip = "path/to/physician_data.rds"
#' )
#' head(physician_income_analysis)
#' }
#' @export
results_section_analysis_household_income <- function(year = 2022, physician_information_with_zip) {
  if (!requireNamespace("zipcodeR", quietly = TRUE)) {
    stop("The 'zipcodeR' package is required for this function. Please install it.")
  }


  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("The 'duckdb' package is required for this function. Please install it.")
  }

  con <- duckdb::dbConnect(duckdb::duckdb())


  con <- duckdb::dbConnect(duckdb::duckdb())


  # Log the start of the function
  logger::log_info("Starting analysis of physician distribution by household income quartile.")

  # Validate input
  if (!file.exists(physician_information_with_zip)) {
    stop("The specified file does not exist: ", physician_information_with_zip)
  }

  # Load physician data
  physician_data <- readr::read_rds(physician_information_with_zip)
  logger::log_info("Loaded physician data with {nrow(physician_data)} rows.")

  # Check if zipcodeR is installed
  if (!requireNamespace("zipcodeR", quietly = TRUE)) {
    stop("The 'zipcodeR' package is required for this function but is not installed. Please install it via install.packages('zipcodeR').")
  }

  # Use zipcodeR to enrich ZIP codes with household income data
  logger::log_info("Using zipcodeR to enrich data with ZIP code information.")
  zip_data <- zipcodeR::zip_code_db

  # Join physician data with ZIP code income data
  enriched_data <- physician_data %>%
    dplyr::left_join(
      zip_data %>%
        dplyr::select(zipcode, median_household_income) %>%
        dplyr::rename(zip = zipcode),
      by = "zip"
    )

  if (nrow(enriched_data) == 0) {
    stop("No matching ZIP codes found in the data.")
  }
  logger::log_info("Enriched physician data with ZIP code income information.")

  # Categorize data into income quartiles
  enriched_data <- enriched_data %>%
    dplyr::mutate(
      income_quartile = dplyr::case_when(
        median_household_income <= quantile(median_household_income, 0.25, na.rm = TRUE) ~ "Q1 (Lowest)",
        median_household_income > quantile(median_household_income, 0.25, na.rm = TRUE) &
          median_household_income <= quantile(median_household_income, 0.5, na.rm = TRUE) ~ "Q2",
        median_household_income > quantile(median_household_income, 0.5, na.rm = TRUE) &
          median_household_income <= quantile(median_household_income, 0.75, na.rm = TRUE) ~ "Q3",
        median_household_income > quantile(median_household_income, 0.75, na.rm = TRUE) ~ "Q4 (Highest)",
        TRUE ~ NA_character_
      )
    )

  # Summarize physician distribution by income quartile and state
  summary_data <- enriched_data %>%
    dplyr::group_by(state, income_quartile) %>%
    dplyr::summarize(
      physician_count = dplyr::n(),
      percent_in_quartile = 100 * physician_count / sum(physician_count),
      .groups = "drop"
    ) %>%
    dplyr::arrange(state, desc(income_quartile))

  logger::log_info("Physician distribution by income quartile and state summarized.")

  return(summary_data)
}
