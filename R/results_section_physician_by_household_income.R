#' Physician Distribution by Household Income Quartile
#'
#' This function retrieves ACS data for household income at the ZIP code level, classifies physicians by income quartiles
#' within each state, and returns a summary of the distribution of physicians across income quartiles for each state
#' as well as an overall summary for the U.S. The function also logs the process, including inputs, outputs, and each step
#' of the analysis.
#'
#' @param year Integer. The year of the ACS survey. Defaults to the latest ACS 5-year survey available (e.g., 2022).
#' @param physician_information_with_zip String. The file path to the physician information dataset. This dataset must contain "ID" and "zip" columns.
#'
#' @return A tibble summarizing physician distribution by household income quartile across each state.
#'         The tibble contains the columns:
#'         - `state`: State or "US" for the U.S. summary.
#'         - `income_quartile`: The income quartile (Q1 - Q4).
#'         - `income_range`: The income range for the quartile.
#'         - `physicians_in_quartile`: The number of physicians in the quartile.
#'         - `total_physicians`: The total number of physicians in the state.
#'         - `percent_in_quartile`: The percentage of physicians in the quartile.
#'
#' @examples
#' \dontrun{
#' # Example 1: Default behavior with specified ACS year and physician data file
#' physician_summary <- results_section_physician_by_household_income(
#'   year = 2022,
#'   physician_information_with_zip = "path/to/Phase_2.rds"
#' )
#' print(physician_summary)
#'
#' # Example 2: Running with a different ACS year and a different file path
#' physician_summary_2020 <- results_section_physician_by_household_income(
#'   year = 2020,
#'   physician_information_with_zip = "path/to/Phase_2_2020.rds"
#' )
#' print(physician_summary_2020)
#'
#' # Example 3: Checking the structure of the output
#' print(head(physician_summary))
#' }
#'
#' @importFrom tidycensus get_acs
#' @importFrom dplyr left_join filter mutate select rename group_by summarize case_when arrange
#' @importFrom scales comma
#' @importFrom logger log_info
#' @export
results_section_physician_by_household_income <- function(
    year = 2022,
    physician_information_with_zip = "Phase_2.rds") {
  required_physician_columns <- c("ID", "zip")

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

  # Confirm required columns in ACS income data
  required_income_columns <- c("zip_code", "median_income")
  if (!all(required_income_columns %in% colnames(income_by_zip))) {
    stop(
      "The ACS income data is missing required columns: ",
      paste(setdiff(required_income_columns, colnames(income_by_zip)), collapse = ", ")
    )
  }

  # Step 2: Load physician details
  physician_details <- load_physician_data(physician_information_with_zip)

  # Confirm required columns in physician data
  if (!all(required_physician_columns %in% colnames(physician_details))) {
    stop(
      "The physician data is missing required columns: ",
      paste(setdiff(required_physician_columns, colnames(physician_details)), collapse = ", ")
    )
  }

  # Step 3: Calculate state-specific income ranges
  zip_data <- zipcodeR::zip_code_db %>%
    dplyr::rename(zip_code = zipcode) %>%
    dplyr::select(zip_code, state)

  income_ranges_by_state <- calculate_income_ranges(income_by_zip, zip_data)

  # Confirm required columns in income ranges
  required_range_columns <- c("state", "Q1_min", "Q1_max", "Q2_min", "Q2_max", "Q3_min", "Q3_max", "Q4_min", "Q4_max")
  if (!all(required_range_columns %in% colnames(income_ranges_by_state))) {
    stop(
      "The income ranges data is missing required columns: ",
      paste(setdiff(required_range_columns, colnames(income_ranges_by_state)), collapse = ", ")
    )
  }

  # Step 4: Assign physicians to income quartiles
  physician_distribution_by_quartile <- assign_income_quartiles(physician_details, income_by_zip, income_ranges_by_state)

  # Step 5: Add US-level summary for Q4
  physician_distribution_by_quartile <- add_us_summary(physician_distribution_by_quartile, income_by_zip, physician_details)

  # Log final output
  log_info("Completed physician distribution analysis. Returning final summary.")

  print(physician_distribution_by_quartile, n = nrow(physician_distribution_by_quartile))
  return(physician_distribution_by_quartile)
}
