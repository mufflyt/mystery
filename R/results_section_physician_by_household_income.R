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
#' @importFrom scales comma
#' @importFrom logger log_info
#' @export
results_section_physician_by_household_income <- function(
    year = 2022,
    physician_information_with_zip = "Phase_2.rds"
) {

  # Vector of all state abbreviations, including DC and PR
  state_abbreviations <- c(state.abb, "DC", "PR")

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
  if (!requireNamespace("zipcodeR", quietly = TRUE)) {
    stop("The 'zipcodeR' package is required but not installed. Please install it using install.packages('zipcodeR').")
  }
  zip_data <- zipcodeR::zip_code_db %>%
    dplyr::rename(zip_code = zipcode) %>%
    dplyr::select(zip_code, state)

  income_ranges_by_state <- calculate_income_ranges(income_by_zip, zip_data)

  # Step 4: Assign physicians to income quartiles
  physician_distribution_by_quartile <- assign_income_quartiles(physician_details, income_by_zip, income_ranges_by_state)

  # Step 5: Add US-level summary for Q4
  physician_distribution_by_quartile <- add_us_summary(physician_distribution_by_quartile, income_by_zip, physician_details)

  # Log final output
  log_info("Completed physician distribution analysis. Returning final summary.")
  print(physician_distribution_by_quartile, n = nrow(physician_distribution_by_quartile))

  return(physician_distribution_by_quartile)
}

# Helper function: Calculate state-specific income ranges
calculate_income_ranges <- function(income_by_zip, zip_data) {
  log_info("Calculating state-specific income ranges for each quartile.")

  income_ranges <- income_by_zip %>%
    dplyr::inner_join(zip_data, by = "zip_code") %>%
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
