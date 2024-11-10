#' Analyze Physician Distribution by Household Income Quartile with Logging
#'
#' This function analyzes the distribution of physicians across household income quartiles based on ZIP code data within each U.S. state.
#' It logs all steps in the process, including data retrieval, processing, and calculation of income quartiles.
#'
#' @param year Integer specifying the ACS survey year for household income data, defaults to 2022.
#' @param physician_information_with_zip String specifying the file path to the physician information dataset, containing at least "ID" and "zip" columns.
#'
#' @return A tibble summarizing the distribution of physicians across household income quartiles by state, including an overall summary for the U.S. in the highest income quartile.
#' @importFrom tidycensus get_acs
#' @importFrom dplyr left_join filter mutate select rename group_by summarize case_when arrange
#' @import zipcodeR
#' @importFrom scales comma
#' @importFrom logger log_info
#'
#' @examples
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
#'
#' # Example 3: Run with another dataset and different year
#' result <- results_section_analysis_household_income(2021, "data/physicians_data.rds")
#' print(result)
#' @export
results_section_analysis_household_income <- function(physician_income_of_neighborhood) {

  # Log function start
  log_info("Starting analysis of physician distribution by household income quartile.")

  # Validate input
  validate_input_data(physician_income_of_neighborhood)

  # Generate national Q4 summary
  national_summary_q4 <- calculate_national_summary_q4(physician_income_of_neighborhood)

  # Generate state-level summaries for Q4
  state_summaries_q4 <- calculate_state_summaries_q4(physician_income_of_neighborhood)

  # Generate state-level summaries for Q1
  state_q1_distribution <- calculate_state_summaries_q1(physician_income_of_neighborhood)

  # Identify high and low Q1 states
  high_q1_states <- identify_high_q1_states(state_q1_distribution)
  low_q1_states <- identify_low_q1_states(state_q1_distribution)

  # Combine all summaries into a single dataframe
  combined_results <- bind_rows(
    data.frame(
      type = "National Q4 Summary",
      summary = national_summary_q4
    ),
    data.frame(
      type = "State Q4 Summary",
      summary = state_summaries_q4
    ),
    data.frame(
      type = "High Q1 States",
      summary = high_q1_states
    ),
    data.frame(
      type = "Low Q1 States",
      summary = low_q1_states
    )
  )

  # Log function completion
  log_info("Completed analysis of physician distribution by household income quartile.")

  return(combined_results)
}

# Helper function: Validate input data
#' @noRd
validate_input_data <- function(physician_income_of_neighborhood) {
  if (!"income_quartile" %in% colnames(physician_income_of_neighborhood) ||
      !"state" %in% colnames(physician_income_of_neighborhood)) {
    stop("The input data must include columns 'income_quartile' and 'state'.")
  }
  log_info("Input data validated successfully.")
}

# Helper function: Calculate national Q4 summary
#' @noRd
calculate_national_summary_q4 <- function(physician_income_of_neighborhood) {
  national_q4 <- physician_income_of_neighborhood %>%
    dplyr::filter(state == "US", income_quartile == "Q4 (Highest)") %>%
    dplyr::select(percent_in_quartile, income_range) %>%
    dplyr::mutate(
      summary = paste0("Overall, ", round(percent_in_quartile, 1),
                       "% of physicians in the U.S. are situated in the most affluent ZIP codes (Q4), with median household incomes ranging from ", income_range, ".")
    )
  log_info("National Q4 summary calculated.")
  national_q4$summary
}

# Helper function: Calculate state summaries for Q4
#' @noRd
calculate_state_summaries_q4 <- function(physician_income_of_neighborhood) {
  physician_income_of_neighborhood %>%
    dplyr::filter(income_quartile == "Q4 (Highest)", state != "US") %>%
    dplyr::select(state, percent_in_quartile, income_range) %>%
    dplyr::arrange(desc(percent_in_quartile)) %>%
    dplyr::mutate(
      summary = paste0("In ", state, ", ", round(percent_in_quartile, 1),
                       "% of physicians are located in the most affluent ZIP codes (Q4), with income range ", income_range, ".")
    ) %>%
    dplyr::pull(summary) %>%
    {log_info("State Q4 summaries calculated."); .}
}

# Helper function: Calculate state summaries for Q1
#' @noRd
calculate_state_summaries_q1 <- function(physician_income_of_neighborhood) {
  physician_income_of_neighborhood %>%
    dplyr::filter(income_quartile == "Q1 (Lowest)", state != "US") %>%
    dplyr::select(state, percent_in_quartile, income_range) %>%
    dplyr::arrange(desc(percent_in_quartile)) %>%
    dplyr::mutate(
      summary = paste0("In ", state, ", ", round(percent_in_quartile, 1),
                       "% of physicians are located in the lowest income quartile (Q1), with income range ", income_range, ".")
    ) %>%
    {log_info("State Q1 summaries calculated."); .}
}

# Helper function: Identify top states with high Q1 concentrations
#' @noRd
identify_high_q1_states <- function(state_q1_distribution) {
  state_q1_distribution %>%
    dplyr::slice(1:5) %>%
    dplyr::pull(summary) %>%
    {log_info("Top states with high Q1 physician concentrations identified."); .}
}

# Helper function: Identify bottom states with low Q1 concentrations
#' @noRd
identify_low_q1_states <- function(state_q1_distribution) {
  state_q1_distribution %>%
    dplyr::arrange(percent_in_quartile) %>%
    dplyr::slice(1:5) %>%
    dplyr::pull(summary) %>%
    {log_info("Bottom states with low Q1 physician concentrations identified."); .}
}
