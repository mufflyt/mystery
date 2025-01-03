#' Generate Wait Time Estimates with 95% CI Using Poisson Regression
#'
#' This function calculates the estimated wait time in business days and a 95% confidence interval
#' using Poisson regression for the `business_days_until_appointment` column, both overall and grouped.
#' It logs each step to the console and includes error handling.
#'
#' @param appointment_data A data frame containing appointment wait time data.
#' @param wait_time_col Character string; the name of the column representing wait time in business days.
#' @param group_var Character string; the name of the column to group by for grouped estimates.
#' @param round_digits Integer; number of decimal places to round estimates.
#'
#' @return A list containing two tibbles:
#' \describe{
#'   \item{stat_summary}{A tibble with estimated wait times, 95% CI, and summary sentences for each group.}
#'   \item{sentence_summary}{A tibble with summary sentences for each group.}
#' }
#'
#' @importFrom dplyr summarise group_by mutate bind_cols bind_rows select ungroup filter
#' @importFrom rlang sym
#' @importFrom logger log_info
#' @importFrom stats glm predict confint.default
#' @examples
#' # Example usage
#' results <- results_section_wait_time_poisson_estimates(
#'   appointment_data = df,
#'   wait_time_col = "business_days_until_appointment",
#'   group_var = "Subspecialty",
#'   round_digits = 1
#' )
#' results$stat_summary # Contains statistical data
#' results$sentence_summary # Contains summary sentences
#'
#' @export
results_section_wait_time_poisson_estimates <- function(appointment_data, wait_time_col = "business_days_until_appointment",
                                                        group_var = NULL, round_digits = 1) {
  # Adjust console display options to widen the view for better readability
  options(tibble.width = Inf)

  log_function_start(appointment_data, wait_time_col, group_var, round_digits)

  validate_inputs(appointment_data, wait_time_col, group_var)

  overall_statistics <- calculate_overall_estimate(appointment_data, wait_time_col, round_digits)
  logger::log_info("Overall estimate: {overall_statistics$estimate} business days, 95% CI: [{overall_statistics$ci_lower}, {overall_statistics$ci_upper}]")

  if (!is.null(group_var)) {
    grouped_statistics <- calculate_grouped_estimates(appointment_data, wait_time_col, group_var, round_digits)
    logger::log_info("Grouped estimates with 95%% CI for '{group_var}' calculated successfully.")
  } else {
    grouped_statistics <- NULL
  }

  combined_statistics <- combine_statistics(overall_statistics, grouped_statistics)
  logger::log_info("Final output data frames created: stat_summary and sentence_summary.")

  return(combined_statistics)
}
