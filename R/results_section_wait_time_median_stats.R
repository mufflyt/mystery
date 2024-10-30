#' Generate Wait Time Statistics and Summary Sentences with Logging and Error Handling
#'
#' This function calculates the median wait time, 25th percentile (Q1), and 75th percentile (Q3)
#' for the `business_days_until_appointment` column both overall and grouped by a specified variable.
#' It logs each step to the console and includes error handling to ensure robustness.
#'
#' @param appointment_data A data frame containing appointment wait time data.
#' @param wait_time_col Character string; the name of the column representing wait time in business days.
#' @param group_var Character string; the name of the column to group by for grouped statistics (e.g., "Subspecialty").
#'                  If NULL, only overall statistics are calculated.
#' @param round_digits Integer; number of decimal places to round Q1 and Q3 values.
#'
#' @return A list containing two tibbles:
#' \describe{
#'   \item{stat_summary}{A tibble with calculated statistics for median wait time, Q1, Q3, and Group.}
#'   \item{sentence_summary}{A tibble with summary sentences for each group.}
#' }
#'
#' @importFrom dplyr summarise group_by mutate bind_cols bind_rows select ungroup
#' @importFrom rlang sym
#' @importFrom logger log_info
#' @examples
#' # Example usage
#' results <- results_section_wait_time_median_stats(
#'   appointment_data = df,
#'   wait_time_col = "business_days_until_appointment",
#'   group_var = "Subspecialty",
#'   round_digits = 1
#' )
#' results$stat_summary  # Contains statistical data
#' results$sentence_summary  # Contains summary sentences
#'
#' @export
results_section_wait_time_median_stats <- function(appointment_data, wait_time_col = "business_days_until_appointment",
                                                   group_var = NULL, round_digits = 1) {

  # Log function start and inputs
  logger::log_info("Starting wait time statistics calculation.")
  logger::log_info("Input details - Rows: {nrow(appointment_data)}, Wait Time Column: {wait_time_col}, Group Variable: {group_var}, Rounding Digits: {round_digits}")

  # Input validation
  if (!is.data.frame(appointment_data)) {
    stop_message <- "Error: Input must be a data frame."
    logger::log_info(stop_message)
    stop(stop_message)
  }

  if (!wait_time_col %in% names(appointment_data)) {
    stop_message <- paste("Error: Column", wait_time_col, "not found in the data frame.")
    logger::log_info(stop_message)
    stop(stop_message)
  }

  if (!is.null(group_var) && !group_var %in% names(appointment_data)) {
    stop_message <- paste("Error: Grouping variable", group_var, "not found in the data frame.")
    logger::log_info(stop_message)
    stop(stop_message)
  }

  # Ensure the wait time column is numeric
  if (!is.numeric(appointment_data[[wait_time_col]])) {
    stop_message <- "Error: The wait time column must be numeric."
    logger::log_info(stop_message)
    stop(stop_message)
  }

  # Calculate overall statistics
  logger::log_info("Calculating overall wait time statistics.")
  overall_wait_time_stats <- appointment_data %>%
    dplyr::ungroup() %>%
    dplyr::summarise(
      median_wait_time = median(!!rlang::sym(wait_time_col), na.rm = TRUE),
      wait_time_q1 = round(quantile(!!rlang::sym(wait_time_col), 0.25, na.rm = TRUE), round_digits),
      wait_time_q3 = round(quantile(!!rlang::sym(wait_time_col), 0.75, na.rm = TRUE), round_digits)
    )

  logger::log_info("Overall statistics: Median = {overall_wait_time_stats$median_wait_time}, Q1 = {overall_wait_time_stats$wait_time_q1}, Q3 = {overall_wait_time_stats$wait_time_q3}")

  # Generate sentence for overall statistics
  overall_summary <- sprintf(
    "The overall median wait time was %.1f business days (IQR: %.1f–%.1f).",
    overall_wait_time_stats$median_wait_time,
    overall_wait_time_stats$wait_time_q1,
    overall_wait_time_stats$wait_time_q3
  )
  logger::log_info("Overall summary sentence: {overall_summary}")

  # Calculate grouped statistics if group_var is provided
  if (!is.null(group_var)) {
    logger::log_info("Calculating grouped statistics by '{group_var}'...")
    grouped_wait_time_stats <- appointment_data %>%
      dplyr::group_by(!!rlang::sym(group_var)) %>%
      dplyr::summarise(
        median_wait_time = median(!!rlang::sym(wait_time_col), na.rm = TRUE),
        wait_time_q1 = round(quantile(!!rlang::sym(wait_time_col), 0.25, na.rm = TRUE), round_digits),
        wait_time_q3 = round(quantile(!!rlang::sym(wait_time_col), 0.75, na.rm = TRUE), round_digits),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        Group = !!rlang::sym(group_var),
        summary_sentence = sprintf(
          "For %s, the median wait time was %.1f business days (IQR: %.1f–%.1f).",
          !!rlang::sym(group_var),
          median_wait_time,
          wait_time_q1,
          wait_time_q3
        )
      )
    logger::log_info("Grouped statistics for '{group_var}' calculated successfully.")
  } else {
    grouped_wait_time_stats <- NULL
    logger::log_info("No grouping variable provided; only overall statistics calculated.")
  }

  # Combine overall and grouped statistics into final result
  logger::log_info("Combining overall and grouped statistics into the final output data frames.")
  overall_data <- dplyr::bind_cols(overall_wait_time_stats, dplyr::tibble(Group = "Overall", summary_sentence = overall_summary))

  if (is.null(grouped_wait_time_stats)) {
    stat_summary <- overall_data
  } else {
    stat_summary <- dplyr::bind_rows(overall_data, grouped_wait_time_stats)
  }

  # Create separate tibbles for statistics and sentences
  sentence_summary <- stat_summary %>%
    dplyr::select(Group, summary_sentence)

  # Select and arrange columns for clarity in stat_summary
  stat_summary <- stat_summary %>%
    dplyr::select(Group, median_wait_time, wait_time_q1, wait_time_q3, summary_sentence)

  # Log final output and completion
  logger::log_info("Final output data frames created: stat_summary and sentence_summary.")
  logger::log_info("stat_summary: {stat_summary}")
  logger::log_info("sentence_summary: {sentence_summary}")
  logger::log_info("results_section_wait_time_median_stats completed.")

  return(list(stat_summary = stat_summary, sentence_summary = sentence_summary))
}
