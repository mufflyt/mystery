#' Exclusion Criteria Summary for Mystery Caller Study
#'
#' This function provides a summary of exclusion criteria applied to the mystery
#' caller study data. It identifies the count and percentage of physicians excluded
#' based on predefined criteria.
#'
#' @param call_data A data frame containing the mystery caller data, including
#'   exclusion criteria and call status columns.
#' @param exclusion_col A string specifying the column that contains exclusion
#'   reasons (e.g., `reason_for_exclusions`).
#' @param able_to_contact_value A string representing the value in the exclusion
#'   column that indicates a successful contact. Default is `"Able to contact"`.
#' @param group_var An optional string specifying a grouping variable (e.g., `specialty`).
#'   If provided, exclusions will be summarized within each group. Default is `NULL`.
#'
#' @return A formatted summary string describing the exclusions and their counts
#'   and percentages.
#'
#' @importFrom dplyr filter group_by summarize mutate ungroup
#' @importFrom logger log_info log_warn
#'
#' @examples
#' # Example 1: Basic exclusion summary
#' call_data <- data.frame(
#'   physician_id = 1:100,
#'   reason_for_exclusions = sample(c("Able to contact", "Went to voicemail",
#'                                    "Not accepting new patients"), 100, replace = TRUE)
#' )
#' summary <- results_section_exclusions(
#'   call_data = call_data,
#'   exclusion_col = "reason_for_exclusions"
#' )
#' print(summary)
#'
#' # Example 2: Summary with grouping variable
#' call_data$specialty <- sample(c("OBGYN", "Family Medicine"), 100, replace = TRUE)
#' grouped_summary <- results_section_exclusions(
#'   call_data = call_data,
#'   exclusion_col = "reason_for_exclusions",
#'   group_var = "specialty"
#' )
#' print(grouped_summary)
#' @export
results_section_exclusions <- function(call_data,
                                       exclusion_col = "reason_for_exclusions",
                                       able_to_contact_value = "Able to contact",
                                       group_var = NULL) {

  # Log function start and inputs
  logger::log_info("Starting exclusions summary calculation.")
  logger::log_info("Input details - Total rows in call_data: {nrow(call_data)}, exclusion_col: {exclusion_col}, able_to_contact_value: {able_to_contact_value}, group_var: {group_var}")

  # Input validation
  if (!is.data.frame(call_data)) {
    stop("Error: call_data must be a data frame.")
  }
  if (!exclusion_col %in% names(call_data)) {
    stop(paste("Error: Column", exclusion_col, "not found in call_data."))
  }
  if (!is.character(able_to_contact_value) || length(able_to_contact_value) != 1) {
    stop("Error: able_to_contact_value must be a single character string.")
  }
  if (!is.null(group_var) && !group_var %in% names(call_data)) {
    stop(paste("Error: Grouping column", group_var, "not found in call_data."))
  }

  # Ensure data is ungrouped before processing
  call_data <- dplyr::ungroup(call_data)

  # Check and log NA values in exclusion column
  na_count <- sum(is.na(call_data[[exclusion_col]]))
  if (na_count > 0) {
    logger::log_info("Found {na_count} NA values in {exclusion_col}. Excluding these from calculations.")
  }

  # Filter out NA values in exclusion column
  call_data <- call_data %>%
    dplyr::filter(!is.na(.data[[exclusion_col]]))

  # Total number of calls
  total_phone_calls <- nrow(call_data)

  # Filter for unsuccessful connections
  unsuccessful_connections_data <- call_data %>%
    dplyr::filter(.data[[exclusion_col]] != able_to_contact_value)

  # Summarize exclusions
  if (!is.null(group_var)) {
    exclusion_summary <- unsuccessful_connections_data %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarize(
        total_exclusions = dplyr::n(),
        .groups = "drop"
      )
    logger::log_info("Exclusion summary by group created.")
  } else {
    exclusion_summary <- unsuccessful_connections_data %>%
      dplyr::summarize(
        total_exclusions = dplyr::n()
      )
  }

  # Calculate successful connections
  successful_connections <- total_phone_calls - exclusion_summary$total_exclusions
  summary_sentence <- sprintf(
    "Of the total %d phone calls made, %d were successfully connected, and %d were excluded.",
    total_phone_calls, successful_connections, exclusion_summary$total_exclusions
  )

  # Log the output and function completion
  logger::log_info("Generated exclusion summary: {summary_sentence}")
  return(summary_sentence)
}
