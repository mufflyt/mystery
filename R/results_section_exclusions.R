results_section_exclusions <- function(call_data,
                                       exclusion_col = "reason_for_exclusions",
                                       able_to_contact_value = "Able to contact",
                                       group_var = NULL) {

  # Log function start and inputs
  logger::log_info("Starting exclusions summary calculation.")
  logger::log_info("IMPORTANT: MAKE SURE THAT YOU USE NON-EXCLUDED DATA HERE. YOU WANT TO SEE THE EXCLUSIONS.")
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
  logger::log_info("Data ungrouped for processing.")

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
  logger::log_info("Calculated total number of phone calls after removing NA values: {total_phone_calls}")

  # Filter for unsuccessful connections (not able to contact)
  unsuccessful_connections_data <- call_data %>%
    dplyr::filter(.data[[exclusion_col]] != able_to_contact_value)
  logger::log_info("Filtered data to exclude '{able_to_contact_value}'. Rows remaining after filtering: {nrow(unsuccessful_connections_data)}")

  # Group data if group_var is specified
  if (!is.null(group_var)) {
    grouped_exclusions <- unsuccessful_connections_data %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarise(
        busy_signal = sum(.data[[exclusion_col]] == "Phone not answered or busy signal on repeat calls"),
        voicemail = sum(.data[[exclusion_col]] == "Went to voicemail"),
        referral_required = sum(.data[[exclusion_col]] == "Physician referral required before scheduling appointment"),
        not_accepting_new_patients = sum(.data[[exclusion_col]] == "Not accepting new patients"),
        long_hold = sum(.data[[exclusion_col]] == "Greater than 5 minutes on hold"),
        total_exclusions = dplyr::n()
      ) %>%
      dplyr::ungroup()

    # Log each specialty group’s exclusion data
    grouped_exclusions %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        log_msg = sprintf(
          "For group '%s': Busy signals = %d, Voicemail redirects = %d, Referral required = %d, Not accepting new patients = %d, Long hold time = %d, Total exclusions = %d",
          !!sym(group_var), busy_signal, voicemail, referral_required, not_accepting_new_patients, long_hold, total_exclusions
        )
      ) %>%
      dplyr::pull(log_msg) %>%
      purrr::walk(logger::log_info)
  }

  # Calculate counts and percentages for overall summary
  busy_signal_count <- sum(unsuccessful_connections_data[[exclusion_col]] == "Phone not answered or busy signal on repeat calls")
  voicemail_count <- sum(unsuccessful_connections_data[[exclusion_col]] == "Went to voicemail")
  total_unsuccessful <- busy_signal_count + voicemail_count
  logger::log_info("Unsuccessful connections: Busy signals = {busy_signal_count}, Voicemail redirects = {voicemail_count}, Total unsuccessful = {total_unsuccessful}")

  # Successful connections and reasons for exclusion
  successful_connections <- total_phone_calls - total_unsuccessful
  referral_required_count <- sum(unsuccessful_connections_data[[exclusion_col]] == "Physician referral required before scheduling appointment")
  not_accepting_new_patients_count <- sum(unsuccessful_connections_data[[exclusion_col]] == "Not accepting new patients")
  long_hold_count <- sum(unsuccessful_connections_data[[exclusion_col]] == "Greater than 5 minutes on hold")
  logger::log_info("Exclusions for successful connections: Referral required = {referral_required_count}, Not accepting new patients = {not_accepting_new_patients_count}, Long hold time = {long_hold_count}, Total successful connections = {successful_connections}")

  # Formatted summary sentence
  summary_sentence <- sprintf(
    "Of the total %d phone calls made, %d (%.0f%%) successfully reached a front desk representative, while %d calls (%.0f%%) did not yield a connection even after two attempts. Among unsuccessful connections, %d (%.0f%%) were redirected to voicemail, and %d (%.0f%%) reached a busy signal. For successful connections, the reasons for exclusion were %d (%.0f%%) requiring a prior referral, %d (%.0f%%) not currently accepting new patients, and %d (%.0f%%) offices putting the caller on hold for more than five minutes.",
    total_phone_calls,
    successful_connections, round(successful_connections / total_phone_calls * 100, 1),
    total_unsuccessful, round(total_unsuccessful / total_phone_calls * 100, 1),
    voicemail_count, round(voicemail_count / total_unsuccessful * 100, 1),
    busy_signal_count, round(busy_signal_count / total_unsuccessful * 100, 1),
    referral_required_count, round(referral_required_count / successful_connections * 100, 1),
    not_accepting_new_patients_count, round(not_accepting_new_patients_count / successful_connections * 100, 1),
    long_hold_count, round(long_hold_count / successful_connections * 100, 1)
  )

  # Log final output and function completion
  logger::log_info("Exclusion summary sentence created: {summary_sentence}")
  logger::log_info("results_section_exclusions completed.")

  return(summary_sentence)
}
