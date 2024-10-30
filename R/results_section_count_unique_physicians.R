#' Count Unique Physicians Based on Insurance Type and Exclusion Reason
#'
#' This function filters a dataframe of physician data based on insurance type, reason for exclusion,
#' and appointment availability, then counts the number of unique physicians who meet the criteria.
#'
#' @param df A dataframe containing physician data. Must include columns 'insurance', 'reason_for_exclusions',
#' 'business_days_until_appointment', and 'phone'.
#' @param insurance_type A string specifying the insurance type to filter by (e.g., "Medicaid").
#' @param reason_for_exclusion A string specifying the reason for exclusion to filter by. Default is NULL,
#' which includes all rows regardless of the exclusion reason.
#' @param verbose A boolean indicating whether to print detailed logs. Default is TRUE.
#'
#' @return An integer representing the number of unique physicians who meet the specified criteria.
#'
#' @examples
#' # Example 1: Counting unique physicians with specific insurance type and reason for exclusion
#' df <- data.frame(
#'   insurance = c("Medicaid", "Medicaid", "Blue Cross/Blue Shield", "Medicaid"),
#'   reason_for_exclusions = c("Able to contact", "Not available", "Able to contact", "Able to contact"),
#'   business_days_until_appointment = c(5, 0, 10, 3),
#'   phone = c("123-456-7890", "123-456-7890", "098-765-4321", "234-567-8901")
#' )
#' unique_count <- count_unique_physicians(df, insurance_type = "Medicaid", reason_for_exclusion = "Able to contact")
#' print(unique_count)  # Expected output: 1
#'
#' # Example 2: Counting unique physicians without specifying a reason for exclusion
#' df2 <- data.frame(
#'   insurance = c("Blue Cross/Blue Shield", "Blue Cross/Blue Shield", "Medicaid"),
#'   reason_for_exclusions = c("Able to contact", "Not available", "Able to contact"),
#'   business_days_until_appointment = c(3, 5, 1),
#'   phone = c("321-654-0987", "321-654-0987", "654-321-0987")
#' )
#' unique_count2 <- count_unique_physicians(df2, insurance_type = "Blue Cross/Blue Shield")
#' print(unique_count2)  # Expected output: 1
#'
#' # Example 3: Using verbose logging to see detailed steps
#' df3 <- data.frame(
#'   insurance = c("Medicaid", "Medicaid", "Medicaid", "Medicaid"),
#'   reason_for_exclusions = c("Able to contact", "Able to contact", "Not available", "Able to contact"),
#'   business_days_until_appointment = c(2, 1, 0, 4),
#'   phone = c("111-222-3333", "111-222-3333", "222-333-4444", "333-444-5555")
#' )
#' unique_count3 <- count_unique_physicians(df3, insurance_type = "Medicaid", verbose = TRUE)
#' print(unique_count3)  # Expected output: 2
#'
#' @importFrom dplyr filter distinct
#' @importFrom glue glue
#' @export
count_unique_physicians <- function(df, insurance_type, reason_for_exclusion = NULL, verbose = TRUE) {

  # Validate inputs
  if (!"insurance" %in% names(df)) {
    stop("The dataframe must contain a column named 'insurance'.")
  }
  if (!"reason_for_exclusions" %in% names(df)) {
    stop("The dataframe must contain a column named 'reason_for_exclusions'.")
  }
  if (!"business_days_until_appointment" %in% names(df)) {
    stop("The dataframe must contain a column named 'business_days_until_appointment'.")
  }
  if (!"phone" %in% names(df)) {
    stop("The dataframe must contain a column named 'phone'.")
  }

  # Log the initial state
  if (verbose) {
    message("Starting count_unique_physicians function...")
    message(glue("Insurance Type: {insurance_type}"))
    if (!is.null(reason_for_exclusion)) {
      message(glue("Reason for Exclusion: {reason_for_exclusion}"))
    } else {
      message("No specific reason for exclusion is provided.")
    }
  }

  # Filter data based on insurance type
  filtered_df <- df %>%
    dplyr::filter(insurance == !!insurance_type)

  if (verbose) {
    message(glue("Filtered rows by insurance type: {nrow(filtered_df)} remaining."))
  }

  # Apply the reason for exclusion filter if provided
  if (!is.null(reason_for_exclusion)) {
    filtered_df <- filtered_df %>%
      dplyr::filter(reason_for_exclusions == !!reason_for_exclusion)

    if (verbose) {
      message(glue("Filtered rows by reason for exclusion: {nrow(filtered_df)} remaining."))
    }
  }

  # Filter rows with positive business days until appointment
  filtered_df <- filtered_df %>%
    dplyr::filter(business_days_until_appointment > 0)

  if (verbose) {
    message(glue("Filtered rows with positive business days until appointment: {nrow(filtered_df)} remaining."))
  }

  # Count unique physicians based on phone number
  unique_physicians_count <- filtered_df %>%
    dplyr::distinct(phone) %>%
    dplyr::nrow()

  if (verbose) {
    message(glue("Number of unique physicians: {unique_physicians_count}"))
    message("Function count_unique_physicians completed successfully.")
  }

  return(unique_physicians_count)
}
