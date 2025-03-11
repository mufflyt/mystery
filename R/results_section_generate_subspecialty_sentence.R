#' Results Section Generate Subspecialty Sentence
#'
#' This function generates a descriptive sentence summarizing statistics for
#' physician specialties based on their subspecialties, successful appointments,
#' and other criteria.
#'
#' @param subspecialty_dataset A data frame containing physician data.
#' @param subspecialty_col A string specifying the column name for the
#'   physician subspecialties.
#' @param calls_vs_individual_doctors A string indicating whether to analyze
#'   "counts" (total calls) or "doctors" (unique physicians).
#'   Default is "counts".
#' @param physician_npi_col A string specifying the column name for physician
#'   NPI numbers. Only used when \code{calls_vs_individual_doctors = "doctors"}.
#' @param physician_firstname_col A string specifying the column name for
#'   physician first names. Used for deduplication when NPI is not available.
#' @param physician_lastname_col A string specifying the column name for
#'   physician last names. Used for deduplication when NPI is not available.
#' @param physician_state_col A string specifying the column name for physician
#'   states. Used for deduplication when NPI is not available.
#' @param successful_appointment A logical indicating whether to filter results
#'   for successful appointments where \code{business_days_until_appointment > 0}.
#'   Default is \code{TRUE}.
#' @param compare_counts A logical indicating whether to compare call counts
#'   with individual doctor counts. Default is \code{TRUE}.
#' @param additional_success_appointment_variable A logical indicating whether
#'   to include additional criteria for successful appointments. Default is
#'   \code{TRUE}.
#' @param verbose A logical indicating whether to output detailed logging
#'   messages. Default is \code{FALSE}.
#'
#' @return A character string containing a descriptive summary of the analysis.
#'
#' @importFrom dplyr group_by summarise ungroup mutate pull n filter distinct
#' @importFrom glue glue glue_collapse
#' @importFrom logger log_info
#' @importFrom assertthat assert_that
#'
#' @examples
#' # Example 1: Analyze calls with successful appointments
#' df <- data.frame(
#'   Subspecialty = c(rep("General Dermatology", 10),
#'                    rep("Pediatric Dermatology", 8)),
#'   NPI = c(1:10, 11:18),
#'   first = c(rep("John", 10), rep("Jane", 8)),
#'   last = c(rep("Doe", 10), rep("Smith", 8)),
#'   state = c(rep("NY", 10), rep("CA", 8)),
#'   business_days_until_appointment = c(5, 0, 10, 15, 0, 7, 12, 0, 10, 5,
#'                                       5, 5, 10, 7, 10, 5, 15, 12),
#'   reason_for_exclusions = c(rep("Able to contact", 16),
#'                             rep("Not able to contact", 2)),
#'   will_this_physician_see_children_if_they_ask_about_insurance_say_they_have_blue_cross_blue_shield =
#'     c(rep("Yes", 12), rep("No", 6))
#' )
#'
#' specialty_analysis <- results_section_generate_subspecialty_sentence(
#'   subspecialty_dataset = df,
#'   subspecialty_col = "Subspecialty",
#'   calls_vs_individual_doctors = "counts",
#'   successful_appointment = TRUE,
#'   verbose = TRUE
#' )
#' print(specialty_analysis)
#'
#' # Example 2: Analyze unique doctors with additional criteria
#' specialty_analysis <- results_section_generate_subspecialty_sentence(
#'   subspecialty_dataset = df,
#'   subspecialty_col = "Subspecialty",
#'   calls_vs_individual_doctors = "doctors",
#'   physician_npi_col = "NPI",
#'   successful_appointment = TRUE,
#'   additional_success_appointment_variable = TRUE,
#'   verbose = TRUE
#' )
#' print(specialty_analysis)
#'
#' # Example 3: Analyze all appointments without filtering
#' specialty_analysis <- results_section_generate_subspecialty_sentence(
#'   subspecialty_dataset = df,
#'   subspecialty_col = "Subspecialty",
#'   calls_vs_individual_doctors = "counts",
#'   successful_appointment = FALSE,
#'   compare_counts = FALSE,
#'   verbose = FALSE
#' )
#' print(specialty_analysis)
#' @export
#'
#' @noRd
validate_subspecialty_inputs <- function(subspecialty_dataset, subspecialty_col,
                                         calls_vs_individual_doctors, verbose) {
  if(verbose) logger::log_info("Validating input parameters")

  assertthat::assert_that(is.data.frame(subspecialty_dataset),
                          msg = "Input must be a data frame")
  assertthat::assert_that(nrow(subspecialty_dataset) > 0,
                          msg = "Input data frame is empty")
  assertthat::assert_that(subspecialty_col %in% colnames(subspecialty_dataset),
                          msg = glue::glue("Column '{subspecialty_col}' not found"))
  assertthat::assert_that(calls_vs_individual_doctors %in% c("calls", "doctors"),
                          msg = "calls_vs_individual_doctors must be 'calls' or 'doctors'")

  if(verbose) logger::log_info("Input validation successful")
}

#' @noRd
filter_successful_subspecialty_appointments <- function(subspecialty_dataset, verbose) {
  if(verbose) logger::log_info("Filtering for successful appointments")

  appointment_cols <- c("business_days_until_appointment", "reason_for_exclusions")
  assertthat::assert_that(all(appointment_cols %in% colnames(subspecialty_dataset)),
                          msg = "Missing required columns for appointment filtering")

  successful_appointments <- subspecialty_dataset %>%
    dplyr::filter(business_days_until_appointment > 0,
                  reason_for_exclusions == "Able to contact")

  if(verbose) {
    logger::log_info(glue::glue("Filtered to {nrow(successful_appointments)} successful appointments"))
  }

  assertthat::assert_that(nrow(successful_appointments) > 0,
                          msg = "No successful appointments found after filtering")

  successful_appointments
}

#' @noRd
filter_pediatric_acceptance <- function(subspecialty_dataset, verbose) {
  if(verbose) logger::log_info("Filtering for pediatric acceptance")

  pediatric_col <- "will_this_physician_see_children_if_they_ask_about_insurance_say_they_have_blue_cross_blue_shield"
  assertthat::assert_that(pediatric_col %in% colnames(subspecialty_dataset),
                          msg = "Missing pediatric acceptance column")

  pediatric_accepted <- subspecialty_dataset %>%
    dplyr::filter(.data[[pediatric_col]] == "Yes")

  if(verbose) {
    logger::log_info(glue::glue("Filtered to {nrow(pediatric_accepted)} records with pediatric acceptance"))
  }

  assertthat::assert_that(nrow(pediatric_accepted) > 0,
                          msg = "No physicians accepting pediatric patients found after filtering")

  pediatric_accepted
}

#' @noRd
deduplicate_physician_records <- function(subspecialty_dataset, physician_npi_col,
                                          physician_firstname_col, physician_lastname_col,
                                          physician_state_col, verbose) {
  if(verbose) logger::log_info("Deduplicating physician records")

  unique_physicians <- if(physician_npi_col %in% colnames(subspecialty_dataset)) {
    if(verbose) logger::log_debug("Deduplicating using NPI")
    dplyr::distinct(subspecialty_dataset, !!sym(physician_npi_col), .keep_all = TRUE)
  } else {
    required_cols <- c(physician_firstname_col, physician_lastname_col, physician_state_col)
    if(all(required_cols %in% colnames(subspecialty_dataset))) {
      if(verbose) logger::log_debug("Deduplicating using name/state combination")
      dplyr::distinct(subspecialty_dataset,
                      !!sym(physician_firstname_col),
                      !!sym(physician_lastname_col),
                      !!sym(physician_state_col),
                      .keep_all = TRUE)
    } else {
      logger::log_warn("No deduplication possible - using all records")
      subspecialty_dataset
    }
  }

  assertthat::assert_that(nrow(unique_physicians) > 0,
                          msg = "No physicians remain after deduplication")

  unique_physicians
}

# Main function
results_section_generate_subspecialty_sentence <- function(subspecialty_dataset,
                                                           subspecialty_col = "Subspecialty",
                                                           calls_vs_individual_doctors = "counts",
                                                           physician_npi_col = "NPI",
                                                           physician_firstname_col = "first",
                                                           physician_lastname_col = "last",
                                                           physician_state_col = "state",
                                                           successful_appointment = TRUE,
                                                           compare_counts = TRUE,
                                                           additional_success_appointment_variable = TRUE,
                                                           verbose = FALSE) {

  # Initial validation and deduplication for physician counts
  validate_subspecialty_inputs(subspecialty_dataset, subspecialty_col,
                               calls_vs_individual_doctors, verbose)

  base_physicians <- if(calls_vs_individual_doctors == "doctors") {
    deduplicate_physician_records(subspecialty_dataset, physician_npi_col,
                                  physician_firstname_col, physician_lastname_col,
                                  physician_state_col, verbose)
  } else {
    subspecialty_dataset
  }

  # Calculate statistics for each filter step by subspecialty
  stats_by_subspecialty <- base_physicians %>%
    dplyr::group_by(.data[[subspecialty_col]]) %>%
    dplyr::summarise(
      total_n = n(),

      # Business days > 0
      business_days_n = sum(business_days_until_appointment > 0, na.rm = TRUE),
      business_days_pct = round(business_days_n / total_n * 100, 1),

      # Able to contact
      contact_n = sum(reason_for_exclusions == "Able to contact", na.rm = TRUE),
      contact_pct = round(contact_n / total_n * 100, 1),

      # Accepts pediatric patients
      pediatric_n = sum(will_this_physician_see_children_if_they_ask_about_insurance_say_they_have_blue_cross_blue_shield == "Yes",
                        na.rm = TRUE),
      pediatric_pct = round(pediatric_n / total_n * 100, 1),

      # All criteria combined
      final_n = sum(business_days_until_appointment > 0 &
                      reason_for_exclusions == "Able to contact" &
                      will_this_physician_see_children_if_they_ask_about_insurance_say_they_have_blue_cross_blue_shield == "Yes",
                    na.rm = TRUE),
      final_pct = round(final_n / total_n * 100, 1)
    ) %>%
    ungroup()

  # Generate detailed sentence
  detailed_stats <- stats_by_subspecialty %>%
    dplyr::mutate(
      text_fragment = glue::glue(
        "{total_n} total {.data[[subspecialty_col]]} PHYSICIANS: ",
        "{business_days_n}/{total_n} ({business_days_pct}%) had business days >0, ",
        "{contact_n}/{total_n} ({contact_pct}%) were able to be contacted, ",
        "{pediatric_n}/{total_n} ({pediatric_pct}%) accepted pediatric patients, and ",
        "{final_n}/{total_n} ({final_pct}%) met all criteria"
      )
    ) %>%
    dplyr::pull(text_fragment) %>%
    glue::glue_collapse(sep = ". ")

  descriptive_sentence <- glue::glue("The study included {detailed_stats}.")

  if(verbose) {
    logger::log_info("Analysis complete")
    logger::log_info(glue::glue("Generated sentence: {descriptive_sentence}"))
  }

  return(descriptive_sentence)
}
