#' Summarize the Most Common Gender, Specialty, Training, and Academic Affiliation in Provider Data
#'
#' This function calculates and returns a summary sentence describing the most common gender, specialty, training, and academic affiliation in the provided dataset.
#' It filters out missing values in each column before determining the most common value, then calculates the proportion of this most common value relative to the total non-missing values in that column.
#'
#' @param provider_info A data frame containing provider information.
#' @param gender_col Name of the column representing gender.
#' @param specialty_col Name of the column representing specialty.
#' @param training_col Name of the column representing training credentials.
#' @param academic_affiliation_col Name of the column representing academic affiliation.
#'
#' @return A character string summarizing the most common gender, specialty, training, and academic affiliation along with their respective proportions.
#'
#' @importFrom dplyr filter count mutate slice_max
#' @importFrom rlang sym
#' @examples
#' # Example usage with specified columns
#' provider_info <- data.frame(
#'   gender = c("Male", "Female", "Female", "Male", "Male"),
#'   specialty = c("Cardiology", "Cardiology", "Neurology", "Cardiology", "Neurology"),
#'   Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
#'   academic_affiliation = c("Yes", "No", "Yes", "No", "Yes")
#' )
#' summarize_common_provider_info(
#'   provider_info,
#'   gender_col = "gender",
#'   specialty_col = "specialty",
#'   training_col = "Provider.Credential.Text",
#'   academic_affiliation_col = "academic_affiliation"
#' )
#'
#' @export
results_section_summarize_common_provider_info <- function(provider_info,
                                           gender_col,
                                           specialty_col,
                                           training_col,
                                           academic_affiliation_col) {
  cat("Starting provider information summary...\n")

  # Ungroup data to ensure a fresh start
  provider_info <- dplyr::ungroup(provider_info)

  # Calculate most common values and log each step
  summaries <- list(
    gender = calculate_common_value(provider_info, gender_col),
    specialty = calculate_common_value(provider_info, specialty_col),
    training = calculate_common_value(provider_info, training_col),
    academic_affiliation = calculate_common_value(provider_info, academic_affiliation_col)
  )

  # Log calculated summaries
  for (type in names(summaries)) {
    summary_info <- summaries[[type]]
    cat(sprintf("Most common %s: %s with proportion: %.1f%%\n",
                type, summary_info$value, summary_info$proportion))
  }

  # Construct the final summary sentence
  summary_sentence <- construct_summary_sentence(
    summaries$gender,
    summaries$specialty,
    summaries$training,
    summaries$academic_affiliation
  )

  cat("Summary sentence created:\n", summary_sentence, "\n")

  return(summary_sentence)
}

#' @noRd
calculate_common_value <- function(provider_info, column) {
  cat("Processing column:", column, "\n")

  # Verify column exists in data frame and handle case errors
  if (!column %in% names(provider_info)) {
    stop(paste("Error: Column", column, "not found in provider_info."))
  }

  filtered_info <- provider_info %>%
    dplyr::filter(!is.na(!!sym(column))) %>%
    dplyr::count(!!sym(column)) %>%
    dplyr::mutate(percent = n / sum(n) * 100)

  # Find the most common value and its proportion
  most_common <- filtered_info %>%
    dplyr::slice_max(n, n = 1)

  list(value = as.character(most_common[[1]]), proportion = round(most_common$percent[1], 1))
}

#' @noRd
construct_summary_sentence <- function(gender_info, specialty_info, training_info, academic_info) {
  cat("Constructing summary sentence...\n")
  sentence <- paste0(
    "The most common gender in the dataset was ", gender_info$value,
    " (", gender_info$proportion, "%). The most common specialty was ", specialty_info$value,
    " (", specialty_info$proportion, "%). The most common training was ", training_info$value,
    " (", training_info$proportion, "%). The academic affiliation status most frequently occurring was ", academic_info$value,
    " (", academic_info$proportion, "%)."
  )
  cat("Summary sentence constructed:\n", sentence, "\n")
  return(sentence)
}
