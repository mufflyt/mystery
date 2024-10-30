#' Results Section: Medicaid Summary with Logging and Error Handling
#'
#' This function generates a summary sentence for the results section based on significant predictor variables and Medicaid acceptance rates.
#' It logs the process, performs error checking, and includes default behavior for robust execution.
#'
#' @param significant_predictors A data frame containing significant predictor variables, their directions, and formatted p-values.
#' It should have columns: "Variable", "Direction", and "Formatted_P_Value".
#' @param medicaid_acceptance_rate A numeric value representing the Medicaid acceptance rate (in percentage).
#' @param accepted_medicaid_count An integer representing the count of physicians accepting Medicaid.
#' @param total_medicaid_physicians_count An integer representing the total count of physicians considered for Medicaid.
#' @return A character string representing the summary sentence.
#' @importFrom dplyr mutate pull
#' @importFrom purrr map_chr
#' @export
#'
#' @examples
#' # Example 1: Basic usage with two significant predictors
#' significant_vars <- data.frame(
#'   Variable = c("Specialty", "Region"),
#'   Direction = c("positively associated", "negatively associated"),
#'   Formatted_P_Value = c("<0.01", "0.02")
#' )
#' results_section_medicaid_summary(significant_vars, 45.6, 100, 200)
#' # Output: Physicians who accepted Medicaid were Specialty positively associated (p = <0.01) and Region negatively associated (p = 0.02). The Medicaid acceptance rate was 45.6% (n = 100/N = 200).
#'
#' # Example 2: Using larger physician counts and different acceptance rate
#' significant_vars <- data.frame(
#'   Variable = c("Experience", "Training Level"),
#'   Direction = c("positively associated", "negatively associated"),
#'   Formatted_P_Value = c("<0.05", "0.01")
#' )
#' results_section_medicaid_summary(significant_vars, 35.2, 500, 1200)
#' # Output: Physicians who accepted Medicaid were Experience positively associated (p = <0.05) and Training Level negatively associated (p = 0.01). The Medicaid acceptance rate was 35.2% (n = 500/N = 1,200).
#'
#' # Example 3: Using a single significant predictor and a high acceptance rate
#' significant_vars <- data.frame(
#'   Variable = c("Age"),
#'   Direction = c("positively associated"),
#'   Formatted_P_Value = c("<0.001")
#' )
#' results_section_medicaid_summary(significant_vars, 60.0, 750, 1250)
#' # Output: Physicians who accepted Medicaid were Age positively associated (p = <0.001). The Medicaid acceptance rate was 60.0% (n = 750/N = 1,250).
results_section_medicaid_summary <- function(significant_predictors,
                                             medicaid_acceptance_rate,
                                             accepted_medicaid_count,
                                             total_medicaid_physicians_count) {
  cat("Starting to create the summary sentence...\n")

  validate_inputs(significant_predictors, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count)

  significant_sentence <- construct_significant_sentence(significant_predictors)

  summary_sentence <- construct_summary_sentence(significant_sentence, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count)

  cat("Final summary sentence constructed:\n", summary_sentence, "\n")

  return(summary_sentence)
}

#' Validate Inputs for Medicaid Summary Sentence
#'
#' This helper function checks for required columns in significant_predictors and validates numerical inputs.
#' @noRd
#' @param significant_predictors Data frame with columns "Variable", "Direction", and "Formatted_P_Value".
#' @param medicaid_acceptance_rate Numeric, Medicaid acceptance rate (0-100).
#' @param accepted_medicaid_count Integer, count of physicians accepting Medicaid.
#' @param total_medicaid_physicians_count Integer, total count of physicians considered for Medicaid.
#' @examples
#' # Example: Validating inputs with complete and correct data
#' significant_vars <- data.frame(
#'   Variable = c("Specialty", "Experience"),
#'   Direction = c("positively associated", "negatively associated"),
#'   Formatted_P_Value = c("<0.05", "0.01")
#' )
#' validate_inputs(significant_vars, 50.0, 250, 500) # Should run without error
validate_inputs <- function(significant_predictors, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count) {
  cat("Logging inputs...\n")

  if (!is.data.frame(significant_predictors)) {
    stop("Error: 'significant_predictors' must be a data frame.")
  }

  required_columns <- c("Variable", "Direction", "Formatted_P_Value")
  missing_columns <- setdiff(required_columns, names(significant_predictors))

  if (length(missing_columns) > 0) {
    stop(paste("Error: The significant_predictors data frame is missing these required columns:", paste(missing_columns, collapse = ", ")))
  }

  if (!is.numeric(medicaid_acceptance_rate) || medicaid_acceptance_rate < 0 || medicaid_acceptance_rate > 100) {
    stop("Error: 'medicaid_acceptance_rate' must be a numeric value between 0 and 100.")
  }

  if (!is.numeric(accepted_medicaid_count) || accepted_medicaid_count < 0) {
    stop("Error: 'accepted_medicaid_count' must be a non-negative integer.")
  }

  if (!is.numeric(total_medicaid_physicians_count) || total_medicaid_physicians_count < 0) {
    stop("Error: 'total_medicaid_physicians_count' must be a non-negative integer.")
  }

  cat("Input validated successfully.\n")
}

#' Construct Sentence for Significant Predictors
#'
#' This helper function generates the sentence fragment for significant predictors with associated directions and p-values.
#' @noRd
#' @param significant_predictors Data frame with columns "Variable", "Direction", and "Formatted_P_Value".
#' @return A character string for significant predictors.
#' @importFrom dplyr mutate pull
#' @importFrom purrr map_chr
#' @examples
#' # Example: Constructing sentence for significant predictors
#' significant_vars <- data.frame(
#'   Variable = c("Region", "Specialty"),
#'   Direction = c("positively associated", "negatively associated"),
#'   Formatted_P_Value = c("0.02", "<0.01")
#' )
#' construct_significant_sentence(significant_vars)
#' # Output: "Region positively associated (p = 0.02) and Specialty negatively associated (p = <0.01)"
construct_significant_sentence <- function(significant_predictors) {
  cat("Step 1: Constructing the sentence for significant predictors...\n")

  significant_sentence <- significant_predictors %>%
    dplyr::mutate(Sentence_Part = purrr::map_chr(
      .x = seq_len(nrow(significant_predictors)),
      .f = function(i) {
        paste(significant_predictors$Variable[i], significant_predictors$Direction[i],
              "(p =", significant_predictors$Formatted_P_Value[i], ")")
      }
    )) %>%
    dplyr::pull(Sentence_Part) %>%
    paste(collapse = " and ")

  cat("Significant predictors sentence part constructed:\n", significant_sentence, "\n")

  return(significant_sentence)
}

#' Construct the Final Medicaid Summary Sentence
#'
#' This helper function creates the final summary sentence based on significant predictors, Medicaid acceptance rate, and counts.
#' @noRd
#' @param significant_sentence A character string of significant predictors.
#' @param medicaid_acceptance_rate Numeric, Medicaid acceptance rate.
#' @param accepted_medicaid_count Integer, count of physicians accepting Medicaid.
#' @param total_medicaid_physicians_count Integer, total count of physicians considered for Medicaid.
#' @return A character string representing the final summary sentence.
#' @examples
#' # Example: Constructing the final summary sentence with formatted counts
#' significant_sentence <- "Specialty positively associated (p = <0.01) and Region negatively associated (p = 0.02)"
#' construct_summary_sentence(significant_sentence, 45.6, 100, 200)
#' # Output: "Physicians who accepted Medicaid were Specialty positively associated (p = <0.01) and Region negatively associated (p = 0.02). The Medicaid acceptance rate was 45.6% (n = 100/N = 200)."
construct_summary_sentence <- function(significant_sentence, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count) {
  cat("Step 2: Constructing the final summary sentence...\n")

  summary_sentence <- sprintf(
    "Physicians who accepted Medicaid were %s. The Medicaid acceptance rate was %.1f%% (n = %s/N = %s).",
    significant_sentence,
    medicaid_acceptance_rate,
    format(accepted_medicaid_count, big.mark = ","),
    format(total_medicaid_physicians_count, big.mark = ",")
  )

  cat("Summary sentence constructed:\n", summary_sentence, "\n")

  return(summary_sentence)
}
