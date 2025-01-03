#' Results Section: Medicaid Summary with Logging and Error Handling
#'
#' This function generates a summary sentence for the results section
#' based on significant predictor variables
#' and Medicaid acceptance rates. It logs the process, performs error
#' checking, and includes default behavior
#' for robust execution.
#'
#' @param significant_predictors A data frame containing significant predictor
#' variables, their directions, and formatted p-values. It should have columns: "Variable", "Direction", and "Formatted_P_Value".
#' @param medicaid_acceptance_rate A numeric value representing the
#' Medicaid acceptance rate (in percentage).
#' @param accepted_medicaid_count An integer representing the count of
#' physicians accepting Medicaid.
#' @param total_medicaid_physicians_count An integer representing the
#' total count of physicians considered for Medicaid.
#' @return A character string representing the summary sentence.
#' @importFrom dplyr mutate pull
#' @importFrom purrr map_chr
#' @export
#'
#' @examples
#' # Example 2: Using larger physician counts and different acceptance rate
#' significant_vars <- data.frame(
#'   Variable = c("Experience", "Training Level"),
#'   Direction = c("positively associated", "negatively associated"),
#'   Formatted_P_Value = c("<0.05", "0.01")
#' )
#' results_section_medicaid_summary(
#'   significant_vars,
#'   medicaid_acceptance_rate = 35.2,
#'   accepted_medicaid_count = 500,
#'   total_medicaid_physicians_count = 1200
#' )
#'
#' # Example 3: Using a single significant predictor and a high acceptance rate
#' significant_vars <- data.frame(
#'   Variable = c("Age"),
#'   Direction = c("positively associated"),
#'   Formatted_P_Value = c("<0.001")
#' )
#' results_section_medicaid_summary(significant_vars, 60.0, 750, 1250)
results_section_medicaid_summary <- function(significant_predictors,
                                             medicaid_acceptance_rate,
                                             accepted_medicaid_count,
                                             total_medicaid_physicians_count) {
  cat("Starting to create the summary sentence...\n")

  # Validate inputs
  validate_inputs(significant_predictors, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count)

  # Construct significant predictors sentence
  significant_sentence <- construct_significant_sentence(significant_predictors)

  # Construct final summary sentence
  summary_sentence <- construct_summary_sentence(significant_sentence, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count)

  cat("Final summary sentence constructed:\n", summary_sentence, "\n")

  return(summary_sentence)
}

#' Validate Inputs for Medicaid Summary Sentence
#' @noRd
validate_inputs <- function(significant_predictors, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count) {
  cat("Validating inputs...\n")

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

  cat("Inputs validated successfully.\n")
}

#' Construct Sentence for Significant Predictors
#' @noRd
construct_significant_sentence <- function(significant_predictors) {
  cat("Constructing the sentence for significant predictors...\n")

  significant_sentence <- significant_predictors %>%
    dplyr::mutate(Sentence_Part = purrr::map_chr(
      .x = seq_len(nrow(significant_predictors)),
      .f = function(i) {
        paste(
          significant_predictors$Variable[i], significant_predictors$Direction[i],
          "(p =", significant_predictors$Formatted_P_Value[i], ")"
        )
      }
    )) %>%
    dplyr::pull(Sentence_Part) %>%
    paste(collapse = " and ")

  cat("Significant predictors sentence part constructed:\n", significant_sentence, "\n")

  return(significant_sentence)
}

#' Construct the Final Medicaid Summary Sentence
#' @noRd
construct_summary_sentence <- function(significant_sentence, medicaid_acceptance_rate, accepted_medicaid_count, total_medicaid_physicians_count) {
  cat("Constructing the final summary sentence...\n")

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
