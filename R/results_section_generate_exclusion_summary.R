#' Generate Exclusion Summary
#'
#' This function generates a summary sentence from a `janitor::tabyl` output
#' that includes counts and percentages of exclusions. It uses the `tidyverse`
#' approach for data transformations and the `logger` package for logging
#' inputs, transformations, and outputs to the console.
#'
#' @param exclusion_summary_table A dataframe created by `janitor::tabyl`,
#'        containing columns `n`, `percent`, and `reason_for_exclusions`. This
#'        dataframe summarizes exclusions and their respective counts and
#'        percentages.
#' @param verbose Logical. If TRUE, logs detailed information about the
#'        process, including input validation, filtering, and sentence
#'        construction. Defaults to TRUE.
#' @return A dynamically ordered summary sentence of exclusions as a character
#'         string.
#' @examples
#' # Example 1: Basic usage with detailed logging
#' library(janitor)
#' exclusion_summary_table <- janitor::tabyl(df_filtered$reason_for_exclusions) %>%
#'   dplyr::rename(reason_for_exclusions = `df_filtered$reason_for_exclusions`) %>%
#'   dplyr::mutate(percent = paste0(round(percent * 100, 0), "%")) %>%
#'   dplyr::arrange(dplyr::desc(n))
#'
#' exclusion_sentence <- results_section_generate_exclusion_summary(
#'   exclusion_summary_table = exclusion_summary_table,
#'   verbose = TRUE
#' )
#' print(exclusion_sentence)
#'
#' # Example 2: Suppressing verbose logging
#' exclusion_summary_table <- janitor::tabyl(df_filtered$reason_for_exclusions) %>%
#'   dplyr::rename(reason_for_exclusions = `df_filtered$reason_for_exclusions`) %>%
#'   dplyr::mutate(percent = paste0(round(percent * 100, 0), "%")) %>%
#'   dplyr::arrange(dplyr::desc(n))
#'
#' exclusion_sentence <- results_section_generate_exclusion_summary(
#'   exclusion_summary_table = exclusion_summary_table,
#'   verbose = FALSE
#' )
#' print(exclusion_sentence)
#'
#' # Example 3: Handling a small exclusion table
#' small_table <- data.frame(
#'   reason_for_exclusions = c("Reason A", "Reason B", "Reason C"),
#'   n = c(10, 5, 2),
#'   percent = c("50%", "25%", "10%")
#' )
#'
#' exclusion_sentence <- results_section_generate_exclusion_summary(
#'   exclusion_summary_table = small_table,
#'   verbose = TRUE
#' )
#' print(exclusion_sentence)
#'
#' @importFrom dplyr filter arrange mutate pull desc
#' @importFrom glue glue glue_collapse
#' @importFrom logger log_info log_error
#' @importFrom assertthat assert_that
#'
#' @export
results_section_generate_exclusion_summary <- function(exclusion_summary_table, verbose = TRUE) {
  log_start("results_section_generate_exclusion_summary", verbose)
  validate_input(exclusion_summary_table, verbose)

  filtered_exclusions <- filter_and_arrange_exclusions(exclusion_summary_table, verbose)

  exclusion_sentence <- generate_sentence(filtered_exclusions, verbose)

  log_completion("results_section_generate_exclusion_summary", exclusion_sentence, verbose)
  return(exclusion_sentence)
}

# Helper function: Log start of the main function
# @noRd
log_start <- function(func_name, verbose) {
  if (verbose) logger::log_info("Starting {func_name} function.")
}

# Helper function: Log completion of the main function
# @noRd
log_completion <- function(func_name, summary_sentence, verbose) {
  if (verbose) {
    logger::log_info("Completed {func_name} function.")
    logger::log_info("Generated exclusion summary sentence: {summary_sentence}")
  }
}

# Helper function: Validate input data
# @noRd
validate_input <- function(exclusion_summary_table, verbose) {
  if (verbose) logger::log_info("Validating input table...")
  assertthat::assert_that(is.data.frame(exclusion_summary_table),
                          msg = "Input must be a data frame.")
  required_columns <- c("n", "percent", "reason_for_exclusions")
  missing_columns <- setdiff(required_columns, colnames(exclusion_summary_table))
  if (length(missing_columns) > 0) {
    logger::log_error("Input table is missing required columns: {paste(missing_columns, collapse = ', ')}")
    stop("Input table must contain columns: ", paste(required_columns, collapse = ", "))
  }
  if (verbose) {
    logger::log_info("Input table dimensions: {nrow(exclusion_summary_table)} rows, {ncol(exclusion_summary_table)} columns.")
    logger::log_info("Input column names: {paste(colnames(exclusion_summary_table), collapse = ', ')}")
  }
}

# Helper function: Filter and arrange exclusions
# @noRd
filter_and_arrange_exclusions <- function(exclusion_summary_table, verbose) {
  if (verbose) logger::log_info("Filtering out 'Able to contact' and arranging by count...")
  exclusions_filtered <- exclusion_summary_table %>%
    dplyr::filter(reason_for_exclusions != "Able to contact") %>%
    dplyr::arrange(dplyr::desc(n))
  if (verbose) {
    logger::log_info("Filtered exclusions: {nrow(exclusions_filtered)} rows after filtering.")
  }
  return(exclusions_filtered)
}

# Helper function: Generate dynamic sentence
# @noRd
generate_sentence <- function(filtered_exclusions, verbose) {
  if (verbose) logger::log_info("Generating dynamic sentence fragments...")
  sentence_fragments <- filtered_exclusions %>%
    dplyr::mutate(fragment = glue::glue("{n} ({percent}) {tolower(reason_for_exclusions)}")) %>%
    dplyr::pull(fragment) %>%
    glue::glue_collapse(sep = ", ", last = ", and ")
  if (verbose) {
    logger::log_info("Generated sentence fragments: {sentence_fragments}")
  }
  exclusion_summary <- glue::glue("Of the excluded calls, {sentence_fragments}.")
  return(exclusion_summary)
}

