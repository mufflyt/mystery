#' Diagnose Mixed-Effects Model Suitability
#'
#' Comprehensively assess the suitability of a mixed-effects model by analyzing
#' group levels, observations, and providing detailed diagnostic information.
#'
#' @importFrom dplyr n_distinct summarise across where
#' @importFrom logger log_info log_error log_threshold INFO
#' @importFrom purrr map_dfr
#' @importFrom stats var median
#' @importFrom assertthat assert_that is.string noNA
#'
#' @param model_dataframe A data frame containing variables for mixed-effects model analysis.
#' @param grouping_column Name of the column representing grouping levels.
#' @param dependent_column Name of the dependent/response variable column.
#' @param significance_threshold Threshold for identifying significant predictors (default 0.2).
#' @param verbose Logical indicating whether to print detailed diagnostic information (default TRUE).
#'
#' @return A list containing:
#' - suitable: Logical indicating model suitability.
#' - diagnostic_details: Detailed information about group levels and observations.
#' - summary_statistics: Descriptive statistics of numeric columns.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with medical research dataset
#' medical_research <- data.frame(
#'   patient_id = rep(1:50, each = 3),
#'   treatment_group = sample(c("A", "B"), 150, replace = TRUE),
#'   recovery_time = rnorm(150, mean = 10, sd = 2)
#' )
#' model_diagnosis_1 <- diagnose_mixed_effects_model(
#'   model_dataframe = medical_research,
#'   grouping_column = "patient_id",
#'   dependent_column = "recovery_time"
#' )
diagnose_mixed_effects_model <- function(
    model_dataframe,
    grouping_column,
    dependent_column,
    significance_threshold = 0.2,
    verbose = TRUE
) {
  # Validate inputs using assertthat
  assertthat::assert_that(
    assertthat::is.data.frame(model_dataframe),
    msg = "Error: 'model_dataframe' must be a data frame."
  )
  assertthat::assert_that(
    grouping_column %in% names(model_dataframe),
    msg = paste("Error: 'grouping_column' not found in the data frame: ", grouping_column)
  )
  assertthat::assert_that(
    dependent_column %in% names(model_dataframe),
    msg = paste("Error: 'dependent_column' not found in the data frame: ", dependent_column)
  )
  assertthat::assert_that(
    is.numeric(model_dataframe[[dependent_column]]),
    msg = paste("Error: 'dependent_column' must be numeric: ", dependent_column)
  )
  assertthat::assert_that(
    assertthat::is.string(grouping_column),
    msg = "Error: 'grouping_column' must be a string."
  )
  assertthat::assert_that(
    assertthat::is.string(dependent_column),
    msg = "Error: 'dependent_column' must be a string."
  )
  assertthat::assert_that(
    assertthat::is.number(significance_threshold),
    msg = "Error: 'significance_threshold' must be a numeric value."
  )
  assertthat::assert_that(
    significance_threshold > 0 && significance_threshold <= 1,
    msg = "Error: 'significance_threshold' must be between 0 and 1."
  )

  # Configure logging
  if (verbose) {
    logger::log_threshold(logger::INFO)
    log_input_details(
      model_dataframe = model_dataframe,
      grouping_column = grouping_column,
      dependent_column = dependent_column
    )
  }

  # Compute group and observation metrics
  group_metrics <- compute_group_metrics(
    model_dataframe = model_dataframe,
    grouping_column = grouping_column,
    dependent_column = dependent_column
  )

  # Determine model suitability
  model_suitability <- assess_model_suitability(
    unique_group_count = group_metrics$unique_group_count,
    total_observations = group_metrics$total_observations,
    verbose = verbose
  )

  # Compute summary statistics
  summary_statistics <- compute_summary_statistics(
    model_dataframe = model_dataframe,
    verbose = verbose
  )

  # Construct and return diagnostic results
  diagnostic_results <- list(
    suitable = model_suitability$suitable,
    diagnostic_details = group_metrics,
    summary_statistics = summary_statistics,
    recommendations = model_suitability$recommendations
  )

  return(diagnostic_results)
}

#' @noRd
log_input_details <- function(model_dataframe, grouping_column, dependent_column) {
  logger::log_info("Input Parameters:")
  logger::log_info("Grouping Column: {grouping_column}")
  logger::log_info("Dependent Column: {dependent_column}")
  logger::log_info("Total Rows: {nrow(model_dataframe)}")
}

#' @noRd
compute_group_metrics <- function(model_dataframe, grouping_column, dependent_column) {
  unique_group_count <- dplyr::n_distinct(model_dataframe[[grouping_column]])
  total_observations <- nrow(model_dataframe)

  list(
    unique_group_count = unique_group_count,
    total_observations = total_observations,
    group_observation_ratio = unique_group_count / total_observations
  )
}

#' @noRd
assess_model_suitability <- function(unique_group_count, total_observations, verbose = TRUE) {
  if (unique_group_count >= total_observations) {
    if (verbose) {
      logger::log_error("Mixed-Effects Model NOT Recommended!")
      logger::log_error("Unique group levels exceed total observations")
    }

    return(list(
      suitable = FALSE,
      recommendations = c(
        "Use fixed effects regression",
        "Aggregate data to reduce group levels",
        "Choose a different grouping variable"
      )
    ))
  }

  list(suitable = TRUE, recommendations = NULL)
}

#' @noRd
compute_summary_statistics <- function(model_dataframe, verbose = TRUE) {
  numeric_columns <- dplyr::select(model_dataframe, dplyr::where(is.numeric))

  summary_stats <- numeric_columns %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE)
        )
      )
    )

  if (verbose) {
    logger::log_info("Numeric Column Summary Statistics:")
    print(summary_stats)
  }

  return(summary_stats)
}
