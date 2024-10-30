#' Prepare the Dataset by Excluding Certain Columns with Logging
#'
#' This function prepares the dataset by excluding specified columns from the predictor variables.
#' It logs the process, including inputs and outputs.
#'
#' @param df A data frame containing the dataset.
#' @param target_variable A string representing the name of the target variable.
#' @param excluded_columns A vector of strings representing the names of columns to exclude from the predictors.
#' @return A vector of strings representing the names of predictor variables.
#' @importFrom dplyr setdiff
#' @export
prepare_dataset <- function(df, target_variable, excluded_columns) {
  cat("Preparing dataset...\n")
  cat("Target Variable:", target_variable, "\n")
  cat("Excluded Columns:", paste(excluded_columns, collapse = ", "), "\n")

  predictor_vars <- dplyr::setdiff(base::names(df), c(target_variable, excluded_columns))

  cat("Predictor Variables Identified:", paste(predictor_vars, collapse = ", "), "\n")
  return(predictor_vars)
}
