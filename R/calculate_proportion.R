#' Calculate the Proportion of Each Level in a Categorical Variable with Logging
#'
#' This function calculates the proportion of each level in a specified categorical variable within a data frame.
#' It returns a data frame with the counts and percentages of each level, while logging the process.
#'
#' @param df A data frame containing the categorical variable.
#' @param variable_name The name of the categorical variable for which proportions are calculated, passed as an unquoted expression.
#' @param log_file The path to the log file where logs will be saved.
#'
#' @return A data frame with two columns: `n` (the count of each level) and `percent` (the percentage of the total count represented by each level).
#'
#' @details The function counts the occurrences of each unique value in the specified variable and calculates the percentage each value represents of the total count.
#' The percentages are rounded to two decimal places.
#'
#' @import dplyr
#' @import logger
#' @import assertthat
#' @export
calculate_proportion <- function(df, variable_name, log_file = "calculate_proportion.log") {
  # Set up logger to log to a file
  logger::log_appender(logger::appender_file(log_file))
  logger::log_info("Function calculate_proportion started.")

  # Validate inputs using assertthat
  assertthat::assert_that(
    is.data.frame(df),
    msg = "Error: 'df' must be a data frame."
  )
  assertthat::assert_that(
    is.character(deparse(substitute(variable_name))),
    msg = "Error: 'variable_name' must be a valid column name as an unquoted expression."
  )
  assertthat::assert_that(
    deparse(substitute(variable_name)) %in% names(df),
    msg = paste("Error: Column", deparse(substitute(variable_name)), "not found in the data frame.")
  )
  assertthat::assert_that(
    is.character(log_file) && nchar(log_file) > 0,
    msg = "Error: 'log_file' must be a non-empty string."
  )

  # Log the input data frame summary
  logger::log_info("Input data frame summary:")
  logger::log_info("Number of rows: {nrow(df)}")
  logger::log_info("Number of columns: {ncol(df)}")
  logger::log_info("Variable name: {deparse(substitute(variable_name))}")

  # Remove grouping if present
  df <- df %>% dplyr::ungroup()

  # Count occurrences of each unique value and calculate proportions
  logger::log_info("Counting occurrences of each level in the variable.")
  tabyl_result <- df %>%
    dplyr::count({{ variable_name }}, name = "n") %>%
    dplyr::mutate(percent = n / sum(n) * 100)

  logger::log_info("Counts and percentages calculated. Preview of results:")
  print(head(tabyl_result)) # Log the actual results

  # Round percentages to two decimal places
  logger::log_info("Rounding percentages to two decimal places.")
  tabyl_result <- tabyl_result %>%
    dplyr::mutate(across(where(is.numeric), round, 2))

  logger::log_info("Final result prepared.")
  print(head(tabyl_result)) # Log the final output

  return(tabyl_result)
}
