#' Calculate Descriptive Statistics with Robust Logging
#'
#' This function calculates the median, 25th percentile (Q1), and 75th percentile (Q3) for a specified column in a dataframe. The function includes detailed logging of inputs, outputs, and each data transformation.
#'
#' @param df A dataframe containing the data.
#' @param column A string representing the column name for which to calculate the descriptive statistics.
#' @param verbose A boolean indicating whether to print detailed logs. Default is TRUE.
#'
#' @return A list containing the median, 25th percentile (Q1), and 75th percentile (Q3) of the specified column.
#' @importFrom dplyr pull
#' @importFrom stats median quantile
#' @importFrom assertthat assert_that is.string has_name
#' @examples
#' # Example: Calculate descriptive statistics for a column with logging
#' stats <- calculate_descriptive_stats(df, "business_days_until_appointment", verbose = TRUE)
#'
#' @export
calculate_descriptive_stats <- function(df, column, verbose = TRUE) {
  # Validate inputs using assertthat
  assertthat::assert_that(is.data.frame(df),
    msg = "The `df` argument must be a data frame."
  )
  assertthat::assert_that(assertthat::is.string(column),
    msg = "The `column` argument must be a single string."
  )
  assertthat::assert_that(assertthat::has_name(df, column),
    msg = paste("The specified column", column, "is not found in the data frame.")
  )
  assertthat::assert_that(is.logical(verbose),
    msg = "The `verbose` argument must be a logical value (TRUE or FALSE)."
  )

  # Log the inputs to the function
  if (verbose) {
    cat("Function calculate_descriptive_stats called with the following inputs:\n")
    cat("  Column:", column, "\n")
    cat("  Dataframe has", nrow(df), "rows and", ncol(df), "columns\n")
  }

  # Calculate the median
  median_val <- round(stats::median(dplyr::pull(df, !!column), na.rm = TRUE), 2)
  if (verbose) {
    cat("  Median calculated:", median_val, "\n")
  }

  # Calculate the 25th percentile (Q1)
  q25 <- stats::quantile(dplyr::pull(df, !!column), probs = 0.25, na.rm = TRUE)
  if (verbose) {
    cat("  25th percentile (Q1) calculated:", q25, "\n")
  }

  # Calculate the 75th percentile (Q3)
  q75 <- stats::quantile(dplyr::pull(df, !!column), probs = 0.75, na.rm = TRUE)
  if (verbose) {
    cat("  75th percentile (Q3) calculated:", q75, "\n")
  }

  # Prepare the output list
  result <- list(median = median_val, q25 = q25, q75 = q75)

  # Log the output
  if (verbose) {
    cat("  Final output:\n")
    print(result)
  }

  return(result)
}
