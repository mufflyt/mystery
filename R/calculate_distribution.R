#' Calculate Demographic Distribution with Robust Logging
#'
#' This function calculates the distribution of a categorical variable within a data frame,
#' including counts and percentages. It also logs inputs, outputs, and all transformations
#' for transparency and debugging purposes.
#'
#' @param df A data frame containing the data.
#' @param column A string representing the name of the column for which the distribution is calculated.
#' @return A data frame with the count, total, and percentage for each level of the specified column.
#' @importFrom dplyr filter group_by summarise mutate arrange slice
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom rlang sym
#' @examples
#' df <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female", NA))
#' result <- calculate_distribution(df, "gender")
#' print(result)
#' @export
calculate_distribution <- function(df, column) {
  # Input validation using assertthat
  assertthat::assert_that(is.data.frame(df),
    msg = "The `df` argument must be a data frame."
  )
  assertthat::assert_that(assertthat::is.string(column),
    msg = "The `column` argument must be a single string."
  )
  assertthat::assert_that(assertthat::has_name(df, column),
    msg = paste("The specified column", column, "is not found in the data frame.")
  )
  assertthat::assert_that(nrow(df) > 0,
    msg = "The data frame `df` must not be empty."
  )

  cat("Starting calculate_distribution...\n")
  cat("Input Data Frame:\n")
  print(head(df)) # Logging the first few rows of input data for debugging

  cat("Column to Calculate Distribution For:", column, "\n")

  # Filter out rows with NA in the specified column
  df_filtered <- df %>%
    dplyr::filter(!is.na(.data[[column]]))

  assertthat::assert_that(nrow(df_filtered) > 0,
    msg = paste("The specified column", column, "contains only NA values.")
  )

  cat("Filtered Data Frame (NA removed):\n")
  print(head(df_filtered)) # Logging the first few rows of filtered data for debugging

  # Calculate the distribution
  result <- df_filtered %>%
    dplyr::group_by(.data[[column]]) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(
      total = sum(count),
      percent = (count / total) * 100
    ) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::slice(1)

  assertthat::assert_that(all(is.numeric(result$count)),
    msg = "The `count` column must contain numeric values."
  )
  assertthat::assert_that(all(is.numeric(result$percent)),
    msg = "The `percent` column must contain numeric values."
  )

  cat("Final Distribution Result:\n")
  print(result) # Logging the final result

  return(result)
}
