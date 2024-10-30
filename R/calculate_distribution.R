#' Calculate Demographic Distribution with Logging
#'
#' This function calculates the distribution of a categorical variable within a data frame,
#' including counts and percentages. It also logs inputs, outputs, and all transformations
#' for transparency and debugging purposes.
#'
#' @param df A data frame containing the data.
#' @param column A string representing the name of the column for which the distribution is calculated.
#' @return A data frame with the count, total, and percentage for each level of the specified column.
#' @importFrom dplyr filter group_by summarise mutate arrange slice
#' @importFrom rlang sym
#' @importFrom utils write.csv
#' @export
#' @examples
#' df <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female", NA))
#' result <- calculate_distribution(df, "gender")
#' print(result)
# Improved calculate_distribution function with robust logging
calculate_distribution <- function(df, column) {
  cat("Starting calculate_distribution...\n")
  cat("Input Data Frame:\n")
  print(head(df))  # Logging the first few rows of input data for debugging

  cat("Column to Calculate Distribution For:", column, "\n")

  if (!column %in% names(df)) {
    stop(paste("Column", column, "not found in the dataframe."))
  }

  df_filtered <- df %>%
    dplyr::filter(!is.na(.data[[column]]))

  cat("Filtered Data Frame (NA removed):\n")
  print(head(df_filtered))  # Logging the first few rows of filtered data for debugging

  result <- df_filtered %>%
    dplyr::group_by(.data[[column]]) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(
      total = sum(count),
      percent = (count / total) * 100
    ) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::slice(1)

  cat("Final Distribution Result:\n")
  print(result)  # Logging the final result

  return(result)
}


# df <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female", NA))
# result <- calculate_distribution(df, "gender")
# print(result)
