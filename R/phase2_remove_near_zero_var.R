#' Remove Near-Zero Variance Variables from a Data Frame
#'
#' This function takes a data frame and returns a new data frame with near-zero variance variables removed.
#'
#' @param data_frame A data frame from which near-zero variance variables should be removed.
#' @param freqCut The ratio of the most common value to the second most common value. Defaults to 19.
#' @param uniqueCut The percentage of distinct values out of the number of total samples. Defaults to 10.
#'
#' @return A data frame with near-zero variance variables removed.
#'
#' @examples
#' \dontrun{
#' new_data <- remove_near_zero_var(data_frame)
#' }
#'
#' @importFrom glue glue
#' @importFrom dplyr select
#' @export
remove_near_zero_var <- function(data_frame, freqCut = 19, uniqueCut = 10) {
  # Log: Starting the function
  message("Starting the function to remove near-zero variance variables.")

  # Log: Checking if the data frame is empty
  if (nrow(data_frame) == 0 || ncol(data_frame) == 0) {
    message("The data frame is empty. Exiting function.")
    return(data_frame)
  }

  # Function to calculate the most-to-second most frequent ratio and percent unique
  calculate_metrics <- function(column) {
    freq_table <- table(column)
    freq_sorted <- sort(freq_table, decreasing = TRUE)
    freq_ratio <- if (length(freq_sorted) > 1) freq_sorted[1] / freq_sorted[2] else Inf
    percent_unique <- length(unique(column)) / length(column) * 100
    list(freq_ratio = freq_ratio, percent_unique = percent_unique)
  }

  # Apply the metrics to each column and find near-zero variance variables
  nzv_metrics <- lapply(data_frame, calculate_metrics)
  remove_cols <- names(nzv_metrics)[sapply(nzv_metrics, function(metrics) {
    metrics$freq_ratio > freqCut && metrics$percent_unique < uniqueCut
  })]

  # Log: Number of near-zero variance variables found
  message(glue::glue("Found {length(remove_cols)} near-zero variance variables."))

  # Remove near-zero variance variables if any
  if (length(remove_cols) > 0) {
    # Log: Removing near-zero variance variables
    message("Removing near-zero variance variables...")
    data_frame <- data_frame %>% dplyr::select(-dplyr::all_of(remove_cols))
  } else {
    # Log: No near-zero variance variables to remove
    message("No near-zero variance variables to remove.")
  }

  # Log: Function completed
  message("Function completed.")

  return(data_frame)
}
