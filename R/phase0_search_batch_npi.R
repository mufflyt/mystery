#' Batch NPI Search Function
#'
#' This function performs a batch search for first and last names using the NPI registry API
#' and returns a flattened dataframe with relevant results. Optionally, it saves the results as a CSV file.
#'
#' @param df A dataframe with two columns: `first` (first names) and `last` (last names).
#' @param limit The number of results to request for each query (default is 5).
#' @param write_csv_path A character string specifying the file path to save the results as a CSV file. If NULL, no CSV is saved. (default is NULL).
#'
#' @return A flattened dataframe with NPI results, including custom columns tracking the
#'         first and last names that were searched, with an option to save the results as a CSV file.
#' @importFrom dplyr bind_rows select left_join
#' @importFrom npi npi_search npi_flatten
#' @importFrom readr write_csv
#' @export
#' @examples
#' # Example 1: Basic batch NPI search with two names
#' df <- data.frame(
#'   first = c("Tyler", "Matthew"),
#'   last = c("Muffly", "Muffly")
#' )
#' npi_results <- search_batch_npi(df, limit = 5)
#'
#' # Example 2: Batch NPI search with different name combinations
#' df <- data.frame(
#'   first = c("John", "Jane"),
#'   last = c("Smith", "Doe")
#' )
#' npi_results <- search_batch_npi(df, limit = 10)
#'
#' # Example 3: Batch NPI search with more names and a CSV output
#' df <- data.frame(
#'   first = c("Anna", "Tom", "Emily"),
#'   last = c("Brown", "White", "Black")
#' )
#' npi_results <- search_batch_npi(df, limit = 15, write_csv_path = "npi_results.csv")
phase0_search_batch_npi <- function(df, limit = 5, write_csv_path = NULL) {
  # Ensure the dataframe has 'first' and 'last' columns
  if (!all(c("first", "last") %in% colnames(df))) {
    stop("The dataframe must have 'first' and 'last' columns.")
  }

  # Initialize an empty list to store results
  results_list <- list()

  # Loop through each row in the dataframe
  for (i in seq_len(nrow(df))) {
    first_name <- df$first[i]
    last_name <- df$last[i]

    # Log progress
    message("Querying NPI for: ", first_name, " ", last_name)

    # Query the NPI registry (limit set to control the number of results returned)
    try({
      results <- npi::npi_search(first_name = first_name, last_name = last_name, limit = limit)

      # Check if any results were found
      if (!is.null(results)) {
        # Add a new column to track the first and last name that produced the result
        results$first_name_searched <- first_name
        results$last_name_searched <- last_name

        # Append the results to the results_list
        results_list[[i]] <- results
      } else {
        # No results found, log it
        message("No results found for: ", first_name, " ", last_name)
      }
    }, silent = TRUE)
  }

  # Combine all results into one dataframe
  combined_results <- dplyr::bind_rows(results_list)

  # Separate the added columns to avoid issues with flattening
  custom_columns <- dplyr::select(combined_results, npi, first_name_searched, last_name_searched)

  # Flatten the combined results without the custom columns
  flattened_results <- npi::npi_flatten(dplyr::select(combined_results, -first_name_searched, -last_name_searched))

  # Rejoin the custom columns to the flattened results
  final_results <- dplyr::left_join(flattened_results, custom_columns, by = "npi")

  # Save the results to a CSV file if write_csv_path is provided
  if (!is.null(write_csv_path) && nrow(final_results) > 0) {
    readr::write_csv(final_results, write_csv_path)
    message("Data saved to file: ", write_csv_path)
  }

  # Return the final flattened dataframe to the user
  return(final_results)
}
