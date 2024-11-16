#' Batch NPI Search Function
#'
#' This function performs a batch search for first and last names using the NPI registry API
#' and returns a flattened dataframe with relevant results. Optionally, it saves the results as a CSV file.
#' Extensive logging is provided to track each step, including function start, input validation, NPI query status,
#' data transformation, and file saving.
#'
#' @param df A dataframe with two columns: `first` (first names) and `last` (last names).
#'        Each row represents a unique name query.
#' @param limit The number of results to request for each NPI query (default is 5). This is passed to
#'        limit the results from the NPI registry API.
#' @param write_csv_path Optional. A character string specifying the file path to save the results as a CSV file.
#'        If NULL, no CSV file is saved (default is NULL).
#'
#' @return A dataframe containing flattened NPI results, including columns for the queried `first` and `last` names.
#'         If no results are found, an empty tibble is returned.
#'
#' @importFrom dplyr bind_rows select left_join
#' @importFrom logger log_info log_error log_warn
#' @importFrom readr write_csv
#' @importFrom tibble tibble
#' @importFrom npi npi_search npi_flatten
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic batch NPI search with default limit (5 results per query)
#' # Create a dataframe with names to search
#' name_df <- data.frame(
#'   first = c("Tyler", "Matthew"),
#'   last = c("Muffly", "Smith"),
#'   stringsAsFactors = FALSE
#' )
#' # Perform the batch search
#' results <- phase0_search_batch_npi(name_df)
#' print(results)  # Show results in the console
#'
#' # Example 2: Batch NPI search with increased limit (10 results per query)
#' # Search for more possible NPI records per query
#' name_df <- data.frame(
#'   first = c("Alice", "Bob"),
#'   last = c("Johnson", "Doe"),
#'   stringsAsFactors = FALSE
#' )
#' results <- phase0_search_batch_npi(name_df, limit = 10)
#' print(results)  # Show more detailed results
#'
#' # Example 3: Batch NPI search with CSV output
#' # Save the search results to a specified CSV file
#' name_df <- data.frame(
#'   first = c("Sarah", "Tom", "Emily"),
#'   last = c("Brown", "White", "Black"),
#'   stringsAsFactors = FALSE
#' )
#' results <- phase0_search_batch_npi(name_df, limit = 5, write_csv_path = "npi_search_results.csv")
#' print(results)  # Display the output, which is also saved to the specified CSV
#' }
#'
#' @export
phase0_search_batch_npi <- function(df, limit = 5, write_csv_path = NULL) {

  # Main logging for function start
  logger::log_info("Starting phase0_search_batch_npi function...")
  logger::log_info("Input dataframe has {nrow(df)} rows and {ncol(df)} columns.")
  logger::log_info("Limit set to: {limit}")
  if (!is.null(write_csv_path)) {
    logger::log_info("Output CSV path specified: {write_csv_path}")
  }

  validate_input(df)

  results_list <- perform_npi_search(df, limit)

  combined_results <- dplyr::bind_rows(results_list)
  logger::log_info("Combined results into one dataframe with {nrow(combined_results)} rows.")

  if (nrow(combined_results) == 0) {
    logger::log_warn("No results found for any of the input names. Returning an empty tibble.")
    return(tibble::tibble())
  }

  final_results <- process_and_flatten_results(combined_results)

  # Save to CSV if path provided and results exist
  if (!is.null(write_csv_path) && nrow(final_results) > 0) {
    save_results(final_results, write_csv_path)
  }

  logger::log_info("phase0_search_batch_npi function completed successfully.")
  return(final_results)
}

#' @noRd
validate_input <- function(df) {
  if (!all(c("first", "last") %in% colnames(df))) {
    logger::log_error("Dataframe is missing 'first' and/or 'last' columns.")
    stop("The dataframe must have 'first' and 'last' columns.")
  }
  logger::log_info("Input dataframe validated successfully.")
}

#' @noRd
perform_npi_search <- function(df, limit) {
  results_list <- list()
  for (i in seq_len(nrow(df))) {
    first_name <- df$first[i]
    last_name <- df$last[i]
    logger::log_info("Querying NPI for: {first_name} {last_name}")

    try({
      results <- npi::npi_search(first_name = first_name, last_name = last_name, limit = limit)

      if (!is.null(results)) {
        results$first_name_searched <- first_name
        results$last_name_searched <- last_name
        results_list[[i]] <- results
        logger::log_info("Results found for: {first_name} {last_name}")
      } else {
        logger::log_info("No results found for: {first_name} {last_name}")
      }
    }, silent = TRUE)
  }
  results_list
}

#' @noRd
process_and_flatten_results <- function(combined_results) {
  custom_columns <- dplyr::select(combined_results, npi, first_name_searched, last_name_searched)
  logger::log_info("Extracted custom columns for tracking searched names.")

  flattened_results <- tryCatch(
    npi::npi_flatten(dplyr::select(combined_results, -first_name_searched, -last_name_searched)),
    error = function(e) {
      logger::log_error("Error in flattening results: {e$message}")
      stop("Failed to flatten the results.")
    }
  )
  logger::log_info("Flattened NPI search results.")

  final_results <- dplyr::left_join(flattened_results, custom_columns, by = "npi")
  logger::log_info("Rejoined custom columns. Final dataframe has {nrow(final_results)} rows and {ncol(final_results)} columns.")

  final_results
}

#' @noRd
save_results <- function(final_results, write_csv_path) {
  tryCatch({
    readr::write_csv(final_results, write_csv_path)
    logger::log_info("Data saved to file: {write_csv_path}")
  }, error = function(e) {
    logger::log_error("Error saving file to {write_csv_path}: {e$message}")
    stop("Failed to save the output CSV.")
  })
}
