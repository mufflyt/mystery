#' Batch NPI Search Function
#'
#' This function performs a batch search for first and last names using the NPI registry API
#' and returns a flattened dataframe with relevant results. Optionally, it saves the results as a CSV file.
#' Extensive logging is provided to track each step, including function start, input validation, NPI query status,
#' data transformation, and file saving.
#'
#' @param name_data A dataframe with two columns: `first` (first names) and `last` (last names).
#'        Each row represents a unique name query.
#' @param max_results The number of results to request for each NPI query (default is 5). This is passed to
#'        limit the results from the NPI registry API.
#' @param csv_path Optional. A character string specifying the file path to save the results as a CSV file.
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
#' # Example 1: Basic batch NPI search with default max_results (5 results per query)
#' name_data <- data.frame(first = c("Tyler", "Matthew"), last = c("Muffly", "Smith"))
#' search_results <- phase0_search_batch_npi(name_data)
#' print(search_results)
#'
#' # Example 2: Batch NPI search with increased max_results
#' name_data <- data.frame(first = c("Alice", "Bob"), last = c("Johnson", "Doe"))
#' search_results <- phase0_search_batch_npi(name_data, max_results = 10)
#' print(search_results)
#'
#' # Example 3: Batch NPI search with CSV output
#' name_data <- data.frame(first = c("Sarah", "Tom", "Emily"), last = c("Brown", "White", "Black"))
#' search_results <- phase0_search_batch_npi(name_data, max_results = 5, csv_path = "npi_search_results.csv")
#' print(search_results)
#' }
#'
#' @export
phase0_search_batch_npi <- function(name_data, max_results = 5, csv_path = NULL) {
  library(dplyr)
  library(logger)
  library(readr)
  library(tibble)
  library(npi)

  # Log function start
  logger::log_info("Starting phase0_search_batch_npi function...")
  logger::log_info("Input dataframe has {nrow(name_data)} rows and {ncol(name_data)} columns.")
  logger::log_info("max_results set to: {max_results}")
  if (!is.null(csv_path)) {
    logger::log_info("CSV path specified: {csv_path}")
  }

  validate_name_data(name_data)

  query_results <- perform_npi_batch_query(name_data, max_results)

  combined_query_data <- dplyr::bind_rows(query_results)
  logger::log_info("Combined query results into one dataframe with {nrow(combined_query_data)} rows.")

  if (nrow(combined_query_data) == 0) {
    logger::log_warn("No results found for any of the input names. Returning an empty tibble.")
    return(tibble::tibble())
  }

  final_data <- flatten_and_combine_query_data(combined_query_data)

  # Save to CSV if path provided and results exist
  if (!is.null(csv_path) && nrow(final_data) > 0) {
    save_query_results(final_data, csv_path)
  }

  logger::log_info("phase0_search_batch_npi function completed successfully.")
  return(final_data)
}

#' @noRd
validate_name_data <- function(name_data) {
  if (!all(c("first", "last") %in% colnames(name_data))) {
    logger::log_error("Dataframe is missing 'first' and/or 'last' columns.")
    stop("The dataframe must have 'first' and 'last' columns.")
  }
  logger::log_info("Input dataframe validated successfully.")
}

#' @noRd
perform_npi_batch_query <- function(name_data, max_results) {
  query_list <- list()
  for (i in seq_len(nrow(name_data))) {
    first_name <- name_data$first[i]
    last_name <- name_data$last[i]
    logger::log_info("Querying NPI for: {first_name} {last_name}")

    try({
      npi_query <- npi::npi_search(first_name = first_name, last_name = last_name, limit = max_results)

      if (!is.null(npi_query)) {
        npi_query$queried_first_name <- first_name
        npi_query$queried_last_name <- last_name
        query_list[[i]] <- npi_query
        logger::log_info("Results found for: {first_name} {last_name}")
      } else {
        logger::log_info("No results found for: {first_name} {last_name}")
      }
    }, silent = TRUE)
  }
  query_list
}

#' @noRd
flatten_and_combine_query_data <- function(combined_query_data) {
  query_columns <- dplyr::select(combined_query_data, npi, queried_first_name, queried_last_name)
  logger::log_info("Extracted columns for tracking queried names.")

  flattened_query_data <- tryCatch(
    npi::npi_flatten(dplyr::select(combined_query_data, -queried_first_name, -queried_last_name)),
    error = function(e) {
      logger::log_error("Error in flattening query data: {e$message}")
      stop("Failed to flatten the query data.")
    }
  )
  logger::log_info("Flattened NPI query data.")

  final_data <- dplyr::left_join(flattened_query_data, query_columns, by = "npi")
  logger::log_info("Rejoined queried columns. Final dataframe has {nrow(final_data)} rows and {ncol(final_data)} columns.")

  final_data
}

#' @noRd
save_query_results <- function(final_data, csv_path) {
  tryCatch({
    readr::write_csv(final_data, csv_path)
    logger::log_info("Data saved to file: {csv_path}")
  }, error = function(e) {
    logger::log_error("Error saving file to {csv_path}: {e$message}")
    stop("Failed to save the output CSV.")
  })
}

