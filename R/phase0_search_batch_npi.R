#' Batch NPI Search Function
#'
#' This function performs a batch search for names using the NPI registry API
#' and returns a flattened dataframe with relevant results. Optionally, it
#' saves the results as a CSV file. Extensive logging is provided to track
#' each step, including function start, input validation, NPI query status,
#' data transformation, and file saving.
#'
#' For additional details on the NPPES API and its data dictionary, see:
#' \url{https://www.cms.gov/regulations-and-guidance/administrative-simplification/nationalprovidentstand/downloads/data_dissemination_file-code_values.pdf}.
#'
#' @param df A data frame with columns `first` and `last`, representing the
#'   first and last names to query in the NPI registry.
#' @param limit Number of results per query. Defaults to 5.
#' @param write_csv_path File path for saving the results as a CSV. Default is NULL.
#'
#' @return A dataframe containing flattened NPI results, including columns for
#' the queried `first` and `last` names. If no results are found, an empty
#' tibble is returned.
#'
#' @importFrom dplyr bind_rows select left_join
#' @importFrom logger log_info log_error log_warn
#' @importFrom readr write_csv
#' @importFrom tibble tibble
#' @importFrom npi npi_search npi_flatten
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic batch NPI search with default limit
#' name_df <- data.frame(
#'   first = c("Tyler", "Matthew"),
#'   last = c("Muffly", "Smith"),
#'   stringsAsFactors = FALSE
#' )
#' results <- phase0_search_batch_npi(df = name_df)
#' print(results)
#'
#' # Example 2: Batch NPI search with increased limit
#' results <- phase0_search_batch_npi(df = name_df, limit = 10)
#' print(results)
#'
#' # Example 3: Batch NPI search with CSV output
#' results <- phase0_search_batch_npi(
#'   df = name_df,
#'   limit = 5,
#'   write_csv_path = "npi_search_results.csv"
#' )
#' }
#'
#' @export
phase0_search_batch_npi <- function(df,
                                    limit = 5,
                                    write_csv_path = NULL) {
  # Log function start and inputs
  logger::log_info("Starting phase0_search_batch_npi function...")
  logger::log_info("Input dataframe has {nrow(df)} rows and {ncol(df)} columns.")
  logger::log_info("Using NPPES API Data Dictionary: https://www.cms.gov/regulations-and-guidance/administrative-simplification/nationalprovidentstand/downloads/data_dissemination_file-code_values.pdf.")
  logger::log_info("Limit set to: {limit}")
  if (!is.null(write_csv_path)) {
    logger::log_info("Output CSV path specified: {write_csv_path}")
  }

  # Validate input dataframe
  validate_input(df)

  # Perform NPI searches
  results_list <- perform_npi_search(df, limit)

  # Combine results
  combined_results <- dplyr::bind_rows(results_list)
  logger::log_info("Combined results into one dataframe with {nrow(combined_results)} rows.")
  logger::log_info("For detailed data code definitions, refer to: https://www.cms.gov/regulations-and-guidance/administrative-simplification/nationalprovidentstand/downloads/data_dissemination_file-code_values.pdf.")

  if (nrow(combined_results) == 0) {
    logger::log_warn("No results found for any of the input names. Returning an empty tibble.")
    return(tibble::tibble())
  }

  # Flatten and process results
  final_results <- process_and_flatten_results(combined_results)

  # Save results to CSV if specified
  if (!is.null(write_csv_path) && nrow(final_results) > 0) {
    save_results(final_results, write_csv_path)
  }

  logger::log_info("phase0_search_batch_npi function completed successfully.")
  return(final_results)
}
