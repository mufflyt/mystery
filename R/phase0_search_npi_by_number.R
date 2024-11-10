#' Search and Process NPI Numbers with Detailed Logging
#'
#' This function searches for clinician data based on a list of provided NPI numbers, processes the data,
#' and saves the search results in specified chunk sizes to CSV files. Logging occurs at every step, and
#' the function is built to work out-of-the-box with example input, default chunking, and logging to the console.
#'
#' @param npi_dataframe A data frame with a column 'npi' containing the NPI numbers to search.
#' @param records_per_chunk Integer. Specifies the number of records to save per CSV file chunk. Default is 10.
#' @param save_directory Character. Directory to save chunked results. Default is NULL, which defaults to the current working directory.
#'
#' @return A combined data frame of all NPI search results. Returns an empty data frame if no valid NPIs are found.
#' @importFrom dplyr filter
#' @importFrom npi npi_flatten npi_search
#' @importFrom purrr map keep
#' @importFrom data.table rbindlist
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default chunk size and current directory
#' npi_list <- data.frame(npi = c("1234567890", "1098765432", "1987654321"))
#' phase0_search_npi_by_number(npi_dataframe = npi_list)
#'
#' # Example 2: Save results in chunks of 5, specifying a custom directory
#' phase0_search_npi_by_number(npi_dataframe = npi_list, records_per_chunk = 5, save_directory = "custom_directory")
#'
#' # Example 3: Large dataset with default chunking
#' large_npi_list <- data.frame(npi = replicate(50, paste0(sample(0:9, 10, replace = TRUE), collapse = "")))
#' phase0_search_npi_by_number(npi_dataframe = large_npi_list)
#'
#' # Expected output: Combined data frame with clinician information, saved to CSV files by chunks.
phase0_search_npi_by_number <- function(npi_dataframe,
                                        records_per_chunk = 10,
                                        save_directory = NULL) {

  # Set default save directory if not provided
  save_directory <- ifelse(is.null(save_directory), getwd(), save_directory)

  # Log function inputs
  log_inputs(npi_dataframe, records_per_chunk, save_directory)

  # Validate and filter input NPI data
  filtered_npi_dataframe <- validate_npi_dataframe(npi_dataframe)

  # Retrieve clinician data for each valid NPI and log the transformation
  clinician_info_list <- retrieve_clinician_data(filtered_npi_dataframe$npi)

  # Combine clinician information into a single data frame and log the result
  combined_clinician_info <- combine_clinician_info(clinician_info_list)

  # Save results in chunks if chunk_size specified and save directory is valid
  if (nrow(combined_clinician_info) > 0) {
    save_clinician_info_in_chunks(combined_clinician_info, "npi_results", save_directory, records_per_chunk)
  } else {
    logger::log_info("No clinician information to save.")
  }

  # Log the function output and completion message
  logger::log_info("Function phase0_search_npi_by_number completed successfully.")
  return(combined_clinician_info)
}

# Helper functions with minimal roxygen comments

#' Log function inputs
#' @noRd
log_inputs <- function(npi_dataframe, records_per_chunk, save_directory) {
  logger::log_info(glue::glue("Function phase0_search_npi_by_number called with inputs: records_per_chunk = {records_per_chunk}, save_directory = {save_directory}"))
}

#' Validate and filter NPI data
#' @noRd
validate_npi_dataframe <- function(npi_dataframe) {
  if (!is.data.frame(npi_dataframe) || !("npi" %in% colnames(npi_dataframe))) {
    logger::log_error("The input must be a data frame with a column named 'npi'.")
    stop("The input must be a data frame with a column named 'npi'.")
  }
  filtered_npi_dataframe <- npi_dataframe %>%
    dplyr::filter(!is.na(npi) & nchar(as.character(npi)) == 10 & grepl("^[0-9]{10}$", as.character(npi)))
  logger::log_info(glue::glue("Filtered NPI data. Number of valid NPIs: {nrow(filtered_npi_dataframe)}"))
  return(filtered_npi_dataframe)
}

#' Fetch clinician data for each NPI
#' @noRd
retrieve_clinician_data <- function(npi_numbers) {
  fetch_clinician_data <- function(npi) {
    logger::log_info(glue::glue("Fetching data for NPI: {npi}"))
    tryCatch({
      clinician_info <- npi::npi_search(number = npi)
      if (!is.null(clinician_info)) {
        flattened_data <- npi::npi_flatten(clinician_info, cols = c("basic", "taxonomies"))
        logger::log_info(glue::glue("Successfully retrieved and flattened data for NPI: {npi}"))
        return(flattened_data)
      } else {
        logger::log_info(glue::glue("No data found for NPI: {npi}"))
        return(NULL)
      }
    }, error = function(e) {
      logger::log_error(glue::glue("Error retrieving data for NPI {npi}: {e$message}"))
      return(NULL)
    })
  }

  clinician_info_list <- purrr::map(npi_numbers, fetch_clinician_data) %>%
    purrr::keep(is.data.frame)
  logger::log_info(glue::glue("Data retrieved for {length(clinician_info_list)} NPIs"))
  return(clinician_info_list)
}

#' Combine clinician data into a single data frame
#' @noRd
combine_clinician_info <- function(clinician_info_list) {
  combined_clinician_info <- data.table::rbindlist(clinician_info_list, fill = TRUE)
  logger::log_info(glue::glue("Combined all clinician information. Total rows: {nrow(combined_clinician_info)}, Total columns: {ncol(combined_clinician_info)}"))
  return(combined_clinician_info)
}

#' Save clinician information in specified chunks with logging
#' @noRd
save_clinician_info_in_chunks <- function(clinician_info, prefix, directory, records_per_chunk) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  for (i in seq(1, nrow(clinician_info), by = records_per_chunk)) {
    chunk <- clinician_info[i:min(i + records_per_chunk - 1, nrow(clinician_info)), ]
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_path <- file.path(directory, paste0(prefix, "_", timestamp, "_chunk_", i, ".csv"))
    readr::write_csv(chunk, file_path)
    logger::log_info(glue::glue("Saved chunk of clinician information to {file_path}"))
  }
}
