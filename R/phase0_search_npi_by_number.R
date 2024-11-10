#' Search and Process NPI Numbers with Logging
#'
#' This function searches for clinician data based on provided NPI numbers and writes chunked results to CSV files.
#' It supports logging of inputs, outputs, and errors.
#'
#' @param npi_data A data frame with a column 'npi' containing the NPI numbers to search.
#' @param chunk_size The number of results to save per chunk. Default is 10.
#' @param output_directory Directory to save chunked results. Default is NULL (current working directory).
#'
#' @return A combined data frame of all NPI search results.
#' @importFrom dplyr filter
#' @importFrom npi npi_flatten npi_search
#' @importFrom purrr map keep
#' @importFrom data.table rbindlist
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#' @export
phase0_search_npi_by_number <- function(npi_data,
                                        chunk_size = 10,
                                        output_directory = NULL) {

  # Set default output directory if not provided
  if (is.null(output_directory)) {
    output_directory <- getwd()
  }

  # Set logging level
  logger::log_info("Starting NPI search by number...")

  # Validate input data
  if (!is.data.frame(npi_data) || !("npi" %in% colnames(npi_data))) {
    logger::log_error("The input must be a data frame with a column named 'npi'.")
    stop("The input must be a data frame with a column named 'npi'.")
  }

  # Filter valid NPI numbers (10-digit numeric)
  npi_data <- npi_data %>%
    dplyr::filter(!is.na(npi) & nchar(as.character(npi)) == 10 & grepl("^[0-9]{10}$", as.character(npi)))

  # Function to fetch clinician data by NPI using 'number' argument
  fetch_clinician_data <- function(npi) {
    tryCatch({
      clinician_info <- npi::npi_search(number = npi)
      if (!is.null(clinician_info)) {
        npi::npi_flatten(clinician_info, cols = c("basic", "taxonomies"))
      } else {
        NULL
      }
    }, error = function(e) {
      logger::log_error(glue::glue("Error retrieving data for NPI {npi}: {e$message}"))
      NULL
    })
  }

  # Retrieve clinician data for each NPI
  clinician_data <- purrr::map(npi_data$npi, fetch_clinician_data) %>%
    purrr::keep(is.data.frame)

  # Combine and save in chunks if chunk_size specified
  combined_data <- data.table::rbindlist(clinician_data, fill = TRUE)
  if (!is.null(output_directory) && nrow(combined_data) > 0) {
    save_chunk <- function(data, prefix, directory) {
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      file_path <- file.path(directory, paste0(prefix, "_", timestamp, ".csv"))
      readr::write_csv(data, file_path)
      logger::log_info(glue::glue("Saved results to {file_path}"))
    }
    save_chunk(combined_data, "npi_results", output_directory)
  }

  logger::log_info("NPI search and processing completed successfully.")
  return(combined_data)
}
