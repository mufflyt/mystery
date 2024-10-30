#' Search and Process NPI Numbers with Logging
#'
#' This function searches for NPI numbers based on provided first and last names, filters results by taxonomy,
#' and writes chunked results to CSV files. It supports retrying failed searches and ensures logging of inputs, outputs,
#' and errors.
#'
#' @param name_data A data frame with columns 'first' and 'last' containing the names to search.
#' @param npi_type The enumeration type for NPI search (e.g., "ind", "org", "all"). Default is "ind".
#' @param max_results_limit The maximum number of search results to request for each name pair. Default is 5.
#' @param valid_country Filter for only valid country, default is "US".
#' @param allowed_credentials A character vector containing the credentials to filter the NPI results. Default is c("MD", "DO").
#' @param searchable_taxonomies A character vector of taxonomies to filter NPI results. Default is a set of clinical taxonomies.  A searchable list can be found at the Medicare Provider and Supplier Taxonomy Crosswalk: https://data.cms.gov/provider-characteristics/medicare-provider-supplier-enrollment/medicare-provider-and-supplier-taxonomy-crosswalk/data.
#' @param chunk_size The number of results to save per chunk. Default is 10.
#' @param output_directory Directory to save chunked results. Default is NULL (current working directory).
#'
#' @return A combined data frame of all NPI search results.
#' @importFrom dplyr filter
#' @importFrom npi npi_search npi_flatten
#' @importFrom progress progress_bar
#' @importFrom purrr map2 keep
#' @importFrom data.table rbindlist
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error log_threshold
#' @importFrom glue glue
#' @export
search_and_process_npi <- function(name_data,
                                   npi_type = "ind",
                                   max_results_limit = 5L,
                                   valid_country = "US",
                                   allowed_credentials = c("MD", "DO"),
                                   searchable_taxonomies = c("Allergy & Immunology", "Anesthesiology", "Dermatology", "Family Medicine",
                                                             "Internal Medicine", "Obstetrics & Gynecology", "Pediatrics",
                                                             "Psychiatry & Neurology", "Radiology", "Surgery", "Urology"),
                                   chunk_size = 10,
                                   output_directory = NULL) {

  conflicted::conflicts_prefer(base::ceiling)
  conflicted::conflicts_prefer(base::floor)
  conflicted::conflicts_prefer(base::cummax)
  conflicted::conflicts_prefer(base::cummin)
  conflicted::conflicts_prefer(base::cumprod)
  conflicted::conflicts_prefer(base::cumsum)
  # Set default output directory if not provided
  if (is.null(output_directory)) {
    output_directory <- getwd()
  }

  # Set logging level
  logger::log_threshold(logger::INFO)
  logger::log_info("Starting NPI search function...")

  # Validate input name_data
  if (!is.data.frame(name_data)) {
    logger::log_error("The input 'name_data' must be a data frame.")
    stop("The input 'name_data' must be a data frame.")
  }
  if (!all(c("first", "last") %in% colnames(name_data))) {
    logger::log_error("The input data must contain 'first' and 'last' columns.")
    stop("The input data must contain 'first' and 'last' columns.")
  }

  # Extract first and last names from name_data
  first_names <- name_data$first
  last_names <- name_data$last
  logger::log_info(glue::glue("Input data contains {nrow(name_data)} names for processing."))

  # Function to validate and search NPI data
  search_npi <- function(first_name, last_name) {
    if (is.na(first_name) || is.na(last_name) || first_name == "" || last_name == "") {
      logger::log_error(glue::glue("Invalid name: {first_name} {last_name}"))
      return(NULL)
    }

    tryCatch({
      logger::log_info(glue::glue("Searching NPI for: {first_name} {last_name}"))
      npi_search_obj <- npi::npi_search(first_name = first_name, last_name = last_name, country_code = valid_country)
      npi_flattened <- npi::npi_flatten(npi_search_obj, cols = c("basic", "taxonomies"))

      # Filter by taxonomy and allowed credentials
      filtered_results <- dplyr::filter(npi_flattened, taxonomies_desc %in% searchable_taxonomies)
      return(filtered_results)
    }, error = function(e) {
      logger::log_error(glue::glue("Error during NPI search for {first_name} {last_name}: {e$message}"))
      return(NULL)
    })
  }

  # Retry function for failed searches
  retry_npi_search <- function(first_name, last_name, retries = 3) {
    attempt <- 1
    while (attempt <= retries) {
      result <- search_npi(first_name, last_name)
      if (!is.null(result)) {
        return(result)
      }
      logger::log_info(glue::glue("Retrying search for {first_name} {last_name} (attempt {attempt}/{retries})"))
      Sys.sleep(2 ^ attempt)  # Exponential backoff
      attempt <- attempt + 1
    }
    logger::log_error(glue::glue("Failed search for {first_name} {last_name} after {retries} attempts."))
    return(NULL)
  }

  # Initialize progress bar
  total_names <- nrow(name_data)
  progress <- progress::progress_bar$new(total = total_names)

  # Perform NPI search for each name and accumulate results
  search_results <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    progress$tick()
    retry_npi_search(first_name, last_name)
  })

  # Filter valid results (data frames only) and combine them
  valid_npi_data <- purrr::keep(search_results, is.data.frame)
  combined_npi_data <- data.table::rbindlist(valid_npi_data, fill = TRUE)

  # Save chunked results
  save_chunk <- function(npi_chunk, file_prefix, directory) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- file.path(directory, paste0(file_prefix, "_", timestamp, ".csv"))

    # Ensure the directory exists
    if (!dir.exists(directory)) {
      logger::log_info(glue::glue("Creating directory: {directory}"))
      dir.create(directory, recursive = TRUE)
    }

    readr::write_csv(npi_chunk, file_name)
    logger::log_info(glue::glue("Saved chunked results to {file_name}"))
  }

  # Save results if chunk_size is provided and output_directory is not NULL
  if (!is.null(output_directory) && nrow(combined_npi_data) > 0) {
    save_chunk(combined_npi_data, "npi_results", output_directory)
  }

  # Log completion
  logger::log_info("NPI search and processing completed successfully.")

  return(combined_npi_data)
}
