#' Geocode Unique Addresses with Detailed Logging
#'
#' This function geocodes unique addresses in a dataset using the Google Maps API,
#' appending latitude and longitude information to each unique address. It provides
#' detailed logging for each step, including validations, transformations, geocoding
#' operations, and saving results.
#'
#' @param address_table A data frame containing address-related columns to be united
#' for geocoding. The data frame must include all columns specified in
#' address_column_names.
#' @param address_column_names A character vector specifying the column names in
#' address_table that should be united to form a complete address for geocoding.
#' All columns in this vector must exist in address_table.
#' @param api_key A character string containing your Google Maps API key. Defaults
#' to the value of GOOGLE_MAPS_API_KEY in the .Renviron file.
#' @param save_file_path Optional. A character string specifying the path where the
#' geocoded data will be saved as a CSV file. If NULL, the data is not saved to a
#' file. Default is NULL.
#' @param enable_verbose Logical. If TRUE, detailed logging will be printed to the
#' console during execution. Default is TRUE.
#'
#' @returns A [tibble][tibble::tibble-package] with the columns:
#'   - complete_address: Combined address column used for geocoding.
#'   - latitude: Geocoded latitude for the address.
#'   - longitude: Geocoded longitude for the address.
#'   - All original columns from address_table.
#'
#' @importFrom ggmap geocode register_google
#' @importFrom dplyr mutate
#' @importFrom tidyr unite
#' @importFrom purrr map_dbl
#' @importFrom logger log_info log_error
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom readr write_csv
#'
#' @examples
#' # Example 1: Basic usage with verbose logging and no file saving
#' addresses <- tibble::tibble(
#'   street = c("1600 Amphitheatre Parkway", "1 Infinite Loop"),
#'   city = c("Mountain View", "Cupertino"),
#'   state = c("CA", "CA"),
#'   zip_code = c("94043", "95014")
#' )
#'
#' geocoded_addresses <- phase0_geocode(
#'   address_table = addresses,
#'   address_column_names = c("street", "city", "state", "zip_code"),
#'   api_key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
#'   enable_verbose = TRUE
#' )
#' print(geocoded_addresses)
#'
#' # Example 2: Save geocoded data to a CSV file
#' addresses <- tibble::tibble(
#'   street = c("1600 Amphitheatre Parkway", "1 Infinite Loop"),
#'   city = c("Mountain View", "Cupertino"),
#'   state = c("CA", "CA"),
#'   zip_code = c("94043", "95014")
#' )
#'
#' geocoded_addresses <- phase0_geocode(
#'   address_table = addresses,
#'   address_column_names = c("street", "city", "state", "zip_code"),
#'   api_key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
#'   save_file_path = "geocoded_addresses.csv",
#'   enable_verbose = TRUE
#' )
#' print(geocoded_addresses)
#'
#' # Example 3: Minimal logging and no file saving
#' addresses <- tibble::tibble(
#'   street = c("1600 Amphitheatre Parkway", "1 Infinite Loop"),
#'   city = c("Mountain View", "Cupertino"),
#'   state = c("CA", "CA"),
#'   zip_code = c("94043", "95014")
#' )
#'
#' geocoded_addresses <- phase0_geocode(
#'   address_table = addresses,
#'   address_column_names = c("street", "city", "state", "zip_code"),
#'   api_key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
#'   enable_verbose = FALSE
#' )
#' print(geocoded_addresses)
#'
#' @export
phase0_geocode <- function(address_table,
                           address_column_names,
                           api_key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
                           save_file_path = NULL,
                           enable_verbose = TRUE) {
  # Define logging behavior
  log_message <- if (enable_verbose) logger::log_info else function(...) {}

  # Log function initialization
  log_message("Initializing phase0_geocode function")

  # Validate inputs
  assertthat::assert_that(is.data.frame(address_table), msg = "The input 'address_table' must be a data frame.")
  log_message("Input 'address_table' validated as a data frame.")

  assertthat::assert_that(is.character(address_column_names), msg = "The 'address_column_names' must be a character vector.")
  log_message("Input 'address_column_names' validated as a character vector.")

  # Check API key availability
  if (api_key == "") {
    logger::log_error("API key missing. Ensure it is set in the 'GOOGLE_MAPS_API_KEY' environment variable or passed explicitly.")
    stop("Google Maps API key is required.")
  }
  log_message("API key provided and valid.")

  # Register Google Maps API key
  ggmap::register_google(key = api_key)
  log_message("Google Maps API key registered successfully.")

  # Check for missing columns in the address table
  missing_columns <- setdiff(address_column_names, colnames(address_table))
  if (length(missing_columns) > 0) {
    logger::log_error(glue("The following required columns are missing: {paste(missing_columns, collapse = ', ')}"))
    stop(glue("The input 'address_table' is missing columns: {paste(missing_columns, collapse = ', ')}"))
  }
  log_message("All specified address columns found in 'address_table'.")

  # Unite address columns into a single column named 'complete_address'
  united_table <- address_table %>%
    tidyr::unite("complete_address", all_of(address_column_names), sep = ", ", na.rm = TRUE, , remove = FALSE)
  log_message("Address columns united into a single column named 'complete_address'.")

  # Perform geocoding
  geocoded_table <- add_geocoding_columns(united_table, api_key, enable_verbose)
  log_message("Geocoding completed successfully. Latitude and longitude columns added.")

  # Save the geocoded table if a save path is provided
  if (!is.null(save_file_path)) {
    save_geocoded_table(geocoded_table, save_file_path, enable_verbose)
  }

  # Log function completion
  log_message("phase0_geocode function completed.")
  log_message(glue("The geocoded table contains {nrow(geocoded_table)} rows and {ncol(geocoded_table)} columns."))

  return(geocoded_table)
}

# Helper function to geocode addresses
add_geocoding_columns <- function(table_with_addresses, api_key, enable_verbose = TRUE) {
  log_message <- if (enable_verbose) logger::log_info else function(...) {}
  log_message("Starting geocoding process.")

  geocoded_table <- table_with_addresses %>%
    dplyr::mutate(
      latitude = purrr::map_dbl(complete_address, ~ {
        geocode_result <- ggmap::geocode(.x, key = api_key)
        log_message(glue::glue("Geocoded '{.x}': Lat = {geocode_result$lat}, Lon = {geocode_result$lon}"))
        geocode_result$lat
      }),
      longitude = purrr::map_dbl(complete_address, ~ ggmap::geocode(.x, key = api_key)$lon)
    )

  return(geocoded_table)
}

# Helper function to save the geocoded table
save_geocoded_table <- function(geocoded_table, file_path, enable_verbose = TRUE) {
  log_message <- if (enable_verbose) logger::log_info else function(...) {}
  log_message(glue("Saving geocoded table to file: {file_path}"))
  readr::write_csv(geocoded_table, file_path)
  log_message("File saved successfully.")
}
