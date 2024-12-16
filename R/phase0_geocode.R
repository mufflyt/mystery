#' Geocode Unique Addresses with Detailed Logging
#'
#' This function geocodes unique addresses in a dataset using the Google Maps API, appending
#' latitude and longitude information to each unique address. Extensive logging provides
#' updates on each step, including function inputs, data transformations, and outputs. The
#' geocoded data can optionally be saved to a specified output file.
#'
#' @param file_path A character string specifying the path to the input file, which must be a CSV, RDS, or XLSX file containing a column named 'address' with the addresses to be geocoded.
#' @param google_maps_api_key A character string containing your Google Maps API key. This key is required to access the Google Maps geocoding service.
#' @param save_to_file_path Optional. A character string specifying the path where the geocoded data will be saved as a CSV file. If NULL, the data is not saved to a file.
#'
#' @return A tibble with geocoded address data, including latitude and longitude columns.
#'
#' @importFrom ggmap geocode
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate select
#' @importFrom purrr map_dbl
#' @importFrom logger log_info log_error
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with input file and Google Maps API key
#' # Here we geocode addresses from "addresses.csv" without saving to an output file
#' google_maps_api_key <- "YOUR_API_KEY"
#' file_path <- "addresses.csv"
#' geocoded_data <- phase0_geocode(file_path, google_maps_api_key)
#'
#' # Output:
#' # Geocoding unique addresses...
#' # Starting phase0_geocode function
#' # Input file path: addresses.csv
#' # Output file path: NULL
#' # Reading data from addresses.csv
#' # Initializing geocoding process with Google Maps API
#' # ...
#' # Geocoding complete.
#'
#' # Example 2: Specifying an output file path to save the geocoded results
#' save_to_file_path <- "geocoded_addresses.csv"
#' geocoded_data <- phase0_geocode(file_path, google_maps_api_key, save_to_file_path)
#'
#' # Output:
#' # Geocoding unique addresses...
#' # Starting phase0_geocode function
#' # Input file path: addresses.csv
#' # Output file path: geocoded_addresses.csv
#' # Reading data from addresses.csv
#' # Initializing geocoding process with Google Maps API
#' # ...
#' # File saved successfully to geocoded_addresses.csv
#'
#' # Example 3: Geocoding with a dataset that lacks the 'address' column
#' # This example demonstrates error handling for a missing 'address' column.
#' file_path <- "invalid_addresses.csv" # Assume this file does not have 'address' column
#' try(phase0_geocode(file_path, google_maps_api_key))
#'
#' # Output:
#' # Geocoding unique addresses...
#' # Starting phase0_geocode function
#' # Input file path: invalid_addresses.csv
#' # Output file path: NULL
#' # Reading data from invalid_addresses.csv
#' # Error: The dataset must have a column named 'address' for geocoding
#' }
#'
#' @export
phase0_geocode <- function(file_path, google_maps_api_key, save_to_file_path = NULL) {

  # Log function start and inputs
  logger::log_info("Starting phase0_geocode function")
  logger::log_info("Input file path: {file_path}")
  logger::log_info("Google Maps API key provided: {if (!is.null(google_maps_api_key)) 'Yes' else 'No'}")
  logger::log_info("Output file path: {save_to_file_path}")

  # Load data and log data loading step
  address_dataset <- load_address_data(file_path)
  logger::log_info("Data loaded with {nrow(address_dataset)} rows and {ncol(address_dataset)} columns")

  # Check if 'address' column exists
  if (!"address" %in% colnames(address_dataset)) {
    logger::log_error("The dataset must have a column named 'address' for geocoding")
    stop("The dataset must have a column named 'address' for geocoding.")
  }

  # Geocode each address and log each transformation step
  geocoded_addresses <- add_geocode_columns(address_dataset, google_maps_api_key)
  logger::log_info("Geocoding completed. Added latitude and longitude columns")

  # Save to CSV if output path is provided, log file location
  if (!is.null(save_to_file_path)) {
    save_geocoded_data(geocoded_addresses, save_to_file_path)
  }

  # Log function completion and output details
  logger::log_info("phase0_geocode function completed successfully")
  logger::log_info("Output data has {nrow(geocoded_addresses)} rows and {ncol(geocoded_addresses)} columns")

  # Return the geocoded data frame
  return(geocoded_addresses)
}

# Helper function to load data with logging
# @noRd
load_address_data <- function(file_path) {
  if (!file.exists(file_path)) {
    logger::log_error("Input file not found: {file_path}")
    stop("Input file not found.")
  }
  logger::log_info("Reading data from {file_path}")
  address_data <- readr::read_csv(file_path, show_col_types = FALSE)
  return(address_data)
}

# Helper function to geocode addresses and add latitude/longitude columns
# @noRd
add_geocode_columns <- function(address_dataset, google_maps_api_key) {
  logger::log_info("Initializing geocoding process with Google Maps API")

  progress_bar <- progress::progress_bar$new(total = nrow(address_dataset), format = "[:bar] :percent :elapsed :eta :rate")

  geocoded_dataset <- address_dataset %>%
    dplyr::mutate(
      latitude = purrr::map_dbl(address, ~ {
        geocode_result <- ggmap::geocode(.x, key = google_maps_api_key)
        progress_bar$tick()
        logger::log_info("Geocoded address: {.x} -> Lat: {geocode_result$lat}, Lon: {geocode_result$lon}")
        geocode_result$lat
      }),
      longitude = purrr::map_dbl(address, ~ ggmap::geocode(.x, key = google_maps_api_key)$lon)
    )

  return(geocoded_dataset)
}

# Helper function to save geocoded data to CSV with logging
# @noRd
save_geocoded_data <- function(geocoded_dataset, save_to_file_path) {
  logger::log_info("Saving geocoded data to {save_to_file_path}")
  readr::write_csv(geocoded_dataset, save_to_file_path)
  logger::log_info("File saved successfully to {save_to_file_path}")
}

