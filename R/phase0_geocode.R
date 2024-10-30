#' Geocode Unique Addresses
#'
#' This function geocodes unique addresses using the Google Maps API and appends the
#' latitude and longitude to the original dataset. Please ensure that every dataset
#' must have a column named 'address'.
#'
#' @param file_path Path to the input file (CSV, RDS, or XLSX) containing address data.
#' @param google_maps_api_key Your Google Maps API key.
#' @param output_file_path Path to the output CSV file where the geocoded data will be saved. (Optional)
#'
#' @return A dataframe containing geocoded address data with latitude and longitude.
#'
#' @examples
#' \dontrun{
#' # Define the input file path, Google Maps API key, and output file path (optional)
#' file_path <- "input_data.csv"
#' google_maps_api_key <- "your_api_key"
#' output_file_path <- "output_data.csv"  # Optional
#'
#' # Call the geocode_unique_addresses function with or without specifying output_file_path
#' geocoded_data <- geocode_unique_addresses(file_path, google_maps_api_key)
#' # or
#' geocoded_data <- geocode_unique_addresses(file_path, google_maps_api_key, output_file_path)
#' }
#'
#' @importFrom ggmap geocode
#' @importFrom readr write_csv
#' @importFrom dplyr mutate
#' @importFrom progress progress_bar
#' @importFrom purrr map_df
#'
#' @export
geocode_unique_addresses <- function(file_path, google_maps_api_key, output_file_path = NULL) {
  # Log function start
  cat("Geocoding unique addresses...\n")

  # Check if the file path exists
  if (!file.exists(file_path)) {
    stop("Input file not found.")
  }

  # Read the data from CSV, RDS, or XLSX file
  data <- readr::read_csv(file_path)

  # Check if the data contains a column named "address"
  if (!"address" %in% colnames(data)) {
    stop("The dataset must have a column named 'address' for geocoding.")
  }

  # Initialize a progress bar
  pb <- progress::progress_bar$new(total = nrow(data), format = "[:bar] :percent :elapsed :eta :rate")

  # Geocode each address and append latitude and longitude
  geocoded_data <- data %>%
    dplyr::mutate(
      latitude = NA_real_,
      longitude = NA_real_
    )

  for (i in seq_len(nrow(geocoded_data))) {
    address <- geocoded_data$address[i]
    result <- ggmap::geocode(address, key = google_maps_api_key)
    geocoded_data$latitude[i] <- result$lat
    geocoded_data$longitude[i] <- result$lon
    pb$tick()  # Increment progress bar
  }

  # Save geocoded data to CSV if output_file_path is provided
  if (!is.null(output_file_path)) {
    readr::write_csv(geocoded_data, output_file_path)
    cat("Geocoded data saved to:", output_file_path, "\n")
  }

  # Log function end
  cat("Geocoding complete.\n")

  # Return geocoded data frame
  return(geocoded_data)
}
