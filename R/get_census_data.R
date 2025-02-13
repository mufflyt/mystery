#' Retrieve Census Data for State Block Groups
#'
#' This function retrieves Census data for all state block groups by looping
#' over the specified list of state FIPS codes. The data is fetched using the
#' Census API for a specified vintage year and aggregated into a single dataframe.
#'
#' @param state_fips_codes A character vector of state FIPS codes for which Census data is to be retrieved.
#'   FIPS codes must be two-character strings representing U.S. states (e.g., "01" for Alabama).
#' @param vintage_year An integer specifying the vintage year of Census data. Default is 2022.
#'
#' @return A dataframe containing Census data for all specified state block groups, including metadata.
#'
#' @details This function uses the `censusapi` package to query Census data via the American Community Survey
#'   (ACS) API. Data is retrieved for block groups across all specified states and aggregated into a single dataframe.
#'   Users must provide a valid Census API key via the `key` argument or environment variables.
#'
#' @examples
#' # Example 1: Retrieve Census data for two states with the default vintage year
#' \dontrun{
#' state_fips_codes <- c("01", "02") # Alabama and Alaska
#' census_data <- get_census_data(state_fips_codes)
#' head(census_data)
#' }
#'
#' # Example 2: Retrieve Census data for multiple states with a different vintage year
#' \dontrun{
#' state_fips_codes <- c("04", "05", "06") # Arizona, Arkansas, California
#' census_data <- get_census_data(state_fips_codes, vintage_year = 2021)
#' print(dim(census_data)) # Display dimensions of the retrieved data
#' }
#'
#' # Example 3: Retrieve Census data for a subset of states and save it to a CSV file
#' \dontrun{
#' state_fips_codes <- c("01", "02", "04", "05", "06", "08", "09")
#' census_data <- get_census_data(state_fips_codes)
#' write.csv(census_data, file = "census_data_block_groups.csv", row.names = FALSE)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows
#' @importFrom censusapi getCensus
#' @export
get_census_data <- function(state_fips_codes, vintage_year = 2022) {
  # Validate inputs
  assertthat::assert_that(is.character(state_fips_codes),
    msg = "`state_fips_codes` must be a character vector of FIPS codes."
  )
  assertthat::assert_that(all(nchar(state_fips_codes) == 2),
    msg = "Each FIPS code must be exactly two characters."
  )
  assertthat::assert_that(is.numeric(vintage_year) && vintage_year > 2000,
    msg = "`vintage_year` must be a valid year greater than 2000."
  )

  # Check if required packages are installed
  if (!requireNamespace("censusapi", quietly = TRUE)) {
    stop("The `censusapi` package is required but not installed. Please install it using install.packages('censusapi').")
  }

  # Initialize an empty list to store Census data for each state
  state_data_list <- list()

  # Loop over each FIPS code to retrieve data
  for (fips_code in state_fips_codes) {
    message("Processing Census data for FIPS: ", fips_code)

    # Define the region for the current state
    region_query <- paste("state:", fips_code, "&in=county:*&in=tract:*", sep = "")

    # Query the Census API
    state_data <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = vintage_year,
      vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")), # Modify variables as needed
      region = "block group:*",
      regionin = region_query,
      key = Sys.getenv("CENSUS_API_KEY") # Ensure API key is available in environment
    )

    # Append data to the list
    state_data_list[[fips_code]] <- state_data
  }

  # Combine all state data into a single dataframe
  combined_census_data <- dplyr::bind_rows(state_data_list)

  # Return the combined data
  return(combined_census_data)
}
