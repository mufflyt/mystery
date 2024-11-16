# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(logger)

#' Fetch CMS Provider Data for Multiple Properties with Pagination and Logging
#'
#' This function retrieves CMS provider data for a given specialty across multiple properties, handling pagination and logging each step.
#' It combines results across all properties and optionally saves the final data frame as a CSV file.
#'
#' @param property_variable Character vector of property names to search (e.g., c("pri_spec", "sec_spec_1", ...)).
#' @param specialty Character. The specialty value to search for within each property (e.g., "OTOLARYNGOLOGY").
#' @param url Character. The CMS API endpoint URL. Defaults to "https://data.cms.gov/provider-data/api/1/datastore/query/mj5m-pzi6/0".
#' @param limit Integer. Number of records per API request. Default is 100.
#' @param sys_sleep Numeric. Delay in seconds between API requests to avoid rate limiting. Default is 1.
#' @param output_csv_path Character. Optional path to save the fetched data as a CSV file with a timestamp. Default is NULL.
#'
#' @return A data frame containing combined results across all properties. If no data is retrieved, returns an empty data frame.
#' @examples
#' \dontrun{
#' phase0_national_downloadable_file(
#'   property_variable = c("pri_spec", "sec_spec_1", "sec_spec_2"),
#'   specialty = "OTOLARYNGOLOGY",
#'   output_csv_path = "path/to/save_directory"
#' )
#' }
#' @export
phase0_national_downloadable_file <- function(
    property_variable,
    specialty,
    url = "https://data.cms.gov/provider-data/api/1/datastore/query/mj5m-pzi6/0",
    limit = 100,
    sys_sleep = 1,
    output_csv_path = NULL
) {
  all_property_results <- list()  # Initialize an empty list to store all results across properties

  # Loop through each property in property_variable
  for (property in property_variable) {
    logger::log_info("Searching for specialty '{specialty}' in property '{property}'")

    offset <- 0
    keep_querying <- TRUE
    property_results <- list()  # To store results for the current property

    # Paginate through results for each property
    while (keep_querying) {
      # Define request body
      body <- list(
        conditions = list(
          list(
            property = property,
            value = specialty,
            operator = "="
          )
        ),
        limit = limit,
        offset = offset,
        results = TRUE
      )

      json_body <- toJSON(body, auto_unbox = TRUE)  # Convert to JSON
      response <- POST(url, body = json_body, encode = "json", accept("application/json"))

      # Check response and parse
      if (status_code(response) == 200) {
        data_content <- content(response, "text", encoding = "UTF-8")
        cms_data <- fromJSON(data_content, flatten = TRUE)

        if (!is.null(cms_data$results) && length(cms_data$results) > 0) {
          # Add a column to indicate the property source
          property_data <- as.data.frame(cms_data$results) %>%
            mutate(property = property)

          property_results[[length(property_results) + 1]] <- property_data
          logger::log_info("Fetched {nrow(property_data)} rows for '{property}' with offset: {offset}")
        } else {
          logger::log_info("No more results returned for '{property}' with offset {offset}. Ending loop.")
          break
        }

        # Stop loop if fewer than 'limit' records are returned
        if (nrow(property_data) < limit) {
          keep_querying <- FALSE
        } else {
          offset <- offset + limit
          Sys.sleep(sys_sleep)
        }
      } else {
        logger::log_error("Failed to retrieve data for property '{property}'")
        stop("Failed to retrieve data from CMS API for property:", property)
      }
    }

    # Combine results for the current property
    if (length(property_results) > 0) {
      property_data <- bind_rows(property_results)
      all_property_results[[property]] <- property_data
    }
  }

  # Combine all results across properties
  final_data <- bind_rows(all_property_results)

  # Optionally save to CSV
  if (!is.null(output_csv_path) && nrow(final_data) > 0) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_path <- file.path(output_csv_path, paste0("cms_data_", timestamp, ".csv"))
    write_csv(final_data, file_path)
    logger::log_info("Data saved to file at: {file_path}")
  }

  logger::log_info("Function phase0_national_downloadable_file completed successfully.")
  return(final_data)
}
