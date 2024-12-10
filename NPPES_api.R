## Full Code
#######
# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(logger)

# # Set parameters for the query
# taxonomy_properties <- c("primary_taxonomy", "secondary_taxonomy_1", "secondary_taxonomy_2")
# taxonomy_code <- "207V00000X"  # Example taxonomy code for otolaryngology
# url <- "https://npiregistry.cms.hhs.gov/api/?version=2.1"
# limit <- 200  # Max number of records per request according to NPPES limits
# sys_sleep <- 1  # Delay between requests to avoid rate limiting
# output_csv_path <- "Melanie/data"  # Set your desired output directory
#
# # Initialize an empty list to store all results across properties
# all_property_results <- list()
#
# # Loop through each taxonomy property
# for (property in taxonomy_properties) {
#   logger::log_info("Searching for taxonomy code '{taxonomy_code}' in property '{property}'")
#
#   # Initialize pagination variables
#   skip <- 0
#   keep_querying <- TRUE
#   property_results <- list()  # To store results for the current property
#
#   # Paginate through results for each property
#   while (keep_querying) {
#     # Define query parameters
#     query_params <- list(
#       taxonomy_description = taxonomy_code,
#       limit = limit,
#       skip = skip
#     )
#
#     # Make the GET request
#     response <- GET(url, query = query_params)
#
#     # Check response and parse
#     if (status_code(response) == 200) {
#       data_content <- content(response, "text", encoding = "UTF-8")
#       nppes_data <- fromJSON(data_content, flatten = TRUE)
#
#       # Check if 'results' is present and has data
#       if (!is.null(nppes_data$results) && length(nppes_data$results) > 0) {
#         # Add a column to indicate the property source
#         property_data <- as.data.frame(nppes_data$results) %>%
#           mutate(taxonomy_property = property)
#
#         property_results[[length(property_results) + 1]] <- property_data
#         logger::log_info("Fetched {nrow(property_data)} rows for '{property}' with skip: {skip}")
#       } else {
#         logger::log_info("No more results returned for '{property}' with skip {skip}. Ending loop.")
#         break
#       }
#
#       # Stop loop if fewer than 'limit' records are returned
#       if (nrow(property_data) < limit) {
#         keep_querying <- FALSE
#       } else {
#         skip <- skip + limit
#         Sys.sleep(sys_sleep)  # Optional: wait to avoid hitting rate limits
#       }
#     } else {
#       logger::log_error("Failed to retrieve data for property '{property}'")
#       stop("Failed to retrieve data from NPPES API for property:", property)
#     }
#   }
#
#   # Combine results for the current property
#   if (length(property_results) > 0) {
#     property_data <- bind_rows(property_results)
#     all_property_results[[property]] <- property_data
#   }
# }
#
# # Combine all results across properties
# final_data <- bind_rows(all_property_results)
#
# # Optionally save to CSV
# if (!is.null(output_csv_path) && nrow(final_data) > 0) {
#   timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
#   file_path <- file.path(output_csv_path, paste0("nppes_taxonomy_data_", timestamp, ".csv"))
#   write_csv(final_data, file_path)
#   logger::log_info("Data saved to file at: {file_path}")
# }
#
# logger::log_info("NPPES taxonomy search completed successfully.")
# print(final_data)

####

# TESTING
# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(logger)

# Set parameters for the query
taxonomy_properties <- c("primary_taxonomy",
                         "secondary_taxonomy_1",
                         "secondary_taxonomy_2")
taxonomy_code <- "OBSTETRICS & GYNECOLOGY"  # Example taxonomy code for otolaryngology
url <- "https://npiregistry.cms.hhs.gov/api/?version=2.1" # NPPES API
limit <- 200  # Max number of records per request according to NPPES limits
sys_sleep <- 10  # Delay between requests to avoid rate limiting
output_csv_path <- "/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/Melanie/data/"

# Initialize an empty list to store all results across properties
all_property_results <- list()

# Loop through each taxonomy property
for (property in taxonomy_properties) {
  logger::log_info("Searching for taxonomy code '{taxonomy_code}' in property '{property}'")

  # Initialize pagination variables
  skip <- 0
  keep_querying <- TRUE
  property_results <- list()  # To store results for the current property

  # Paginate through results for each property
  while (keep_querying) {
    # Define query parameters
    query_params <- list(
      taxonomy_description = taxonomy_code,
      limit = limit,
      skip = skip
    )

    # Make the GET request
    response <- GET(url, query = query_params)

    # Check response and parse
    if (status_code(response) == 200) {
      data_content <- content(response, "text", encoding = "UTF-8")
      nppes_data <- fromJSON(data_content, flatten = TRUE)

      # Check if 'results' is present and has data
      if (!is.null(nppes_data$results) && length(nppes_data$results) > 0) {
        # Add a column to indicate the property source
        property_data <- as.data.frame(nppes_data$results) %>%
          mutate(taxonomy_property = property)

        property_results[[length(property_results) + 1]] <- property_data
        logger::log_info("Fetched {nrow(property_data)} rows for '{property}' with skip: {skip}")
      } else {
        logger::log_info("No more results returned for '{property}' with skip {skip}. Ending loop.")
        break
      }

      # Stop loop if fewer than 'limit' records are returned
      if (nrow(property_data) < limit) {
        keep_querying <- FALSE
      } else {
        skip <- skip + limit
        Sys.sleep(sys_sleep)  # Optional: wait to avoid hitting rate limits
      }
    } else {
      logger::log_error("Failed to retrieve data for property '{property}'")
      stop("Failed to retrieve data from NPPES API for property:", property)
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
  file_path <- file.path(output_csv_path, paste0("nppes_taxonomy_data_", timestamp, ".csv"))
  write_csv(final_data, file_path)
  logger::log_info("Data saved to file at: {file_path}")
}

logger::log_info("NPPES taxonomy search completed successfully.")
print(final_data)

#####
# Function
#####
# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(logger)

# Define the function to download NPPES data by taxonomy code with default values
download_nppes_data <- function(
    taxonomy_code = "OBSTETRICS & GYNECOLOGY",
    output_csv_path = "/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/Melanie/data/",
    limit = 200,
    sys_sleep = 1
) {
  # Define taxonomy properties to search within
  taxonomy_properties <- c("primary_taxonomy", "secondary_taxonomy_1", "secondary_taxonomy_2")
  url <- "https://npiregistry.cms.hhs.gov/api/?version=2.1"

  # Initialize an empty list to store all results across properties
  all_property_results <- list()

  # Loop through each taxonomy property
  for (property in taxonomy_properties) {
    logger::log_info("Searching for taxonomy code '{taxonomy_code}' in property '{property}'")

    # Initialize pagination variables
    skip <- 0
    keep_querying <- TRUE
    property_results <- list()

    # Paginate through results for each property
    while (keep_querying) {
      # Define query parameters
      query_params <- list(
        taxonomy_description = taxonomy_code,
        limit = limit,
        skip = skip
      )

      # Make the GET request
      response <- httr::GET(url, query = query_params)

      # Check response and parse
      if (httr::status_code(response) == 200) {
        data_content <- httr::content(response, "text", encoding = "UTF-8")
        nppes_data <- jsonlite::fromJSON(data_content, flatten = TRUE)

        # Check if 'results' is present and has data
        if (!is.null(nppes_data$results) && length(nppes_data$results) > 0) {
          # Add a column to indicate the property source
          property_data <- as.data.frame(nppes_data$results) %>%
            dplyr::mutate(taxonomy_property = property)

          property_results[[length(property_results) + 1]] <- property_data
          logger::log_info("Fetched {nrow(property_data)} rows for '{property}' with skip: {skip}")
        } else {
          logger::log_info("No more results returned for '{property}' with skip {skip}. Ending loop.")
          break
        }

        # Stop loop if fewer than 'limit' records are returned
        if (nrow(property_data) < limit) {
          keep_querying <- FALSE
        } else {
          skip <- skip + limit
          Sys.sleep(sys_sleep)
        }
      } else {
        logger::log_error("Failed to retrieve data for property '{property}'")
        stop("Failed to retrieve data from NPPES API for property:", property)
      }
    }

    # Combine results for the current property
    if (length(property_results) > 0) {
      property_data <- dplyr::bind_rows(property_results)
      all_property_results[[property]] <- property_data
    }
  }

  # Combine all results across properties
  final_data <- dplyr::bind_rows(all_property_results)

  # Optionally save to CSV with timestamp
  if (!is.null(output_csv_path) && nrow(final_data) > 0) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_path <- file.path(output_csv_path, paste0("nppes_taxonomy_data_", timestamp, ".csv"))
    readr::write_csv(final_data, file_path)
    logger::log_info("Data saved to file at: {file_path}")
  }

  logger::log_info("NPPES taxonomy search completed successfully.")
  return(final_data)
}

# Example usage with defaults
# final_data <- download_nppes_data()
