#' Fetch CMS Provider Data for Multiple Values with Pagination and Logging
#'
#' This function retrieves filtered CMS provider data using pagination. It handles multiple values for a single property by querying each value separately, combining results, and logging each step.
#'
#' @param sys_sleep Numeric. Number of seconds to wait between API requests to avoid rate limiting. Default is 1 second.
#' @param property Character. The property (column) to filter by. Default is "pri_spec".
#' @param value Character vector. The values to filter within the specified property. Default is "OBSTETRICS/GYNECOLOGY".
#' @param output_csv_path Character. Optional path to save the fetched data as a CSV file with a timestamp.
#' @return A data frame with the CMS provider data filtered according to the specified criteria.
#' @importFrom httr POST content status_code
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom logger log_info log_error
#' @examples
#' fetch_cms_data(sys_sleep = 1, property = "pri_spec", value = c("OBSTETRICS/GYNECOLOGY", "OTOLARYNGOLOGY"))
#' @export
phase0_national_downloadable_file <- function(sys_sleep = 1, property = "pri_spec", value = "OBSTETRICS/GYNECOLOGY", output_csv_path = NULL) {
  # Log the function inputs
  logger::log_info("Function phase0_national_downloadable_file called with inputs: sys_sleep = {sys_sleep}, property = {property}, value = {value}")

  # Define the base URL
  url <- "https://data.cms.gov/provider-data/api/1/datastore/query/mj5m-pzi6/0"

  # Initialize an empty list to store results for all values
  all_results <- list()

  # Loop through each value in `value` to fetch data
  for (single_value in value) {
    logger::log_info("Fetching data for {property} = {single_value}")

    # Initialize pagination variables
    limit <- 100
    offset <- 0
    keep_querying <- TRUE

    # Loop to paginate through results for the current `single_value`
    while (keep_querying) {
      # Define the body of the POST request
      body <- list(
        conditions = list(
          list(
            property = property,
            value = single_value,
            operator = "="
          )
        ),
        limit = limit,
        offset = offset,
        results = TRUE
      )

      # Log the request body for debugging
      logger::log_info("Request body: {jsonlite::toJSON(body, auto_unbox = TRUE)}")

      # Convert body to JSON for the POST request
      json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

      # Make the POST request
      response <- httr::POST(
        url,
        body = json_body,
        encode = "json",
        httr::accept("application/json")
      )

      # Check response status
      if (httr::status_code(response) == 200) {
        data_content <- httr::content(response, "text")
        cms_data <- jsonlite::fromJSON(data_content, flatten = TRUE)

        # Append results if present
        if (!is.null(cms_data$results)) {
          logger::log_info("Fetched {length(cms_data$results)} rows for {property} = {single_value} with offset: {offset}")
          all_results[[length(all_results) + 1]] <- dplyr::as_tibble(cms_data$results)
        }

        # Stop loop if fewer than 'limit' records are returned
        if (length(cms_data$results) < limit) {
          logger::log_info("Final batch for {property} = {single_value} received with {length(cms_data$results)} rows. Exiting pagination loop.")
          keep_querying <- FALSE
        } else {
          offset <- offset + limit
        }
      } else {
        logger::log_error("Failed to retrieve data for {property} = {single_value}. Status code: {httr::status_code(response)}")
        stop("Failed to retrieve filtered data from the CMS API.")
      }

      # Wait to avoid hitting rate limits
      Sys.sleep(sys_sleep)
    }
  }

  # Combine all results across values into a single data frame
  combined_data <- dplyr::bind_rows(all_results)
  logger::log_info("All data fetched and combined. Total rows: {nrow(combined_data)}, Total columns: {ncol(combined_data)}")

  # Save to CSV if a path is provided
  if (!is.null(output_csv_path)) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_path <- file.path(output_csv_path, paste0("cms_data_", timestamp, ".csv"))
    readr::write_csv(combined_data, file_path)
    logger::log_info("Data saved to file at: {file_path}")
  }

  # Log function completion
  logger::log_info("Function phase0_national_downloadable_file completed successfully.")
  return(combined_data)
}
