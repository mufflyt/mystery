#' Fetch CMS Provider Data with Pagination and Logging
#'
#' This function retrieves filtered CMS provider data using pagination and supports customization of
#' query parameters. Each transformation and operation is logged to the console using the `logger` package.
#'
#' @param sys_sleep Numeric. Number of seconds to wait between API requests to avoid rate limiting. Default is 1 second.
#' @param property Character. The property (column) to filter. Default is "pri_spec".
#' @param value Character. The value to filter by within the specified property. Default is "OBSTETRICS/GYNECOLOGY".
#' @return A data frame with the CMS provider data filtered according to the specified criteria.
#' @examples
#' # Default parameters fetching obstetrics/gynecology providers with 1-second delay
#' fetch_cms_data()
#'
#' # Custom parameters to fetch data filtered by another specialty
#' fetch_cms_data(sys_sleep = 2, property = "pri_spec", value = "PEDIATRICS")
#'
#' # Fetching data with different wait time and offset values
#' fetch_cms_data(sys_sleep = 0.5, property = "state", value = "NY")
#'
#' @importFrom httr POST content status_code
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr bind_rows
fetch_cms_data <- function(sys_sleep = 1, property = "pri_spec", value = "OBSTETRICS/GYNECOLOGY") {
  # Log the function inputs
  logger::log_info("Function fetch_cms_data called with inputs: sys_sleep = {sys_sleep}, property = {property}, value = {value}")

  # Define the base URL
  url <- "https://data.cms.gov/provider-data/api/1/datastore/query/mj5m-pzi6/0"

  # Initialize an empty list to store all paginated results
  all_results <- list()
  limit <- 100
  offset <- 0
  keep_querying <- TRUE

  # Loop to paginate through results
  while (keep_querying) {
    # Define the body of the POST request
    body <- list(
      conditions = list(
        list(
          property = property,
          value = value,
          operator = "="
        )
      ),
      limit = limit,
      offset = offset,
      results = TRUE
    )

    # Convert body to JSON for the POST request
    json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

    # Make the POST request
    response <- httr::POST(
      url,
      body = json_body,
      encode = "json",
      httr::accept("application/json")
    )

    # Check response and parse
    if (httr::status_code(response) == 200) {
      data_content <- httr::content(response, "text")
      cms_data <- jsonlite::fromJSON(data_content, flatten = TRUE)

      # Append the current batch of results to the all_results list
      if (!is.null(cms_data$results)) {
        logger::log_info("Fetched {length(cms_data$results)} rows with offset: {offset}")
        all_results[[length(all_results) + 1]] <- dplyr::as_tibble(cms_data$results)
      }

      # Stop the loop if fewer than 'limit' records are returned, indicating the last page
      if (length(cms_data$results) < limit) {
        logger::log_info("Final batch received with {length(cms_data$results)} rows. Exiting pagination loop.")
        keep_querying <- FALSE
      } else {
        # Increment the offset for the next batch
        offset <- offset + limit
      }
    } else {
      logger::log_error("Failed to retrieve data. Status code: {httr::status_code(response)}")
      stop("Failed to retrieve filtered data from the CMS API.")
    }

    # Respect rate limiting by sleeping between requests
    Sys.sleep(sys_sleep)
  }

  # Combine all batches into a single data frame
  final_data <- dplyr::bind_rows(all_results)
  logger::log_info("All data fetched and combined. Total rows: {nrow(final_data)}, Total columns: {ncol(final_data)}")

  # Log the function output
  logger::log_info("Function fetch_cms_data completed. Returning final data frame.")
  return(final_data)
}
