#' Query CMS National Downloadable File for Provider Information
#'
#' @description
#' Fetches provider information from the CMS National Downloadable File API using NPI
#' numbers. Includes robust error handling, logging, and batch processing.
#'
#' @param npi_numbers Character vector of NPI numbers to query
#' @param batch_size Integer. Number of NPIs to query per batch. Default: 10
#' @param url Character. CMS API endpoint URL. Default: CMS data store URL
#' @param limit Integer. Maximum records per API call. Default: 100
#' @param sys_sleep Numeric. Sleep time between API calls in seconds. Default: 0.5
#' @param user_agent Character. User agent string for API calls.
#'        Default: "r-package-cms-query"
#' @param verbose Logical. Whether to output detailed logs. Default: FALSE
#' @param csv_save_path Character. Optional path to save results as CSV. Default: NULL
#'
#' @return A tibble containing provider information from CMS with "NDF_" prefixed
#'         column names. Common returned fields include:
#' \itemize{
#'   \item NDF_npi - National Provider Identifier
#'   \item NDF_provider_name - Provider's full name
#'   \item NDF_cred - Provider credentials (e.g., "M.D.", "D.O.")
#'   \item NDF_provider_address - Practice location address
#'   \item NDF_provider_state - State of practice
#'   \item NDF_specialty - Provider specialty description
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with minimal parameters
#' npi_results <- phase0_national_downloadable_file(
#'   npi_numbers = c("1234567890", "0987654321"),
#'   verbose = TRUE
#' )
#'
#' # Example with CSV output
#' npi_with_save <- phase0_national_downloadable_file(
#'   npi_numbers = c("1234567890", "0987654321"),
#'   batch_size = 2,
#'   csv_save_path = "output/cms_data",
#'   verbose = TRUE
#' )
#'
#' # Full example with all parameters
#' npi_detailed <- phase0_national_downloadable_file(
#'   npi_numbers = c("1234567890", "0987654321", "1122334455"),
#'   batch_size = 5,
#'   url = "https://data.cms.gov/provider-data/api/1/datastore/query/mj5m-pzi6/0",
#'   limit = 50,
#'   sys_sleep = 1,
#'   user_agent = "my-research-project",
#'   verbose = TRUE,
#'   csv_save_path = "output/cms_data"
#' )
#' }
#'
#' @importFrom dplyr bind_rows rename_with
#' @importFrom logger log_info log_error log_warn
#' @importFrom httr POST accept add_headers content status_code
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom readr write_csv
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @export
phase0_national_downloadable_file <- function(
    npi_numbers,
    batch_size = 10,
    url = "https://data.cms.gov/provider-data/api/1/datastore/query/mj5m-pzi6/0",
    limit = 100,
    sys_sleep = 0.5,
    user_agent = "r-package-cms-query",
    verbose = TRUE,
    csv_save_path = NULL
) {
  # Auto-convert npi_numbers to character
  npi_numbers <- as.character(npi_numbers)

  # Input validation
  assert_inputs(npi_numbers, batch_size, url, limit, sys_sleep, user_agent, verbose)

  # Initial logging
  if (verbose) {
    tryCatch({
      logger::log_info("Starting phase0_national_downloadable_file function...")
      logger::log_info(glue::glue(
        "Data source: National Downloadable File\n",
        "URL: https://data.cms.gov/provider-data/dataset/mj5m-pzi6"
      ))
      logger::log_info(glue::glue(
        "Documentation: CMS Data Dictionary\n",
        "URL: https://data.cms.gov/provider-data/sites/default/files/",
        "data_dictionaries/physician/DOC_Data_Dictionary.pdf"
      ))
      logger::log_info(glue::glue(
        "Query parameters:\n",
        "- Total NPIs to query: {length(npi_numbers)}\n",
        "- API URL: {url}"
      ))
    }, error = function(e) {
      warning("Failed to log initialization: ", e$message)
    })
  }

  # Process batches
  all_records <- process_batches(
    npi_numbers = npi_numbers,
    batch_size = batch_size,
    url = url,
    limit = limit,
    sys_sleep = sys_sleep,
    user_agent = user_agent,
    verbose = verbose
  )

  # Combine all records
  if (length(all_records) > 0) {
    combined_records <- dplyr::bind_rows(all_records)

    # Completion logging
    if (verbose) {
      tryCatch({
        logger::log_info(glue::glue("Total rows retrieved: {nrow(combined_records)}"))
        logger::log_info("Function phase0_national_downloadable_file completed successfully.")
      }, error = function(e) {
        warning("Failed to log completion: ", e$message)
      })
    }

    # Save to CSV if path provided
    save_records_to_csv(combined_records, csv_save_path, verbose)

    # Add prefix to column names
    if (nrow(combined_records) > 0) {
      combined_records <- combined_records %>%
        dplyr::rename_with(~ paste0("NDF_", .))
    }

    return(combined_records)
  } else {
    if (verbose) logger::log_warn("No records found for provided NPIs")
    return(tibble::tibble())
  }
}

#' @noRd
process_batches <- function(npi_numbers, batch_size, url, limit, sys_sleep,
                            user_agent, verbose) {
  npi_batches <- split(npi_numbers, ceiling(seq_along(npi_numbers) / batch_size))
  all_records <- list()

  for (batch_idx in seq_along(npi_batches)) {
    current_batch <- npi_batches[[batch_idx]]
    if (verbose) {
      logger::log_info(glue::glue(
        "Processing batch {batch_idx} with NPIs: {paste(current_batch, collapse = ', ')}"
      ))
    }

    batch_records <- tryCatch({
      fetch_batch(
        current_batch, url, limit, sys_sleep, user_agent, verbose
      )
    }, error = function(e) {
      logger::log_error(glue::glue("Failed to process batch {batch_idx}: {e$message}"))
      return(NULL)
    })

    if (!is.null(batch_records)) {
      all_records[[batch_idx]] <- batch_records
    }
  }

  return(all_records)
}

#' @noRd
fetch_batch <- function(npi_batch, url, limit, sys_sleep, user_agent, verbose) {
  offset <- 0
  keep_querying <- TRUE
  batch_records <- list()

  while (keep_querying) {
    body <- list(
      conditions = list(
        list(
          property = "npi",
          value = npi_batch,
          operator = "IN"
        )
      ),
      limit = limit,
      offset = offset,
      results = TRUE
    )

    json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    if (verbose) {
      logger::log_info(glue::glue("Sending request for batch with offset {offset}"))
    }

    response <- tryCatch({
      httr::POST(
        url,
        body = json_body,
        encode = "json",
        httr::accept("application/json"),
        httr::add_headers(`User-Agent` = user_agent)
      )
    }, error = function(e) {
      logger::log_error(glue::glue("API request failed: {e$message}"))
      stop(glue::glue("Failed to connect to CMS API: {e$message}"))
    })

    if (httr::status_code(response) == 200) {
      data_content <- httr::content(response, "text", encoding = "UTF-8")
      cms_data <- jsonlite::fromJSON(data_content, flatten = TRUE)

      if (!is.null(cms_data$results) && length(cms_data$results) > 0) {
        batch_data <- as.data.frame(cms_data$results)
        batch_records[[length(batch_records) + 1]] <- batch_data
        if (verbose) {
          logger::log_info(glue::glue(
            "Fetched {nrow(batch_data)} rows for batch with offset {offset}"
          ))
        }

        if (nrow(batch_data) < limit) {
          keep_querying <- FALSE
        } else {
          offset <- offset + limit
          Sys.sleep(sys_sleep)
        }
      } else {
        if (verbose) {
          logger::log_info(glue::glue(
            "No more results for batch with offset {offset}."
          ))
        }
        break
      }
    } else {
      logger::log_error(glue::glue(
        "Failed to retrieve data. HTTP Status: {httr::status_code(response)}"
      ))
      stop(glue::glue("Failed to retrieve data from CMS API."))
    }
  }

  if (length(batch_records) > 0) {
    return(dplyr::bind_rows(batch_records))
  } else {
    return(NULL)
  }
}

#' @noRd
save_records_to_csv <- function(combined_records, csv_save_path, verbose) {
  if (!is.null(csv_save_path) && nrow(combined_records) > 0) {
    tryCatch({
      dir.create(csv_save_path, showWarnings = FALSE, recursive = TRUE)
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      file_path <- file.path(
        csv_save_path,
        paste0("cms_npi_data_", timestamp, ".csv")
      )
      readr::write_csv(combined_records, file_path)
      if (verbose) logger::log_info(glue::glue("Data saved to: {file_path}"))
    }, error = function(e) {
      logger::log_error(glue::glue("Failed to save CSV: {e$message}"))
      stop(e)
    })
  }
}

#' @noRd
assert_inputs <- function(npi_numbers, batch_size, url, limit, sys_sleep,
                          user_agent, verbose) {
  assertthat::assert_that(
    is.character(npi_numbers),
    msg = "`npi_numbers` must be a character vector."
  )
  assertthat::assert_that(
    length(npi_numbers) > 0,
    msg = "`npi_numbers` must contain at least one NPI."
  )
  assertthat::assert_that(
    is.numeric(batch_size) && batch_size > 0,
    msg = "`batch_size` must be a positive number."
  )
  assertthat::assert_that(
    is.character(url),
    msg = "`url` must be a character value."
  )
  assertthat::assert_that(
    is.numeric(limit) && limit > 0,
    msg = "`limit` must be a positive number."
  )
  assertthat::assert_that(
    is.numeric(sys_sleep) && sys_sleep >= 0,
    msg = "`sys_sleep` must be a non-negative number."
  )
  assertthat::assert_that(
    is.character(user_agent) && nzchar(user_agent),
    msg = "`user_agent` must be a non-empty string."
  )
  assertthat::assert_that(
    is.logical(verbose),
    msg = "`verbose` must be a logical value."
  )
}
