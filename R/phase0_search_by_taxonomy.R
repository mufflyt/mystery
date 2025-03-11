#' Batch NPI Search by Taxonomy Function
#'
#' This function performs a comprehensive search of the NPPES registry using
#' taxonomy codes, retrieving all matching providers across states. It handles
#' pagination and API limits by breaking searches into state and name-prefix
#' combinations.
#'
#' @param taxonomy_codes Character vector of taxonomy codes to search.
#' @param states Character vector of two-letter state codes to search. If NULL,
#'   searches all states. Default is NULL.
#' @param limit_per_code Integer. Maximum results per request (max 200).
#'   Default 200.
#' @param enumeration_type Character. Filter by provider type: "NPI-1" for
#'   individuals, "NPI-2" for organizations, or NULL for both. Default NULL.
#' @param write_csv_path Character. Optional file path to save results as CSV.
#'   Default NULL.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default TRUE.
#' @param batch_delay Numeric. Delay in seconds between API calls. Default is 1.
#'
#' @return A tibble containing flattened NPI results with provider information.
#'
#' @importFrom dplyr bind_rows select filter mutate
#' @importFrom logger log_info log_error log_warn
#' @importFrom purrr map safely map_dfr
#' @importFrom readr write_csv
#' @importFrom httr GET content http_error status_code add_headers
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' # Search for all OBGYNs across all states
#' all_obgyns <- search_npi_by_taxonomy(
#'   taxonomy_codes = "207V00000X",
#'   states = NULL,
#'   limit_per_code = 200,
#'   enumeration_type = "NPI-1",
#'   write_csv_path = "all_obgyn_providers.csv",
#'   batch_delay = 1
#' )
#'
#' # Search specific states
#' state_obgyns <- search_npi_by_taxonomy(
#'   taxonomy_codes = "207V00000X",
#'   states = c("CA", "NY", "TX"),
#'   limit_per_code = 200,
#'   enumeration_type = "NPI-1",
#'   write_csv_path = "state_obgyn_providers.csv"
#' )
#'
#' # Search multiple taxonomies
#' specialists <- search_npi_by_taxonomy(
#'   taxonomy_codes = c("207V00000X", "207VX0201X"),
#'   states = "CA",
#'   limit_per_code = 200,
#'   enumeration_type = "NPI-1",
#'   write_csv_path = "ca_specialists.csv"
#' )
#' }
#'
#' @export
search_npi_by_taxonomy <- function(taxonomy_codes,
                                   states = NULL,
                                   limit_per_code = 200,
                                   enumeration_type = NULL,
                                   write_csv_path = NULL,
                                   verbose = TRUE,
                                   batch_delay = 1) {

  logger::log_info("Starting NPI taxonomy search...")

  # Validate inputs
  validate_taxonomy_inputs(taxonomy_codes, limit_per_code, enumeration_type)

  # Get state codes if not provided
  state_list <- if (is.null(states)) {
    c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
      "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
      "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
      "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
      "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
      "DC", "PR", "VI", "GU", "AS", "MP")
  } else {
    toupper(states)
  }

  all_results <- list()
  total_providers <- 0

  # Process each taxonomy code
  for (code in taxonomy_codes) {
    logger::log_info("Processing taxonomy code: {code}")

    # Process each state
    for (state in state_list) {
      # Get all results for this state using prefix pagination
      state_results <- paginate_state_search(
        taxonomy_code = code,
        state = state,
        enum_type = enumeration_type,
        limit = limit_per_code,
        batch_delay = batch_delay
      )

      # Process and save state results
      if (length(state_results) > 0) {
        processed_state <- process_taxonomy_results(state_results)

        if (nrow(processed_state) > 0) {
          # Update total count
          total_providers <- total_providers + nrow(processed_state)

          # Append to master results list
          all_results[[length(all_results) + 1]] <- processed_state

          # Optionally save state results
          if (!is.null(write_csv_path)) {
            state_file <- gsub("\\.csv$", paste0("_", state, ".csv"), write_csv_path)
            save_taxonomy_results(processed_state, state_file)
            logger::log_info("Saved {nrow(processed_state)} records for state {state} to {state_file}")
          }
        }
      }
    }
  }

  # Combine all results
  final_results <- if (length(all_results) > 0) {
    dplyr::bind_rows(all_results)
  } else {
    create_empty_results_tibble()
  }

  # Save combined results if path specified
  if (!is.null(write_csv_path) && nrow(final_results) > 0) {
    save_taxonomy_results(final_results, write_csv_path)
    logger::log_info("Saved combined results ({nrow(final_results)} records) to {write_csv_path}")
  }

  logger::log_info("NPI taxonomy search completed successfully.")
  logger::log_info("Final record count: {nrow(final_results)}")

  return(final_results)
}

#' @noRd
paginate_state_search <- function(taxonomy_code, state, enum_type, limit = 200,
                                  batch_delay = 1) {
  # Create two-letter combinations for name filtering
  first_letters <- LETTERS
  second_letters <- c(letters, as.character(0:9))
  name_combinations <- expand.grid(first_letters, second_letters)
  name_filters <- apply(name_combinations, 1, paste0, collapse="")

  state_results <- list()
  total_state_records <- 0

  logger::log_info("Starting search for state: {state}")

  # Search by each two-letter combination
  for (prefix in name_filters) {
    logger::log_info("Searching {state} providers with names starting with '{prefix}'")
    current_page <- 0

    repeat {
      skip_value <- current_page * limit

      # Break if we've hit the API limit for this prefix
      if (skip_value >= 1000) {
        logger::log_warn("Reached API skip limit (1000) for {state} - '{prefix}'")
        break
      }

      # Construct query with state and name filter
      query_params <- construct_taxonomy_query(
        taxonomy_code = taxonomy_code,
        limit = limit,
        enum_type = enum_type,
        skip = skip_value,
        state = state,
        last_name = paste0(prefix, "*")
      )

      # Execute API call
      result <- safely_query_npi_api(query_params)

      # Break if no results or error
      if (is.null(result) || is.null(result$results) ||
          length(result$results) == 0) {
        if (current_page == 0) {
          logger::log_info("No results for {state} - '{prefix}'")
        }
        break
      }

      # Store results
      state_results[[length(state_results) + 1]] <- result

      # Update counters
      records_this_batch <- length(result$results)
      total_state_records <- total_state_records + records_this_batch
      current_page <- current_page + 1

      logger::log_info("Retrieved {records_this_batch} records for {state} - '{prefix}' (page {current_page})")

      # Break if we got less than the limit
      if (records_this_batch < limit) {
        break
      }

      # Add delay between requests
      Sys.sleep(batch_delay)
    }
  }

  logger::log_info("Completed search for state {state}. Total records: {total_state_records}")
  return(state_results)
}

#' @noRd
construct_taxonomy_query <- function(taxonomy_code, limit, enum_type, skip = 0,
                                     state = NULL, last_name = NULL) {
  taxonomy_desc <- get_taxonomy_description(taxonomy_code)

  query <- list(
    version = "2.1",
    taxonomy_description = taxonomy_desc,
    limit = as.character(limit),
    skip = as.character(skip),
    pretty = "true"
  )

  if (!is.null(enum_type)) {
    query$enumeration_type <- enum_type
  }

  if (!is.null(state)) {
    query$state <- state
  }

  if (!is.null(last_name)) {
    query$last_name <- last_name
  }

  return(query)
}

#' @noRd
safely_query_npi_api <- function(query_params) {
  Sys.sleep(0.5)

  base_url <- "https://npiregistry.cms.hhs.gov/api/"

  logger::log_info("Making API request to: {base_url}")
  logger::log_info("With parameters: {jsonlite::toJSON(query_params)}")

  tryCatch({
    response <- httr::GET(
      url = base_url,
      query = query_params,
      httr::add_headers(
        `Accept` = "application/json",
        `User-Agent` = "R-NPI-Search/1.0"
      )
    )

    if (httr::http_error(response)) {
      error_msg <- sprintf(
        "API request failed with status %d",
        httr::status_code(response)
      )
      logger::log_error(error_msg)
      return(NULL)
    }

    parsed <- httr::content(response, "parsed", encoding = "UTF-8")

    if (!is.null(parsed$Errors)) {
      error_msg <- paste(unlist(parsed$Errors), collapse = "; ")
      logger::log_error("API returned errors: {error_msg}")
      return(NULL)
    }

    return(parsed)

  }, error = function(e) {
    logger::log_error("Error in API request: {e$message}")
    return(NULL)
  })
}

#' @noRd
process_taxonomy_results <- function(results_list) {
  if (length(results_list) == 0) {
    logger::log_warn("No results list available")
    return(create_empty_results_tibble())
  }

  # Process each result set
  processed <- purrr::map_dfr(results_list, function(result) {
    if (!is.null(result) && !is.null(result$results) &&
        length(result$results) > 0) {

      # Process each record in the result set
      records <- purrr::map_dfr(result$results, function(record) {
        tryCatch({
          flatten_npi_record(record)
        }, error = function(e) {
          logger::log_error("Error processing record: {e$message}")
          return(NULL)
        })
      })

      return(records)
    }
    return(NULL)
  })

  # Check if we got any results
  if (nrow(processed) == 0) {
    logger::log_warn("No records found after processing")
    return(create_empty_results_tibble())
  }

  # Select and rename columns
  final_processed <- processed |>
    dplyr::select(
      npi = number,
      provider_type = enumeration_type,
      name,
      taxonomy_code,
      taxonomy_desc,
      dplyr::everything()
    )

  return(final_processed)
}

#' @noRd
flatten_npi_record <- function(record) {
  # Basic info
  basic_info <- tibble::tibble(
    number = record$number %||% NA_character_,
    enumeration_type = record$enumeration_type %||% NA_character_
  )

  # Provider info
  if (!is.null(record$basic)) {
    if (!is.null(record$enumeration_type) && record$enumeration_type == "NPI-1") {
      provider_info <- tibble::tibble(
        first_name = record$basic$first_name %||% NA_character_,
        last_name = record$basic$last_name %||% NA_character_,
        name = paste(record$basic$first_name %||% "", record$basic$last_name %||% ""),
        credential = record$basic$credential %||% NA_character_,
        gender = record$basic$gender %||% NA_character_
      )
    } else {
      provider_info <- tibble::tibble(
        first_name = NA_character_,
        last_name = NA_character_,
        name = record$basic$organization_name %||% NA_character_,
        credential = NA_character_,
        gender = NA_character_
      )
    }
  } else {
    provider_info <- tibble::tibble(
      first_name = NA_character_,
      last_name = NA_character_,
      name = NA_character_,
      credential = NA_character_,
      gender = NA_character_
    )
  }

  # Taxonomy info
  taxonomy_info <- if (!is.null(record$taxonomies) && length(record$taxonomies) > 0) {
    tax <- record$taxonomies[[1]]
    tibble::tibble(
      taxonomy_code = tax$code %||% NA_character_,
      taxonomy_desc = tax$desc %||% NA_character_,
      taxonomy_state = tax$state %||% NA_character_,
      taxonomy_license = tax$license %||% NA_character_
    )
  } else {
    tibble::tibble(
      taxonomy_code = NA_character_,
      taxonomy_desc = NA_character_,
      taxonomy_state = NA_character_,
      taxonomy_license = NA_character_
    )
  }

  address_info <- if (!is.null(record$addresses) && length(record$addresses) > 0) {
    addr <- record$addresses[[1]]
    tibble::tibble(
      address_1 = addr$address_1 %||% NA_character_,
      address_2 = addr$address_2 %||% NA_character_,
      city = addr$city %||% NA_character_,
      state = addr$state %||% NA_character_,
      postal_code = addr$postal_code %||% NA_character_,
      country_code = addr$country_code %||% NA_character_,
      address_purpose = addr$address_purpose %||% NA_character_,
      address_type = addr$address_type %||% NA_character_
    )
  } else {
    tibble::tibble(
      address_1 = NA_character_,
      address_2 = NA_character_,
      city = NA_character_,
      state = NA_character_,
      postal_code = NA_character_,
      country_code = NA_character_,
      address_purpose = NA_character_,
      address_type = NA_character_
    )
  }

  dplyr::bind_cols(basic_info, provider_info, taxonomy_info, address_info)
}

#' @noRd
get_taxonomy_description <- function(taxonomy_code) {
  taxonomy_map <- list(
    "207V00000X" = "Obstetrics & Gynecology",
    "207VX0201X" = "Gynecologic Oncology",
    "207VM0101X" = "Maternal & Fetal Medicine",
    "207VE0102X" = "Reproductive Endocrinology",
    "207VF0040X" = "Female Pelvic Medicine and Reconstructive Surgery"
  )

  description <- taxonomy_map[[taxonomy_code]]
  if (is.null(description)) {
    logger::log_warn("Unknown taxonomy code: {taxonomy_code}")
    return(taxonomy_code)
  }
  logger::log_info("Mapped taxonomy code {taxonomy_code} to description: {description}")
  return(description)
}

#' @noRd
create_empty_results_tibble <- function() {
  tibble::tibble(
    npi = character(),
    provider_type = character(),
    name = character(),
    taxonomy_code = character(),
    taxonomy_desc = character(),
    first_name = character(),
    last_name = character(),
    credential = character(),
    gender = character(),
    taxonomy_state = character(),
    taxonomy_license = character(),
    address_1 = character(),
    address_2 = character(),
    city = character(),
    state = character(),
    postal_code = character(),
    country_code = character(),
    address_purpose = character(),
    address_type = character()
  )
}

#' @noRd
save_taxonomy_results <- function(results_df, file_path) {
  logger::log_info("Saving results to: {file_path}")
  readr::write_csv(results_df, file_path)
  logger::log_info("Results saved successfully")
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
validate_taxonomy_inputs <- function(taxonomy_codes, limit_per_code, enumeration_type) {
  assertthat::assert_that(
    is.character(taxonomy_codes),
    all(nchar(taxonomy_codes) == 10),
    is.numeric(limit_per_code),
    limit_per_code > 0,
    limit_per_code <= 200
  )

  if (!is.null(enumeration_type)) {
    assertthat::assert_that(
      enumeration_type %in% c("NPI-1", "NPI-2")
    )
  }
}

# all_obgyns <- search_npi_by_taxonomy(
#   taxonomy_codes = "207XS0117X",
#   states = NULL,
#   limit_per_code = 200,
#   enumeration_type = "NPI-1",
#   write_csv_path = NULL,
#   batch_delay = 1
# )


# Or for smaller fields you can use:
# spine_surg_by_taxonomy <- npi_search(taxonomy_description = "Orthopaedic Surgery of the Spine", #https://taxonomy.nucc.org/
#                                      enumeration_type = "ind")
#
# spine_surg_by_taxonomy_flat <- npi_flatten(spine_surg_by_taxonomy) %>% distinct(npi, .keep_all = TRUE)
