#' Phase 0: Retrieve Clinician Data by NPI
#'
#' This function validates a set of NPI numbers, removes invalid or missing entries, and retrieves detailed
#' clinician information for valid NPIs. It supports both dataframe and CSV file inputs and ensures a clean
#' and validated output with clinician data.
#'
#' @param npi_data Either a dataframe containing NPI numbers (must include a column named `npi`) or a
#'   path to a CSV file with NPI numbers. The column `npi` must contain numeric or character representations
#'   of NPIs with exactly 10 digits.
#' @param verbose A logical value that controls whether detailed logging is shown during the process.
#'
#' @return A tibble with detailed clinician data for each valid NPI, including expanded metadata columns.
#'
#' @details
#' This function first validates the input data to ensure it contains correctly formatted NPIs. Missing or invalid
#' NPIs are removed. Then, for each valid NPI, the function retrieves clinician data using the `provider` package.
#'
#' @examples
#' # Example 1: Validate and retrieve clinician data from a dataframe
#' \dontrun{
#' npi_data_frame <- tibble::tibble(npi = c("1234567890", "1689603763", "invalid_npi", NA))
#' clinician_data <- phase0_retrieve_clinician_data(npi_data_frame, verbose = TRUE)
#' print(clinician_data)
#' }
#'
#' # Example 2: Validate and retrieve clinician data from a CSV file
#' \dontrun{
#' npi_csv_path <- "npi_data.csv"
#' npi_data_frame <- tibble::tibble(npi = c("1234567890", "1689603763", "invalid_npi"))
#' readr::write_csv(npi_data_frame, npi_csv_path)
#' clinician_data <- phase0_retrieve_clinician_data(npi_csv_path, verbose = TRUE)
#' print(clinician_data)
#' }
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr filter mutate select
#' @importFrom readr read_csv write_csv
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map
#' @importFrom provider clinicians
#' @importFrom logger log_info
#' @export
phase0_retrieve_clinician_data <- function(npi_data, verbose = TRUE) {

  # Step 1: Validate input data
  npi_data <- validate_input_data(npi_data, verbose)

  # Step 2: Filter and validate NPIs
  valid_npi_data <- filter_and_validate_npis(npi_data, verbose)

  # Step 3: Retrieve clinician data for valid NPIs
  clinician_data <- retrieve_clinician_data(valid_npi_data, verbose)

  # Step 4: Keep rows with NA NPI values, clean the clinician_info columns, and update required fields
  clinician_data_cleaned <- clinician_data %>%
    dplyr::mutate(across(starts_with("clinician_info_"), ~ purrr::map_chr(.x, clean_column_unwrap))) %>%
    dplyr::bind_rows(dplyr::filter(npi_data, is.na(npi))) %>%
    dplyr::mutate(
      middle = dplyr::coalesce(middle, basic_middle_name),

      # Handle the existence of clinician_info_gender to avoid missing columns
      basic_gender = dplyr::coalesce(basic_gender,
                                     ifelse("clinician_info_gender" %in% names(clinician_data), clinician_info_gender, NA)),

      addresses_address_1 = dplyr::coalesce(addresses_address_1, clinician_info_address_org),
      addresses_city = dplyr::coalesce(addresses_city, clinician_info_city_org),
      taxonomies_state = dplyr::coalesce(taxonomies_state, clinician_info_state_org),

      addresses_postal_code = dplyr::coalesce(addresses_postal_code,
                                              as.character(clinician_info_zip_org) %>%
                                                stringr::str_sub(1, 5)),

      addresses_telephone_number = dplyr::coalesce(addresses_telephone_number,
                                                   as.character(clinician_info_phone_org)),
      honorrific = dplyr::coalesce(honorrific, clinician_info_credential)
    ) %>%
    # Now drop unnecessary columns to clean up the dataframe
    dplyr::select(-clinician_info_npi, -clinician_info_pac, -clinician_info_enid,
                  -clinician_info_first, -clinician_info_last, -clinician_info_pac_org,
                  -clinician_info_middle, -clinician_info_gender, -clinician_info_credential,
                  -clinician_info_specialty, -clinician_info_address_org, -clinician_info_city_org,
                  -clinician_info_state_org, -clinician_info_zip_org, -clinician_info_phone_org)

  # Log source information (wrapped for readability)
  logger::log_info("This ONLY includes people who take Medicare!!!!!!")
  logger::log_info("Source: National Downloadable File. The Doctors and Clinicians")
  logger::log_info("national downloadable file is organized such that each line is")
  logger::log_info("unique at the clinician/enrollment record/group/address level. Clinicians")
  logger::log_info("with multiple Medicare enrollment records and/or single enrollments")
  logger::log_info("linking to multiple practice locations are listed on multiple lines.")

  # Step 5: Return the cleaned clinician data
  return(clinician_data_cleaned)

  # Helper function to clean column values (unwrap the list)
  clean_column_unwrap <- function(x) {
    # If the list has one element, return it directly
    if (length(x) > 0) {
      return(as.character(x[[1]]))  # Explicitly coerce to character
    }
    return(NA)  # Return NA if the list is empty
  }
}

#' Validate input data
#'
#' @param npi_data The input data (dataframe or CSV path).
#' @param verbose Whether to log detailed information.
#'
#' @return A validated dataframe.
#' @noRd
validate_input_data <- function(npi_data, verbose) {
  logger::log_info("Validating input data type...")
  assertthat::assert_that(
    base::is.data.frame(npi_data) || (base::is.character(npi_data) && file.exists(npi_data)),
    msg = "`npi_data` must be a dataframe or a valid file path to a CSV."
  )

  # Load data
  if (is.data.frame(npi_data)) {
    clinician_input <- npi_data
    logger::log_info("Data loaded from dataframe.")
  } else {
    clinician_input <- readr::read_csv(npi_data, col_types = readr::cols(npi = readr::col_character()))
    logger::log_info("Data loaded from CSV file: %s", npi_data)
  }

  # Validate 'npi' column existence
  assertthat::assert_that(
    "npi" %in% colnames(clinician_input),
    msg = "The input must contain a column named `npi`."
  )

  logger::log_info("Validated 'npi' column exists in input data.")
  return(clinician_input)
}

#' Filter and validate NPIs
#'
#' @param npi_data The dataframe with NPI data.
#' @param verbose Whether to log detailed information.
#'
#' @return A dataframe containing only valid NPIs.
#' @noRd
filter_and_validate_npis <- function(npi_data, verbose) {
  logger::log_info("Filtering valid NPIs...")
  valid_npi_data <- npi_data %>%
    dplyr::filter(!is.na(npi) & nchar(as.character(npi)) == 10 & grepl("^[0-9]{10}$", as.character(npi)))

  assertthat::assert_that(
    nrow(valid_npi_data) > 0,
    msg = "No valid NPIs found after filtering. Check the input data."
  )

  logger::log_info("After filtering, valid NPIs: %d", nrow(valid_npi_data))
  return(valid_npi_data)
}

#' Retrieve clinician data for valid NPIs
#'
#' @param valid_npi_data The dataframe containing valid NPIs.
#' @param verbose Whether to log detailed information.
#'
#' @return A dataframe containing clinician data.
#' @noRd
retrieve_clinician_data <- function(valid_npi_data, verbose) {
  logger::log_info("Retrieving clinician data for valid NPIs...")
  clinician_data <- valid_npi_data %>%
    dplyr::mutate(clinician_info = purrr::map(npi, ~ {
      provider::clinicians(npi = .x)
    })) %>%
    tidyr::unnest_wider(clinician_info, names_sep = "_")  # Use names_sep to avoid duplication of 'npi'

  logger::log_info("Clinician data retrieval complete. Rows in final data: %d", nrow(clinician_data))
  return(clinician_data)
}
