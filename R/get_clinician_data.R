#' Validate and Retrieve Clinician Data by NPI
#'
#' This function validates a set of NPI numbers, removes invalid or missing entries, and retrieves detailed
#' clinician information for valid NPIs. It supports both dataframe and CSV file inputs and ensures a clean
#' and validated output with clinician data.
#'
#' @param npi_data Either a dataframe containing NPI numbers (must include a column named `npi`) or a
#'   path to a CSV file with NPI numbers. The column `npi` must contain numeric or character representations
#'   of NPIs with exactly 10 digits.
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
#' clinician_data <- validate_and_retrieve_clinician_data(npi_data_frame)
#' print(clinician_data)
#' }
#'
#' # Example 2: Validate and retrieve clinician data from a CSV file
#' \dontrun{
#' npi_csv_path <- "npi_data.csv"
#' npi_data_frame <- tibble::tibble(npi = c("1234567890", "1689603763", "invalid_npi"))
#' readr::write_csv(npi_data_frame, npi_csv_path)
#' clinician_data <- validate_and_retrieve_clinician_data(npi_csv_path)
#' print(clinician_data)
#' }
#'
#' # Example 3: Handle a large dataset of NPIs
#' \dontrun{
#' large_npi_data <- tibble::tibble(npi = sprintf("%010d", sample(1e9, 1000)))
#' clinician_data <- validate_and_retrieve_clinician_data(large_npi_data)
#' print(dim(clinician_data))
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate
#' @importFrom readr read_csv write_csv
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map
#' @importFrom provider clinicians
#' @export
validate_and_retrieve_clinician_data <- function(npi_data) {
  # Validate input type
  assertthat::assert_that(
    is.data.frame(npi_data) || (is.character(npi_data) && file.exists(npi_data)),
    msg = "`npi_data` must be a dataframe or a valid file path to a CSV."
  )

  # Load data
  if (is.data.frame(npi_data)) {
    clinician_input <- npi_data
  } else {
    clinician_input <- readr::read_csv(npi_data, col_types = readr::cols(npi = readr::col_character()))
  }

  # Validate the existence of the 'npi' column
  assertthat::assert_that(
    "npi" %in% colnames(clinician_input),
    msg = "The input must contain a column named `npi`."
  )

  # Filter and validate NPI numbers
  valid_npi_data <- clinician_input %>%
    dplyr::filter(!is.na(npi) & nchar(as.character(npi)) == 10 & grepl("^[0-9]{10}$", as.character(npi)))

  assertthat::assert_that(
    nrow(valid_npi_data) > 0,
    msg = "No valid NPIs found after filtering. Check the input data."
  )

  # Retrieve clinician information for valid NPIs
  clinician_data <- valid_npi_data %>%
    dplyr::mutate(clinician_info = purrr::map(npi, ~ {
      provider::clinicians(npi = .x)
    })) %>%
    tidyr::unnest_wider(clinician_info)

  return(clinician_data)
}
