#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_data Either a dataframe containing NPI numbers or a path to a CSV file.
#'
#' @return A dataframe containing valid NPI numbers.
#' @importFrom npi npi_is_valid
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate
#' @export
#'
validate_and_remove_invalid_npi <- function(input_data) {

  cat("Starting validate_and_remove_invalid_npi...\n")

  if (is.data.frame(input_data)) {
    cat("Input is a data frame.\n")
    df <- input_data
  } else if (is.character(input_data)) {
    cat("Input is a file path to a CSV.\n")
    df <- readr::read_csv(input_data, col_types = readr::cols(npi = readr::col_character()))
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  cat("Initial dataframe:\n")
  print(df)

  df <- df %>%
    dplyr::filter(!is.na(npi) & npi != "")

  cat("After filtering missing or empty NPIs:\n")
  print(df)

  df <- df %>%
    dplyr::mutate(npi_is_valid = sapply(npi, function(x) {
      if (nchar(x) == 10) {
        npi::npi_is_valid(x)
      } else {
        FALSE
      }
    })) %>%
    dplyr::filter(!is.na(npi_is_valid) & npi_is_valid)

  cat("After filtering invalid NPIs:\n")
  print(df)
  cat("validate_and_remove_invalid_npi completed.\n")

  return(df)
}


#' Retrieve Clinician Data
#'
#' Retrieves clinician data for each valid NPI from a dataframe or CSV file. This
#' function ensures valid NPI numbers are processed and retrieves detailed
#' clinician information.
#'
#' @param input_data Either a dataframe containing NPI numbers (must include a column named `npi`) or a path to a CSV file with NPI numbers.
#' @return A tibble with clinician data for each valid NPI.
#'
#' @details
#' - The function checks the input data for validity and removes invalid NPI numbers.
#' - If a CSV file path is provided, the file is read, and the data is processed.
#'
#' @examples
#' # Example 1: Provide a dataframe with NPI numbers
#' sample_data <- tibble::tibble(npi = c(1689603763, 1234567890))
#' clinician_data <- retrieve_clinician_data(sample_data)
#' print(clinician_data)
#'
#' # Example 2: Provide a CSV file path
#' write.csv(sample_data, "npi_data.csv", row.names = FALSE)
#' clinician_data <- retrieve_clinician_data("npi_data.csv")
#' print(clinician_data)
#'
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tidyr unnest_wider
#' @importFrom provider clinicians
#' @importFrom dplyr mutate
#' @importFrom logger log_info
#' @export
retrieve_clinician_data <- function(input_data) {
  logger::log_info("Starting retrieve_clinician_data with input_data of class {class(input_data)}.")

  # Load and validate input
  clinician_data_tbl <- load_and_validate_input(input_data)
  logger::log_info("Loaded input data with {nrow(clinician_data_tbl)} rows.")

  # Validate and remove invalid NPI numbers
  clinician_data_tbl <- validate_and_remove_invalid_npi(clinician_data_tbl)
  logger::log_info("Validated NPI numbers. Remaining rows: {nrow(clinician_data_tbl)}.")

  # Retrieve clinician information
  clinician_data_tbl <- retrieve_clinician_info(clinician_data_tbl)
  logger::log_info("Retrieved clinician information. Final dataframe has {nrow(clinician_data_tbl)} rows and {ncol(clinician_data_tbl)} columns.")

  return(clinician_data_tbl)
}

# Helper Functions --------------------------------------------------------

#' @noRd
load_and_validate_input <- function(input_data) {
  if (is.data.frame(input_data)) {
    logger::log_info("Input data provided as a dataframe with {nrow(input_data)} rows.")
    return(input_data)
  } else if (is.character(input_data)) {
    data_tbl <- readr::read_csv(input_data)
    logger::log_info("Input data loaded from file path: {input_data} with {nrow(data_tbl)} rows.")
    return(data_tbl)
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }
}

#' @noRd
validate_and_remove_invalid_npi <- function(tbl) {
  logger::log_info("Starting validation of NPI numbers.")
  valid_tbl <- tbl %>%
    dplyr::filter(!is.na(npi) & nchar(as.character(npi)) == 10 & grepl("^[0-9]{10}$", as.character(npi)))

  logger::log_info("NPI validation complete. {nrow(valid_tbl)} valid NPIs retained out of {nrow(tbl)} rows.")
  return(valid_tbl)
}

#' @noRd
retrieve_clinician_info <- function(tbl) {
  logger::log_info("Mapping clinician data retrieval over each NPI.")
  tbl %>%
    dplyr::mutate(clinician_data = purrr::map(npi, fetch_clinician_data)) %>%
    tidyr::unnest_wider(clinician_data)
}

#' @noRd
fetch_clinician_data <- function(npi) {
  if (!is.numeric(npi) || nchar(as.character(npi)) != 10) {
    logger::log_info("Invalid NPI skipped: {npi}")
    return(NULL)
  }

  clinician_info <- provider::clinicians(npi = npi)
  if (is.null(clinician_info)) {
    logger::log_info("No results found for NPI: {npi}")
    return(NULL)
  } else {
    logger::log_info("Retrieved clinician data for NPI: {npi}")
    return(clinician_info)
  }
}
