#' Sample Spine Surgeons Across Insurance Scenarios (Detailed Logging)
#'
#' This function expands a dataset of physicians by assigning multiple insurance types to each physician
#' while retaining all original columns. It logs inputs, outputs, data transformations, and file paths
#' to the console using the `logger` package.
#'
#' @param data A dataframe containing physician information. Must include columns for specialty, NPI,
#'        and phone number.
#' @param insurance_types A character vector of insurance types to assign to each physician. Default is
#'        `c("Medicare", "Medicaid", "BCBS")`.
#' @param ensure_unique_phone_numbers Logical indicating whether to ensure unique phone numbers among
#'        physicians before expansion. Default is `TRUE`.
#' @param output_csv_path Directory to save the output CSV file. If `NULL`, no file is saved. Default
#'        is `NULL`.
#' @param seed An integer to set the random seed for reproducibility. Default is `1978`.
#' @param specialty_column The name of the column representing the specialty. Default is `"specialty_primary"`.
#' @param npi_column The name of the column representing the NPI. Default is `"NPI"`.
#'
#' @return A dataframe with expanded rows, including one for each physician-insurance combination.
#'
#' @importFrom dplyr distinct
#' @importFrom tidyr expand_grid
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' physicians <- data.frame(
#'   specialty_primary = c("Orthopedics", "Cardiology"),
#'   NPI = c("1234567890", "9876543210"),
#'   phone_number = c("123-456-7890", "098-765-4321")
#' )
#' expanded_data <- phase2_sample_surgeons(
#'   data = physicians
#' )
#' print(expanded_data)
#'
#' # Example 2: Custom insurance types and saving output
#' expanded_data <- phase2_sample_surgeons(
#'   data = physicians,
#'   insurance_types = c("Private", "Public"),
#'   output_csv_path = "output/"
#' )
#' print(expanded_data)
#'
#' # Example 3: Ensuring unique phone numbers and using a custom seed
#' physicians <- data.frame(
#'   specialty_primary = c("Orthopedics", "Cardiology", "Orthopedics"),
#'   NPI = c("1234567890", "9876543210", "1234567890"),
#'   phone_number = c("123-456-7890", "098-765-4321", "123-456-7890")
#' )
#' expanded_data <- phase2_sample_surgeons(
#'   data = physicians,
#'   ensure_unique_phone_numbers = TRUE,
#'   seed = 42
#' )
#' print(expanded_data)
#'
#' @export
phase2_sample_surgeons <- function(data,
                                   insurance_types = c("Medicare", "Medicaid", "BCBS"),
                                   ensure_unique_phone_numbers = TRUE,
                                   output_csv_path = NULL,
                                   seed = 1978,
                                   specialty_column = "specialty_primary",
                                   npi_column = "NPI") {
  # Log function inputs
  logger::log_info("Starting phase2_sample_surgeons() with inputs:")
  logger::log_info("Insurance types: {insurance_types}")
  logger::log_info("Ensure unique phone numbers: {ensure_unique_phone_numbers}")
  logger::log_info("Output CSV path: {output_csv_path}")
  logger::log_info("Seed: {seed}")
  logger::log_info("Specialty column: {specialty_column}")
  logger::log_info("NPI column: {npi_column}")

  # Set seed for reproducibility
  set.seed(seed)
  logger::log_info("Seed set to {seed}.")

  # Validate inputs
  validate_input_data(data, specialty_column, npi_column)

  # Ensure unique phone numbers if specified
  if (ensure_unique_phone_numbers) {
    logger::log_info("Ensuring unique phone numbers in the dataset.")
    data <- dplyr::distinct(data, phone_number, .keep_all = TRUE)
    logger::log_info("Number of rows after ensuring unique phone numbers: {nrow(data)}.")
  }

  # Expand physicians by insurance types
  logger::log_info("Expanding physicians by insurance types...")
  expanded_data <- expand_by_insurance(data, insurance_types)
  logger::log_info("Expanded data includes insurance types. Total rows: {nrow(expanded_data)}.")

  # Save data if output path is specified
  if (!is.null(output_csv_path)) {
    output_file <- prepare_output_file(output_csv_path)
    save_to_csv(expanded_data, output_file)
  }

  # Log completion
  logger::log_info("Completed phase2_sample_surgeons(). Returning expanded data.")
  return(expanded_data)
}

# Helper Functions

#' @noRd
validate_input_data <- function(data, specialty_column, npi_column) {
  if (!is.data.frame(data)) {
    logger::log_error("The input 'data' must be a dataframe.")
    stop("The input 'data' must be a dataframe.")
  }

  required_columns <- c(specialty_column, npi_column, "phone_number")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    logger::log_error("Missing columns: {paste(missing_columns, collapse = ', ')}.")
    stop("Missing required columns in the dataset.")
  }
}

#' @noRd
expand_by_insurance <- function(data, insurance_types) {
  tidyr::expand_grid(data, insurance = insurance_types)
}

#' @noRd
prepare_output_file <- function(output_csv_path) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  if (!dir.exists(output_csv_path)) {
    dir.create(output_csv_path, recursive = TRUE)
    logger::log_info("Created output directory: {output_csv_path}.")
  }
  file.path(output_csv_path, paste0("phase2_sample_surgeons_by_insurance_", timestamp, ".csv"))
}

#' @noRd
save_to_csv <- function(data, file_path) {
  tryCatch(
    {
      readr::write_csv(data, file_path)
      logger::log_info("Saved expanded data to: {file_path}.")
    },
    error = function(e) {
      logger::log_error("Error saving expanded data: {e$message}")
      stop("Error saving expanded data: ", e$message)
    }
  )
}
