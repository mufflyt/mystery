#' Genderize Physicians Data with Logging and Error Handling
#'
#' This function reads a CSV file containing physician data, genderizes the
#' first names using multiple methods,
#' and merges the gender information back to the original data. The process is
#' logged in detail, including the
#' inputs, transformations, outputs, and any errors encountered. The resulting
#' dataset is saved to a new CSV file
#' in the specified output directory, with a timestamp appended to the
#' filename for uniqueness.
#'
#' @param input_csv A string representing the file path to the input CSV file
#' containing physician data.
#'                  This CSV file must contain a column named 'first_name'
#'                  for genderization to proceed.
#' @param output_dir A string representing the directory where the output
#' CSV file will be saved.
#'                   If no directory is provided,
#'                   the current working directory is used.
#' @return A tibble with genderized information joined to the original data.
#' The output file is also saved in the specified output directory.
#' @importFrom gender gender
#' @import genderdata
#' @importFrom dplyr select rename distinct left_join mutate
#' @importFrom readr read_csv
#' @importFrom logger log_info log_error
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with the default output directory
#' result <- phase0_genderize_physicians("physicians_data.csv")
#'
#' # Example 2: Custom output directory for saving the result
#' result <- phase0_genderize_physicians("physicians_data.csv",
#'   output_dir = "output_directory/"
#' )
#'
#' # Example 3: Handling missing 'first_name' gracefully
#' result <- phase0_genderize_physicians("physicians_data_missing_names.csv")
#' }
phase0_genderize_physicians <- function(input_csv, output_dir = getwd()) {
  # Validate input_csv file
  if (!file.exists(input_csv)) {
    logger::log_error(glue::glue("Input CSV file not found: {input_csv}"))
    stop(glue::glue("Input CSV file does not exist: {input_csv}"))
  }

  # Validate output_dir directory
  if (!dir.exists(output_dir)) {
    logger::log_error(glue::glue("Output directory not found: {output_dir}"))
    stop(glue::glue("Output directory does not exist: {output_dir}"))
  }

  # Log function start
  logger::log_info(glue::glue("Starting phase0_genderize_physicians function with input CSV: {input_csv} and output directory: {output_dir}"))

  # Read input data
  physician_data <- tryCatch(
    {
      readr::read_csv(input_csv, show_col_types = FALSE)
    },
    error = function(e) {
      logger::log_error(glue::glue("Error reading input CSV: {e$message}"))
      stop("Failed to read the input CSV file.")
    }
  )

  # Ensure 'first_name' column exists
  if (!"first_name" %in% names(physician_data)) {
    logger::log_error("The input data does not contain a 'first_name' column.")
    stop("Input data must contain a 'first_name' column.")
  }

  # Genderize first names
  logger::log_info("Genderizing first names...")
  gender_data <- tryCatch(
    {
      gender::gender(
        names = physician_data$first_name,
        years = c(1932, 2012),
        method = "ssa"
      ) %>%
        dplyr::rename(first_name = name) %>%
        dplyr::distinct(first_name, .keep_all = TRUE) %>%
        dplyr::select(first_name, gender)
    },
    error = function(e) {
      logger::log_error(glue::glue("Error during genderization: {e$message}"))
      stop("Failed to genderize the first names.")
    }
  )

  # Merge gender data with original data
  logger::log_info("Joining genderized data with the original dataset...")
  joined_data <- dplyr::left_join(physician_data, gender_data, by = "first_name")

  # Log missing genders
  missing_genders <- sum(is.na(joined_data$gender))
  logger::log_info(glue::glue("Number of rows with missing gender: {missing_genders}"))

  # Save output to CSV
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  output_file <- file.path(output_dir, glue::glue("genderized_{timestamp}_{basename(input_csv)}"))
  tryCatch(
    {
      write_output_csv(joined_data, filename = basename(output_file), output_dir = dirname(output_file), verbose = TRUE)
    },
    error = function(e) {
      logger::log_error(glue::glue("Error saving output CSV: {e$message}"))
      stop("Failed to save the output CSV.")
    }
  )

  # Log completion
  logger::log_info(glue::glue("Genderization completed successfully. Output saved to: {output_file}"))

  # Return joined data
  return(joined_data)
}
