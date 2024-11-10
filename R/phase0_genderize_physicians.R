#' Genderize Physicians Data with Logging and Error Handling
#'
#' This function reads a CSV file containing physician data, genderizes the first names using multiple methods,
#' and merges the gender information back to the original data. The process is logged in detail, including the
#' inputs, transformations, outputs, and any errors encountered. The resulting dataset is saved to a new CSV file
#' in the specified output directory, with a timestamp appended to the filename for uniqueness.
#'
#' @param input_csv A string representing the file path to the input CSV file containing physician data.
#'                  This CSV file must contain a column named 'first_name' for genderization to proceed.
#' @param output_dir A string representing the directory where the output CSV file will be saved.
#'                   If no directory is provided, the current working directory is used.
#' @return A tibble with genderized information joined to the original data. The output file is also saved in the specified output directory.
#' @importFrom gender gender
#' @importFrom dplyr select rename distinct left_join mutate
#' @importFrom readr read_csv
#' @importFrom logger log_info log_error
#' @importFrom glue glue
#' @export
#'
#' @examples
#' # Example 1: Basic usage with the default output directory
#' \dontrun{
#' # Assumes "physicians_data.csv" exists with a 'first_name' column
#' result <- phase0_genderize_physicians("physicians_data.csv")
#' # Output saved in the current working directory as genderized_<timestamp>_physicians_data.csv
#' # Returns a tibble with the original data plus a 'gender' column based on 'first_name'
#' print(result)
#' }
#'
#' # Example 2: Custom output directory for saving the result
#' \dontrun{
#' # Assumes "physicians_data.csv" exists with a 'first_name' column
#' result <- phase0_genderize_physicians("physicians_data.csv", output_dir = "output_directory/")
#' # Output saved in "output_directory" as genderized_<timestamp>_physicians_data.csv
#' # Returns a tibble with original data plus 'gender' information
#' print(result)
#' }
#'
#' # Example 3: Handling missing 'first_name' gracefully
#' \dontrun{
#' # Assumes "physicians_data_missing_names.csv" contains missing values in the 'first_name' column
#' result <- phase0_genderize_physicians("physicians_data_missing_names.csv")
#' # Returns a tibble with original data plus a 'gender' column. Rows with missing first names are retained
#' # but may have NA in the 'gender' column.
#' print(result)
#' }
#'
#' # Example 4: Using a CSV without the required 'first_name' column
#' \dontrun{
#' # Assumes "invalid_data.csv" does not contain a 'first_name' column
#' result <- phase0_genderize_physicians("invalid_data.csv")
#' # Error: Logs an error and stops the function as 'first_name' column is missing.
#' }
#'
#' # Example 5: Saving the output in a different directory with a custom name
#' \dontrun{
#' # Assumes "physicians_data.csv" exists with a 'first_name' column
#' result <- phase0_genderize_physicians("physicians_data.csv", output_dir = "custom_output/")
#' # Output saved in "custom_output/" with a filename like genderized_<timestamp>_physicians_data.csv
#' # Returns a tibble with original data plus 'gender' information
#' print(result)
#' }

phase0_genderize_physicians <- function(input_csv, output_dir = getwd()) {
  # Input validation
  if (!file.exists(input_csv)) {
    logger::log_error("Input CSV file does not exist: {input_csv}")
    stop(glue::glue("Input CSV file not found: {input_csv}"))
  }

  if (!dir.exists(output_dir)) {
    logger::log_error("Output directory does not exist: {output_dir}")
    stop(glue::glue("Output directory not found: {output_dir}"))
  }

  # Log the start of the function and inputs
  logger::log_info("Starting genderize_physicians function...")
  logger::log_info("Input CSV: {input_csv}")
  logger::log_info("Output directory: {output_dir}")

  # Read the input data
  physician_data <- tryCatch({
    readr::read_csv(input_csv, show_col_types = FALSE)
  }, error = function(e) {
    logger::log_error("Error reading the input CSV: {e$message}")
    stop("Failed to read the input CSV.")
  })

  # Check if the first_name column exists
  if (!"first_name" %in% names(physician_data)) {
    logger::log_error("Missing 'first_name' column in the input data.")
    stop("The input CSV must contain a 'first_name' column.")
  }

  # Genderize the first names
  logger::log_info("Genderizing first names...")
  gender_data <- tryCatch({
    gender::gender(
      names = physician_data$first_name,
      years = c(1932, 2012),
      method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"),
      countries = "United States"
    ) %>%
      dplyr::rename(first_name = name) %>%
      dplyr::distinct(first_name, .keep_all = TRUE) %>%
      dplyr::select(first_name, gender)
  }, error = function(e) {
    logger::log_error("Error during genderizing process: {e$message}")
    stop("Failed to genderize the first names.")
  })

  # Join the gender data back to the original dataset
  logger::log_info("Joining gender data with the original dataset...")
  joined_data <- dplyr::left_join(physician_data, gender_data, by = "first_name")

  # Check for missing genders after the join
  missing_genders <- sum(is.na(joined_data$gender))
  logger::log_info("Number of physicians with missing gender after joining: {missing_genders}")

  # Generate a timestamp for the output filename
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  output_file <- file.path(output_dir, glue::glue("genderized_{timestamp}_{basename(input_csv)}"))

  # Write the output CSV using write_output_csv
  tryCatch({
    write_output_csv(joined_data, filename = basename(output_file), output_dir = dirname(output_file), verbose = TRUE)
  }, error = function(e) {
    logger::log_error("Error writing the output CSV: {e$message}")
    stop("Failed to save the output CSV.")
  })

  # Log the output details
  logger::log_info("Output saved to: {output_file}")
  logger::log_info("Genderizing process completed successfully.")

  # Return the joined data
  return(joined_data)
}
