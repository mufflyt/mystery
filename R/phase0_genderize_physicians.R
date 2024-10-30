#' Genderize Physicians Data with Logging and Error Handling
#'
#' This function reads a CSV file containing physician data, genderizes the first names,
#' and joins the gender information back to the original data. It logs the entire process,
#' including inputs, transformations, and outputs, and saves the result to a new CSV file with a timestamp.
#'
#' @param input_csv A string representing the path to the input CSV file containing physician data.
#' The CSV should contain a column named 'first_name' for genderization.
#' @param output_dir A string representing the directory where the output CSV file will be saved.
#' The default is the current working directory.
#' @return A tibble with genderized information joined to the original data.
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
#' result <- genderize_physicians("physicians_data.csv")
#' }
#'
#' # Example 2: Custom output directory for saving the result
#' \dontrun{
#' result <- genderize_physicians("physicians_data.csv", output_dir = "output_directory/")
#' }
#'
#' # Example 3: Handling missing first names gracefully
#' \dontrun{
#' result <- genderize_physicians("physicians_data_missing_names.csv")
#' }
#'
#' # Example 4: Using a CSV without the required 'first_name' column
#' \dontrun{
#' result <- genderize_physicians("invalid_data.csv")
#' }
#'
#' # Example 5: Saving the output in a different directory with a custom name
#' \dontrun{
#' result <- genderize_physicians("physicians_data.csv", output_dir = "custom_output/")
#' }

genderize_physicians <- function(input_csv, output_dir = getwd()) {

  # Input validation
  if (!file.exists(input_csv)) {
    log_error("Input CSV file does not exist: {input_csv}")
    stop(glue::glue("Input CSV file not found: {input_csv}"))
  }

  if (!dir.exists(output_dir)) {
    log_error("Output directory does not exist: {output_dir}")
    stop(glue::glue("Output directory not found: {output_dir}"))
  }

  # Log the start of the function and inputs
  log_info("Starting genderize_physicians function...")
  log_info("Input CSV: {input_csv}")
  log_info("Output directory: {output_dir}")

  # Read the input data
  physician_data <- tryCatch({
    readr::read_csv(input_csv, show_col_types = FALSE)
  }, error = function(e) {
    log_error("Error reading the input CSV: {e$message}")
    stop("Failed to read the input CSV.")
  })

  # Check if the first_name column exists
  if (!"first_name" %in% names(physician_data)) {
    log_error("Missing 'first_name' column in the input data.")
    stop("The input CSV must contain a 'first_name' column.")
  }

  # Genderize the first names
  log_info("Genderizing first names...")
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
    log_error("Error during genderizing process: {e$message}")
    stop("Failed to genderize the first names.")
  })

  # Join the gender data back to the original dataset
  log_info("Joining gender data with the original dataset...")
  joined_data <- dplyr::left_join(physician_data, gender_data, by = "first_name")

  # Check for missing genders after the join
  missing_genders <- sum(is.na(joined_data$gender))
  log_info("Number of physicians with missing gender after joining: {missing_genders}")

  # Generate a timestamp for the output filename
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  output_file <- file.path(output_dir, glue::glue("genderized_{timestamp}_{basename(input_csv)}"))

  # Write the output CSV using write_output_csv
  tryCatch({
    write_output_csv(joined_data, filename = basename(output_file), output_dir = dirname(output_file), verbose = TRUE)
  }, error = function(e) {
    log_error("Error writing the output CSV: {e$message}")
    stop("Failed to save the output CSV.")
  })

  # Log the output details
  log_info("Output saved to: {output_file}")
  log_info("Genderizing process completed successfully.")

  # Return the joined data
  return(joined_data)
}
