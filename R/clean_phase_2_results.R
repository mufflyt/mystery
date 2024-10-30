#' Clean and process Phase 2 data
#'
#' This function reads data from a file or data frame, cleans column names, and applies renaming based on specified criteria to facilitate data analysis. The function logs each step of the process, including data loading, column cleaning, and renaming for transparency.
#'
#' @param data_or_path Path to the data file or a data frame.
#' @param required_strings Vector of substrings for which to search in column names.
#' @param standard_names Vector of new names to apply to the matched columns.
#' @param output_csv_path Optional. If provided, the cleaned data will be saved to this path.
#' @return A data frame with processed data.
#' @export
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr filter mutate
#' @importFrom janitor clean_names
#' @examples
#' # Example 1: Cleaning data from a CSV file
#' input_path <- "path_to_your_data.csv"
#' required_strings <- c("physician_information", "able_to_contact_office")
#' standard_names <- c("physician_info", "contact_office")
#' cleaned_data <- clean_phase_2_data(input_path, required_strings, standard_names)
#' # cleaned_data will now have updated column names
#'
#' # Example 2: Directly using a data frame
#' df <- data.frame(
#'   doc_info = 1:5,
#'   contact_data = 6:10
#' )
#' required_strings <- c("doc_info", "contact_data")
#' standard_names <- c("doctor_info", "patient_contact_info")
#' cleaned_df <- clean_phase_2_data(df, required_strings, standard_names)
#' print(cleaned_df)  # Should show updated column names
#'
#' # Example 3: Attempting to clean with missing columns
#' df2 <- data.frame(
#'   appointment_date = 1:5,
#'   patient_name = 6:10
#' )
#' required_strings <- c("doctor_info", "contact_data")  # Note: these do not exist
#' standard_names <- c("physician_info", "patient_contact_info")
#' cleaned_df2 <- clean_phase_2_data(df2, required_strings, standard_names)
#' print(cleaned_df2)  # Should issue warnings about missing columns
clean_phase_2_data <- function(data_or_path, required_strings, standard_names, output_csv_path = NULL) {
  # Log the input parameters
  cat("--- Starting data cleaning process ---\n")
  cat("Input data or path: ", data_or_path, "\n")
  cat("Required strings for renaming: ", paste(required_strings, collapse = ", "), "\n")
  cat("Standard names to apply: ", paste(standard_names, collapse = ", "), "\n")

  # Data loading and initial checks
  if (is.character(data_or_path)) {
    if (!file.exists(data_or_path)) {
      stop("Error: File does not exist at the specified path: ", data_or_path)
    }
    data <- readr::read_csv(data_or_path, show_col_types = FALSE)
    message("Data read from file at: ", data_or_path)
  } else if (is.data.frame(data_or_path)) {
    data <- data_or_path
    message("Data loaded from provided data frame.")
  } else {
    stop("Error: Data input must be either a data frame or a valid file path.")
  }

  # Log the initial dimensions of the data
  cat("Initial data dimensions: ", dim(data), "\n")

  # Clean and standardize column names
  data <- janitor::clean_names(data)
  message("Columns have been cleaned to snake case format. Updated column names: ", paste(names(data), collapse = ", "))

  # Apply the renaming function with detailed logging
  data <- rename_columns_by_substring(data, required_strings, standard_names)

  # Additional data processing (placeholder for future enhancements)
  message("Proceeding with additional data processing steps...")

  # Generate default output path with timestamp if not provided
  if (is.null(output_csv_path)) {
    current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    output_csv_path <- paste0("cleaned_phase_2_data_", current_datetime, ".csv")
  }

  # Save the cleaned data using write_output_csv
  write_output_csv(data, filename = basename(output_csv_path), output_dir = dirname(output_csv_path), verbose = TRUE)

  message("Cleaned data successfully saved to: ", output_csv_path)

  return(data)
}
