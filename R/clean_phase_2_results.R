#' Clean and Process Phase 2 Data
#'
#' This function reads data from a file or data frame, cleans column names, and applies renaming based on specified criteria to facilitate data analysis. The function logs each step of the process, including data loading, column cleaning, and renaming for transparency.
#'
#' @param data_or_path Path to the data file or a data frame.
#' @param required_strings Vector of substrings for which to search in column names.
#' @param standard_names Vector of new names to apply to the matched columns.
#' @param output_csv_path Optional. If provided, the cleaned data will be saved to this path.
#' @return A data frame with processed data.
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr rename_with mutate select
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom assertthat assert_that is.string is.readable
#' @examples
#' # Example 1: Cleaning data from a CSV file
#' input_path <- "path_to_your_data.csv"
#' required_strings <- c("physician_information", "able_to_contact_office")
#' standard_names <- c("physician_info", "contact_office")
#' cleaned_data <- clean_phase_2_data(input_path, required_strings, standard_names)
#'
#' # Example 2: Directly using a data frame
#' df <- data.frame(DocInfo = 1:5, ContactData = 6:10)
#' required_strings <- c("doc_info", "contact_data")
#' standard_names <- c("doctor_info", "patient_contact_info")
#' cleaned_df <- clean_phase_2_data(df, required_strings, standard_names)
#' print(cleaned_df)  # Should show updated column names
#'
#' @export
clean_phase_2_data <- function(data_or_path, required_strings, standard_names, output_csv_path = NULL) {
  # Validate inputs using assertthat
  assertthat::assert_that(
    is.character(required_strings) && length(required_strings) > 0,
    msg = "Error: 'required_strings' must be a non-empty character vector."
  )
  assertthat::assert_that(
    is.character(standard_names) && length(standard_names) > 0,
    msg = "Error: 'standard_names' must be a non-empty character vector."
  )
  assertthat::assert_that(
    length(required_strings) == length(standard_names),
    msg = "Error: 'required_strings' and 'standard_names' must have the same length."
  )
  if (is.character(data_or_path)) {
    assertthat::assert_that(
      assertthat::is.string(data_or_path) && assertthat::is.readable(data_or_path),
      msg = "Error: If 'data_or_path' is a string, it must be a valid, readable file path."
    )
  } else {
    assertthat::assert_that(
      is.data.frame(data_or_path),
      msg = "Error: 'data_or_path' must be either a valid file path or a data frame."
    )
  }
  if (!is.null(output_csv_path)) {
    assertthat::assert_that(
      assertthat::is.string(output_csv_path),
      msg = "Error: 'output_csv_path', if provided, must be a string."
    )
  }

  # Log the input parameters
  cat("--- Starting data cleaning process ---\n")
  cat("Input data or path: ", data_or_path, "\n")
  cat("Required strings for renaming: ", paste(required_strings, collapse = ", "), "\n")
  cat("Standard names to apply: ", paste(standard_names, collapse = ", "), "\n")

  # Data loading
  if (is.character(data_or_path)) {
    data <- readr::read_csv(data_or_path, show_col_types = FALSE)
    message("Data read from file at: ", data_or_path)
  } else {
    data <- data_or_path
    message("Data loaded from provided data frame.")
  }

  # Log the initial dimensions of the data
  cat("Initial data dimensions: ", dim(data), "\n")

  # Clean and standardize column names
  data <- data %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_to_lower(stringr::str_replace_all(., "[^[:alnum:]_]", "_"))
    )
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

  # Save the cleaned data
  readr::write_csv(data, output_csv_path)
  message("Cleaned data successfully saved to: ", output_csv_path)

  return(data)
}

# Helper Function: Rename Columns by Substring Matching
rename_columns_by_substring <- function(data, required_strings, standard_names) {
  # Validate column renaming logic
  assertthat::assert_that(
    length(required_strings) == length(standard_names),
    msg = "Error: 'required_strings' and 'standard_names' must have the same length."
  )

  # Map required strings to their corresponding standard names
  column_map <- setNames(standard_names, required_strings)

  # Rename columns based on matching substrings
  new_names <- names(data)
  for (pattern in names(column_map)) {
    new_names <- ifelse(
      stringr::str_detect(new_names, pattern, ignore_case = TRUE),
      column_map[[pattern]],
      new_names
    )
  }
  names(data) <- new_names

  return(data)
}
