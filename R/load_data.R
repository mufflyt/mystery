#' Load and Process Data from an RDS File with Robust Logging
#'
#' This function loads data from an RDS file, renames the 'ID' column to 'id_number', and logs every step of the process.
#'
#' @param data_dir A string specifying the directory where the RDS file is located.
#' @param file_name A string specifying the name of the RDS file to load.
#' @param verbose A boolean indicating whether to print detailed logs. Default is TRUE.
#'
#' @return A data frame with the 'ID' column renamed to 'id_number'.
#'
#' @details This function performs the following steps:
#' - Constructs the full file path from `data_dir` and `file_name`.
#' - Validates the existence of the RDS file at the specified path.
#' - Loads the data from the RDS file.
#' - Renames the 'ID' column to 'id_number'.
#' - Logs detailed information about each step if `verbose = TRUE`.
#'
#' @examples
#' # Example 1: Load data from a specified directory with logging
#' \dontrun{
#' df <- load_data(data_dir = "data", file_name = "Phase_2.rds", verbose = TRUE)
#' }
#'
#' # Example 2: Load data without logging
#' \dontrun{
#' df <- load_data(data_dir = "data", file_name = "Phase_2.rds", verbose = FALSE)
#' }
#'
#' # Example 3: Handle missing file error
#' \dontrun{
#' tryCatch({
#'   df <- load_data(data_dir = "invalid_path", file_name = "missing_file.rds", verbose = TRUE)
#' }, error = function(e) {
#'   cat("Error encountered:", e$message, "\n")
#' })
#' }
#'
#' @importFrom dplyr rename
#' @importFrom tools file_path_sans_ext
#' @import assertthat
#' @export
load_data <- function(data_dir, file_name, verbose = TRUE) {

  # Validate inputs using assertthat
  assertthat::assert_that(assertthat::is.string(data_dir), msg = "`data_dir` must be a string.")
  assertthat::assert_that(assertthat::is.string(file_name), msg = "`file_name` must be a string.")

  # Log the inputs to the function
  if (verbose) {
    cat("Function load_data called with the following inputs:\n")
    cat("  data_dir:", data_dir, "\n")
    cat("  file_name:", file_name, "\n")
  }

  # Construct the full file path
  file_path <- file.path(data_dir, file_name)
  if (verbose) {
    cat("  Constructed file path:", file_path, "\n")
  }

  # Validate that the file exists
  if (!file.exists(file_path)) {
    stop("File not found at path: ", file_path)
  }

  # Attempt to load the data and rename the column
  tryCatch({
    if (verbose) {
      cat("  Attempting to load data from:", file_path, "\n")
    }

    loaded_data <- readRDS(file_path)
    if (verbose) {
      cat("  Data successfully loaded. Number of rows:", nrow(loaded_data), "\n")
    }

    # Rename the 'ID' column to 'id_number' if it exists
    if ("ID" %in% colnames(loaded_data)) {
      loaded_data <- dplyr::rename(loaded_data, id_number = ID)
      if (verbose) {
        cat("  Column 'ID' renamed to 'id_number'.\n")
      }
    } else if (verbose) {
      cat("  Column 'ID' not found. No renaming performed.\n")
    }

    # Log the structure of the final data
    if (verbose) {
      cat("  Final data structure:\n")
      print(str(loaded_data))
    }

    return(loaded_data)

  }, error = function(e) {
    stop("Failed to load and process the data. Error: ", e$message)
  })
}
