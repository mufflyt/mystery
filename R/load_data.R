#' Load and Process Data from an RDS File with Robust Logging
#'
#' This function loads data from an RDS file, renames the 'ID' column to 'id_number', and logs every step of the process.
#'
#' @param data_dir A string specifying the directory where the RDS file is located.
#' @param file_name A string specifying the name of the RDS file to load.
#' @param verbose A boolean indicating whether to print detailed logs. Default is TRUE.
#'
#' @return A data frame with the 'ID' column renamed to 'id_number'.
#' @importFrom dplyr rename
#' @examples
#' # Example: Load data from a specified directory with logging
#' df <- load_data(data_dir = "data", file_name = "Phase_2.rds", verbose = TRUE)
#'
#' @export
load_data <- function(data_dir, file_name, verbose = TRUE) {

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

    df <- readRDS(file_path)
    if (verbose) {
      cat("  Data successfully loaded. Number of rows:", nrow(df), "\n")
    }

    # # Rename the 'ID' column to 'id_number'
    # df <- dplyr::rename(df, id_number = ID)
    # if (verbose) {
    #   cat("  Column 'ID' renamed to 'id_number'.\n")
    # }

    # Log the output
    if (verbose) {
      cat("  Final data structure:\n")
      print(str(df))
    }

    return(df)

  }, error = function(e) {
    stop("Failed to load and process the data. Error: ", e$message)
  })
}

# Example: Load data from a specified directory with logging
# df <- load_data(data_dir = "data", file_name = "Phase_2.rds", verbose = TRUE)
