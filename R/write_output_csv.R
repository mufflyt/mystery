#' Write a Data Frame to CSV with Robust Logging and Error Handling
#'
#' This function writes a data frame to a CSV file in the specified output directory.
#' It includes detailed logging to inform the user of the function's progress and any potential issues.
#'
#' #### Key Features of the Function:
#' - **Directory Creation**: If the specified output directory does not exist, the function attempts to create it.
#' - **Logging**: Verbose logging informs the user of the progress and any potential errors during the writing process.
#' - **Error Handling**: The function handles errors such as invalid input types or directory creation failures.
#'
#' @param df A data frame to be written to a CSV file.
#' @param filename A string specifying the name of the output file (with .csv extension).
#' @param output_dir A string specifying the directory where the CSV file will be saved. Default is `"ortho_sports_med/Figures"`.
#' @param verbose A boolean indicating whether to print detailed logs. Default is `TRUE`.
#'
#' @return NULL. The function saves the CSV file to the specified location.
#'
#' @importFrom utils write.csv
#' @importFrom assertthat assert_that is.string
#'
#' @examples
#' # Example 1: Save a data frame to the default directory with detailed logging
#' df <- data.frame(Name = c("John", "Jane"), Age = c(30, 25))
#' write_output_csv(df, "output.csv")
#'
#' # Example 2: Save a data frame to a custom directory without logging
#' df <- data.frame(Product = c("Apple", "Banana"), Price = c(1.2, 0.5))
#' write_output_csv(df, "output.csv", output_dir = "custom/directory", verbose = FALSE)
#'
#' # Example 3: Save a large data frame with detailed logging
#' df_large <- data.frame(ID = 1:1000, Value = rnorm(1000))
#' write_output_csv(df_large, "large_output.csv", output_dir = "data/outputs", verbose = TRUE)
#'
#' @export
write_output_csv <- function(df, filename, output_dir = "ortho_sports_med/Figures", verbose = TRUE) {

  # Validate inputs
  assertthat::assert_that(is.data.frame(df), msg = "The input 'df' must be a data frame.")
  assertthat::assert_that(assertthat::is.string(filename), grepl("\\.csv$", filename),
                          msg = "The 'filename' must be a string ending with '.csv'.")
  assertthat::assert_that(assertthat::is.string(output_dir), msg = "The 'output_dir' must be a valid string.")

  if (!is.logical(verbose)) {
    stop("'verbose' must be a boolean.")
  }

  # Check and create the directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    if (verbose) {
      cat("Output directory does not exist. Attempting to create directory:", output_dir, "\n")
    }
    dir.create(output_dir, recursive = TRUE)
    if (!dir.exists(output_dir)) {
      stop("Failed to create output directory:", output_dir)
    }
  }

  # Construct the full output file path
  output_file_path <- file.path(output_dir, filename)

  # Attempt to write the CSV file
  tryCatch({
    utils::write.csv(df, file = output_file_path, row.names = FALSE)
    if (verbose) {
      cat("File successfully saved to:", output_file_path, "\n")
    }
  }, error = function(e) {
    stop("Failed to write the CSV file. Error: ", e$message)
  })
}
