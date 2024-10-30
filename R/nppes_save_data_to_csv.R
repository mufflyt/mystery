#' Save the final data to CSV
#'
#' This function saves the cleaned data to a CSV file.
#'
#' @details Use this function to save the final cleaned data to a specified file path.
#'
#' @param data A cleaned data frame.
#' @param file_path A character string specifying the path to the CSV file.
#' @return None
#' @examples
#' nppes_save_data_to_csv(cleaned_data, "/path/to/your/file.csv")
nppes_save_data_to_csv <- function(data, file_path) {
  base::cat("Saving data to CSV at", file_path, "...\n")

  # Use write_csv with append logic
  readr::write_csv(data, file_path)

  base::cat("Data saved to", file_path, "successfully.\n")
  base::cat("Next step: None, process complete.\n")
}
