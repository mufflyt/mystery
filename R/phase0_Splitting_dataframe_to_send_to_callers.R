#' Split data into multiple parts and save each part as separate CSV files
#'
#' This function splits the data based on a specified column (e.g., insurance or specialty),
#' and saves each part as a separate CSV file. It allows you to prioritize certain values in the column,
#' and ensures that no physician appears in consecutive rows.
#'
#' @param input_data_or_path Either a dataframe containing the input data or a path to the input data file (RDS, CSV, or XLS/XLSX).
#' @param output_folder Directory where output CSV files will be saved. Default is current working directory.
#' @param assistant_names Vector of lab assistant names to name the output files.
#' @param seed_value Seed value for randomization (default is 1978).
#' @param complete_file_prefix Prefix for the complete output file name (default is "complete_version_").
#' @param split_file_prefix Prefix for each split output file name (default is empty).
#' @param recursive_create Logical indicating if directories should be created recursively (default is TRUE).
#' @param split_column The column on which to base the split (e.g., "insurance", "specialty_primary"). Default is "insurance".
#' @param priority_values Vector of values to prioritize within the split_column (e.g., c("Pediatric Dermatologists", "Medicaid")). Default is NULL.
#'
#' @importFrom dplyr arrange mutate group_by_at ungroup sample_frac
#' @importFrom readr write_csv read_csv
#' @importFrom fs dir_create dir_exists
#' @export
#'
#' @examples
#' \dontrun{
#' split_calls_to_lab_assistants_and_save_by_priority(input_data_or_path = "/path/to/file.csv",
#'                                                   output_folder = "/path/to/output/",
#'                                                   assistant_names = c("Assistant1", "Assistant2"),
#'                                                   split_column = "specialty_primary",
#'                                                   priority_values = c("Pediatric Dermatologists"))
#' }

split_calls_to_lab_assistants_and_save_by_priority <- function(input_data_or_path,
                                                               output_folder = getwd(),
                                                               assistant_names,
                                                               seed_value = 1978,
                                                               complete_file_prefix = "complete_version_",
                                                               split_file_prefix = "",
                                                               recursive_create = TRUE,
                                                               split_column = "insurance",
                                                               priority_values = NULL) {

  # conflicted::conflicts_prefer(dplyr::lag)

  # Step 1: Load and validate input data
  input_data <- load_and_validate_input_data(input_data_or_path)

  # Step 2: Ensure required column exists
  validate_split_column(input_data, split_column)

  # Step 3: Create a priority rank if needed
  if (!is.null(priority_values)) {
    input_data <- assign_priority_rank(input_data, split_column, priority_values)
  }

  # Step 4: Assign lab assistants randomly
  input_data <- assign_lab_assistants(input_data, assistant_names, split_column, seed_value)

  # Step 5: Avoid consecutive physicians
  input_data <- avoid_consecutive_physicians(input_data, "first", "last")

  # Step 6: Save the complete data and split data
  save_complete_and_split_data(input_data, output_folder, complete_file_prefix, split_file_prefix, recursive_create)

  return(invisible(input_data))
}

#' @noRd
save_complete_and_split_data <- function(input_data, output_folder, complete_file_prefix, split_file_prefix, recursive_create) {

  # Create output folder if it doesn't exist
  if (!fs::dir_exists(output_folder)) {
    tryCatch({
      fs::dir_create(output_folder, recursive = recursive_create)
    }, error = function(e) {
      stop("Failed to create output folder. Check permissions and try again.")
    })
  }

  # Save the complete data to a CSV file
  current_timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_path <- file.path(output_folder, paste0(complete_file_prefix, current_timestamp, ".csv"))

  tryCatch({
    readr::write_csv(input_data, complete_output_path)
    message("Saved the complete data to: ", complete_output_path)
  }, error = function(e) {
    stop("Error saving the complete data file: ", e$message)
  })

  # Split the data by lab assistants and save each part
  split_data <- split(input_data, input_data$assigned_lab_assistant)

  for (lab_assistant in names(split_data)) {
    output_file <- file.path(output_folder, paste0(split_file_prefix, lab_assistant, "_", current_timestamp, ".csv"))
    tryCatch({
      readr::write_csv(split_data[[lab_assistant]], output_file)
      message("Saved split data for ", lab_assistant, " to: ", output_file)
    }, error = function(e) {
      stop("Error saving split data for ", lab_assistant, ": ", e$message)
    })
  }
}
