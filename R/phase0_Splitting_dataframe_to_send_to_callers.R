#' Split data into multiple parts and save each part as separate Excel files
#'
#' This function splits the data based on a specified column (e.g., insurance or specialty),
#' and saves each part as a separate Excel file. It allows you to prioritize certain values in the column,
#' and ensures that no physician appears in consecutive rows.
#'
#' @param input_data_or_path Either a dataframe containing the input data or a path to the input data file (RDS, CSV, or XLS/XLSX).
#' @param output_folder Directory where output Excel files will be saved. Default is current working directory.
#' @param assistant_names Vector of lab assistant names to name the output files.
#' @param seed_value Seed value for randomization (default is 1978).
#' @param complete_file_prefix Prefix for the complete output file name (default is "complete_version_").
#' @param split_file_prefix Prefix for each split output file name (default is empty).
#' @param recursive_create Logical indicating if directories should be created recursively (default is TRUE).
#' @param split_column The column on which to base the split (e.g., "insurance", "specialty_primary"). Default is "insurance".
#' @param priority_values Vector of values to prioritize within the split_column (e.g., c("Pediatric Dermatologists", "Medicaid")). Default is NULL.
#'
#' @importFrom dplyr arrange mutate group_by_at ungroup sample_frac
#' @importFrom openxlsx write.xlsx
#' @importFrom fs dir_create dir_exists
#' @importFrom readr read_csv
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

  conflicted::conflicts_prefer(dplyr::lag)

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
load_and_validate_input_data <- function(input_data_or_path) {
  if (is.character(input_data_or_path)) {
    if (!base::file.exists(input_data_or_path)) {
      stop("The file does not exist at the specified path: ", input_data_or_path)
    }
    input_data <- readr::read_csv(input_data_or_path)
  } else if (is.data.frame(input_data_or_path)) {
    input_data <- input_data_or_path
  } else {
    stop("The input must be either a data frame or a valid file path.")
  }
  return(input_data)
}

#' @noRd
validate_split_column <- function(input_data, split_column) {
  if (!split_column %in% colnames(input_data)) {
    stop(paste("The column '", split_column, "' does not exist in the input data."))
  }
}

#' @noRd
assign_priority_rank <- function(input_data, split_column, priority_values) {
  if (!all(priority_values %in% unique(input_data[[split_column]]))) {
    stop("Some priority values are not present in the split column.")
  }

  priority_rank <- setNames(seq_along(priority_values), priority_values)
  input_data <- input_data %>%
    dplyr::mutate(priority_rank = dplyr::if_else(
      input_data[[split_column]] %in% priority_values,
      priority_rank[.[[split_column]]],
      length(priority_values) + 1
    )) %>%
    dplyr::arrange(priority_rank)

  return(input_data)
}

#' @noRd
assign_lab_assistants <- function(input_data, assistant_names, split_column, seed_value) {
  if (length(assistant_names) < 2) {
    stop("Please provide at least two lab assistant names.")
  }

  set.seed(seed_value)
  input_data <- input_data %>%
    dplyr::group_by_at(split_column) %>%
    dplyr::mutate(assigned_lab_assistant = sample(assistant_names, n(), replace = TRUE)) %>%
    dplyr::ungroup()

  return(input_data)
}

#' @noRd
avoid_consecutive_physicians <- function(data, first_name_column, last_name_column) {
  log_progress("Shuffling rows to avoid consecutive physicians...")

  shuffled_data <- data
  consecutive_issue <- TRUE

  while (consecutive_issue) {
    # Shuffle the rows
    shuffled_data <- shuffled_data %>%
      dplyr::sample_frac(1)

    # Create a lagged column to check for consecutive rows with the same physician
    shuffled_data <- shuffled_data %>%
      dplyr::mutate(lag_first_name = lag(.data[[first_name_column]]),
                    lag_last_name = lag(.data[[last_name_column]]),
                    same_physician = if_else(.data[[first_name_column]] == lag_first_name &
                                               .data[[last_name_column]] == lag_last_name, TRUE, FALSE))

    # Check if there are any consecutive physicians
    consecutive_issue <- any(shuffled_data$same_physician, na.rm = TRUE)

    # Log each shuffle
    log_progress(paste("Checking for consecutive physicians: Consecutive issue =", consecutive_issue))
  }

  # Drop helper columns
  shuffled_data <- shuffled_data %>%
    dplyr::select(-lag_first_name, -lag_last_name, -same_physician)

  log_progress("Shuffling completed without consecutive physicians.")
  return(shuffled_data)
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

  # Save the complete data to an Excel file
  current_timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_path <- file.path(output_folder, paste0(complete_file_prefix, current_timestamp, ".xlsx"))

  tryCatch({
    openxlsx::write.xlsx(input_data, complete_output_path)
    message("Saved the complete data to: ", complete_output_path)
  }, error = function(e) {
    stop("Error saving the complete data file: ", e$message)
  })

  # Split the data by lab assistants and save each part
  split_data <- split(input_data, input_data$assigned_lab_assistant)

  for (lab_assistant in names(split_data)) {
    output_file <- file.path(output_folder, paste0(split_file_prefix, lab_assistant, "_", current_timestamp, ".xlsx"))
    tryCatch({
      openxlsx::write.xlsx(split_data[[lab_assistant]], output_file)
      message("Saved split data for ", lab_assistant, " to: ", output_file)
    }, error = function(e) {
      stop("Error saving split data for ", lab_assistant, ": ", e$message)
    })
  }
}

#' @noRd
log_progress <- function(message) {
  cat("[LOG]", message, "\n")
}

