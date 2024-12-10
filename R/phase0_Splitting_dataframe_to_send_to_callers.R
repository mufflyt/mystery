#' Phase 0: Split and Save Data by Lab Assistants with Priority
#'
#' This function splits the input dataset based on a specified column (e.g., insurance or specialty),
#' assigns lab assistants to each group, and saves each subset to separate CSV files. It logs
#' inputs, outputs, and all transformations to the console using the `logger` package.
#'
#' @param input_data_or_path A dataframe or a valid file path to the input data (supports CSV, RDS, XLS/XLSX).
#' @param output_folder Directory where output files will be saved. Default is the current working directory.
#' @param assistant_names A character vector of lab assistant names for assigning cases.
#' @param seed_value An integer seed for reproducibility. Default is `1978`.
#' @param complete_file_prefix Prefix for the complete output file. Default is `"complete_version_"`.
#' @param split_file_prefix Prefix for the split output files. Default is an empty string.
#' @param recursive_create Logical indicating if directories should be created recursively. Default is `TRUE`.
#' @param split_column The column used to split the data (e.g., `"insurance"`). Default is `"insurance"`.
#' @param priority_values A character vector of priority values within the split_column to ensure priority assignment.
#'        Default is `NULL`.
#'
#' @return The modified dataset after processing. Data transformations are logged to the console.
#' @importFrom dplyr mutate arrange group_by ungroup row_number distinct
#' @importFrom readr write_csv read_csv
#' @importFrom readxl read_excel
#' @importFrom fs dir_create dir_exists
#' @importFrom glue glue
#' @importFrom logger log_info log_error
#'
#' @examples
#' # Example 1: Basic usage with a dataframe
#' sample_dataset <- data.frame(
#'   insurance = c("Medicare", "Medicaid", "BCBS"),
#'   NPI = c("1234567890", "9876543210", "1122334455"),
#'   phone_number = c("123-456-7890", "098-765-4321", "555-555-5555")
#' )
#' assistants <- c("Alice", "Bob", "Charlie")
#' phase0_split_calls_to_lab_assistants_and_save_by_priority(
#'   input_data_or_path = sample_dataset,
#'   output_folder = "output/",
#'   assistant_names = assistants
#' )
#'
#' @export
phase0_split_calls_to_lab_assistants_and_save_by_priority <- function(input_data_or_path,
                                                                      output_folder = getwd(),
                                                                      assistant_names,
                                                                      seed_value = 1978,
                                                                      complete_file_prefix = "complete_version_",
                                                                      split_file_prefix = "",
                                                                      recursive_create = TRUE,
                                                                      split_column = "insurance",
                                                                      priority_values = NULL) {
  # Log inputs
  logger::log_info("Starting phase0_split_calls_to_lab_assistants_and_save_by_priority() with inputs:")
  logger::log_info("Output folder: {output_folder}")
  logger::log_info("Assistant names: {paste(assistant_names, collapse = ', ')}")
  logger::log_info("Seed value: {seed_value}")
  logger::log_info("Split column: {split_column}")
  logger::log_info("Priority values: {if (is.null(priority_values)) 'None' else paste(priority_values, collapse = ', ')}")

  # Step 1: Load and validate input data
  dataset <- load_and_validate_input_data(input_data_or_path)
  logger::log_info("Loaded input data with {nrow(dataset)} rows and {ncol(dataset)} columns.")

  # Step 2: Ensure required column exists
  validate_split_column(dataset, split_column)
  logger::log_info("Validated presence of split column: {split_column}.")

  # Step 3: Assign priorities if specified
  if (!is.null(priority_values)) {
    dataset <- assign_priority_values(dataset, split_column, priority_values)
    logger::log_info("Assigned priorities to split column: {split_column}.")
  }

  # Step 4: Assign lab assistants
  dataset <- assign_lab_assistants_to_cases(dataset, assistant_names, split_column, seed_value)
  logger::log_info("Assigned lab assistants to rows in the dataset.")

  # Step 5: Add row numbers and update combined_info
  dataset <- dataset %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::arrange(row_number) %>%
    dplyr::mutate(
      combined_info = paste0(
        row_number,
        ", ", row_number,
        ", ", combined_info,
        ", Grouping: ", !!rlang::sym(split_column)
      )
    )
  logger::log_info("Added row numbers and updated combined_info column.")

  # Step 6: Save the complete and split data
  save_complete_and_split_dataset(dataset, output_folder, complete_file_prefix, split_file_prefix, recursive_create)

  # Log completion
  logger::log_info("Completed phase0_split_calls_to_lab_assistants_and_save_by_priority().")
  return(invisible(dataset))
}

# Supporting Functions

#' @noRd
load_and_validate_input_data <- function(input_data_or_path) {
  if (is.data.frame(input_data_or_path)) {
    return(input_data_or_path)
  }

  if (is.character(input_data_or_path) && file.exists(input_data_or_path)) {
    file_extension <- tools::file_ext(input_data_or_path)
    if (file_extension == "csv") {
      return(readr::read_csv(input_data_or_path, show_col_types = FALSE))
    } else if (file_extension == "rds") {
      return(readRDS(input_data_or_path))
    } else if (file_extension %in% c("xls", "xlsx")) {
      return(readxl::read_excel(input_data_or_path))
    } else {
      logger::log_error("Unsupported file format: {file_extension}.")
      stop("Unsupported file format: ", file_extension)
    }
  }

  logger::log_error("Input must be a dataframe or a valid file path.")
  stop("Input must be a dataframe or a valid file path.")
}

#' @noRd
validate_split_column <- function(data, column_name) {
  if (!is.data.frame(data)) stop("Input data must be a dataframe.")
  if (!column_name %in% colnames(data)) stop(glue::glue("Column '{column_name}' is missing in the dataset."))
  return(invisible(TRUE))
}

#' @noRd
assign_priority_values <- function(dataset, split_column, priority_values) {
  dataset %>%
    dplyr::mutate(priority = dplyr::case_when(
      !!rlang::sym(split_column) %in% priority_values ~ 1,
      TRUE ~ 2
    )) %>%
    dplyr::arrange(priority)
}

#' @noRd
assign_lab_assistants_to_cases <- function(dataset, assistant_names, split_column, seed_value = NULL) {
  if (!is.null(seed_value)) set.seed(seed_value)
  dataset %>%
    dplyr::group_by(!!rlang::sym(split_column)) %>%
    dplyr::mutate(
      assigned_lab_assistant = sample(assistant_names, size = dplyr::n(), replace = TRUE)
    ) %>%
    dplyr::ungroup()
}

#' @noRd
save_complete_and_split_dataset <- function(dataset, output_folder, complete_file_prefix, split_file_prefix, recursive_create) {
  # Create output folder if it doesn't exist
  if (!fs::dir_exists(output_folder)) {
    fs::dir_create(output_folder, recursive = recursive_create)
    logger::log_info("Created output directory: {output_folder}.")
  }

  # Save the complete dataset to a CSV file
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_path <- file.path(output_folder, paste0(complete_file_prefix, timestamp, ".csv"))
  readr::write_csv(dataset, complete_output_path)
  logger::log_info("Saved complete dataset to: {complete_output_path}.")

  # Split the dataset by lab assistants and save each part
  split_data <- split(dataset, dataset$assigned_lab_assistant)
  for (lab_assistant in names(split_data)) {
    output_file <- file.path(output_folder, paste0(split_file_prefix, lab_assistant, "_", timestamp, ".csv"))
    readr::write_csv(split_data[[lab_assistant]], output_file)
    logger::log_info("Saved split data for {lab_assistant} to: {output_file}.")
  }
}
