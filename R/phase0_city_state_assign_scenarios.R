#' Assign Cases to Professionals by City and State
#'
#' The `phase0_city_state_assign_scenarios` function is designed to assign cases to professionals based on their
#' **specialty** and **location** (city and state). This function is particularly useful for managing scenarios where
#' professionals, such as physicians or other healthcare workers, need to be assigned cases for administrative or analytical purposes.
#'
#' #### **Key Features of the Function**:
#' - **Generalists vs. Specialists**: The function differentiates between generalists and specialists, assigning cases accordingly.
#' - **CSV Output**: The final output, including case assignments for each professional, is saved to a CSV file using the `write_output_csv` function.
#'
#' #### **Use Cases**:
#' - **Healthcare Assignment**: Assigning different types of cases to healthcare professionals based on their specialties and the cities/states where they practice.
#' - **Research Studies**: Managing scenarios in research studies where professionals need to be randomly assigned to cases.
#'
#' @param data A data frame containing professional information. Must include at least the columns `city`, `state_code`, and `specialty_primary`.
#' @param generalist A character string specifying the specialty name for generalists. Default is `"Generalist"`.
#' @param specialty A character string specifying the specialty name for specialists. Default is `"Specialist"`.
#' @param case_names A character vector of case names to assign. Default is `c("Alpha", "Beta", "Gamma")`.
#' @param output_csv_path A character string specifying the file path to save the output CSV. Default is `"output/city_state_assign_scenarios.csv"`.
#' @param seed An optional integer value to set the random seed for reproducibility. Default is `NULL` (no seed set).
#'
#' @return A data frame with the assigned cases.
#'
#' @examples
#' # Example 1: Using default parameters
#' data <- data.frame(
#'   city = c("CityA", "CityA", "CityB", "CityB"),
#'   state_code = c("State1", "State1", "State2", "State2"),
#'   specialty_primary = c("Generalist", "Specialist", "Generalist", "Specialist"),
#'   stringsAsFactors = FALSE
#' )
#' result <- city_state_assign_scenarios(data)
#' print(result)
#'
#' @importFrom dplyr filter group_by mutate ungroup bind_rows arrange
#' @importFrom tidyr unnest
#' @importFrom logger log_threshold log_info log_error log_debug
#' @importFrom glue glue
#' @export
phase0_city_state_assign_scenarios <- function(data,
                                               generalist = "General Dermatology",
                                               specialty = "Pediatric Dermatology",
                                               case_names = c("Case Alpha", "Case Beta", "Case Gamma"),
                                               output_csv_path = "Lizzy/data/city_state_assign_scenarios.csv",
                                               seed = 1978) {
  # Set logging threshold to INFO
  logger::log_threshold(logger::INFO)

  # Log the start of the function
  logger::log_info("Starting function city_state_assign_scenarios()")

  # Input validation
  logger::log_info("Validating input data...")
  if (!base::is.data.frame(data)) {
    logger::log_error("Input 'data' must be a data frame.")
    base::stop("Input 'data' must be a data frame.")
  }

  required_columns <- c("city", "state_code", "specialty_primary")
  missing_columns <- base::setdiff(required_columns, base::colnames(data))
  if (base::length(missing_columns) > 0) {
    logger::log_error(glue::glue("Missing required columns: {base::paste(missing_columns, collapse = ', ')}"))
    base::stop("Missing required columns: ", base::paste(missing_columns, collapse = ", "))
  }

  # Log input parameters
  logger::log_info(glue::glue(
    "Function inputs - Generalist: {generalist}, ",
    "Specialty: {specialty}, ",
    "Case Names: {base::paste(case_names, collapse = ', ')}, ",
    "Output CSV Path: {output_csv_path}"
  ))

  # Set seed for reproducibility if provided
  if (!base::is.null(seed)) {
    logger::log_info(glue::glue("Setting seed to {seed} for reproducibility"))
    base::set.seed(seed)
  }

  # Log initial data dimensions
  logger::log_info(glue::glue("Input data has {base::nrow(data)} rows and {base::ncol(data)} columns"))

  # Begin data processing
  logger::log_info("Starting data processing...")

  # Separate generalists and specialists
  logger::log_info("Separating generalists and specialists...")
  generalists <- data %>%
    dplyr::filter(specialty_primary == generalist)
  logger::log_info(glue::glue("Number of generalists: {base::nrow(generalists)}"))

  specialists <- data %>%
    dplyr::filter(specialty_primary == specialty)
  logger::log_info(glue::glue("Number of specialists: {base::nrow(specialists)}"))

  # Assign cases to specialists (each specialist gets all cases)
  logger::log_info("Assigning cases to specialists...")
  specialists_with_cases <- specialists %>%
    dplyr::group_by(city, state_code) %>%
    dplyr::mutate(case_assigned = base::list(case_names)) %>%
    tidyr::unnest(cols = c(case_assigned)) %>%
    dplyr::ungroup()
  logger::log_info(glue::glue("Specialists assigned cases. Total rows after unnesting: {base::nrow(specialists_with_cases)}"))

  # Assign cases to generalists (cases distributed among generalists)
  logger::log_info("Assigning cases to generalists...")
  generalists_with_cases <- generalists %>%
    dplyr::group_by(city, state_code) %>%
    dplyr::mutate(
      cases_repeated = base::rep(case_names, length.out = dplyr::n()),
      case_assigned = cases_repeated
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cases_repeated)

  # Randomly shuffle the case assignments for generalists within each city-state group
  generalists_with_cases <- generalists_with_cases %>%
    dplyr::group_by(city, state_code) %>%
    dplyr::mutate(case_assigned = base::sample(case_assigned)) %>%
    dplyr::ungroup()

  logger::log_info(glue::glue("Generalists assigned cases. Total generalists: {base::nrow(generalists_with_cases)}"))

  # Handle other professionals (who are neither generalists nor the specified specialists)
  logger::log_info("Processing other professionals...")
  others <- data %>%
    dplyr::filter(!specialty_primary %in% c(generalist, specialty)) %>%
    dplyr::mutate(case_assigned = NA_character_)
  logger::log_info(glue::glue("Number of other professionals: {base::nrow(others)}"))

  # Combine all data
  logger::log_info("Combining all data...")
  combined_data <- dplyr::bind_rows(specialists_with_cases, generalists_with_cases, others)
  logger::log_info(glue::glue("Combined data has {base::nrow(combined_data)} rows and {base::ncol(combined_data)} columns"))

  # Arrange the combined data
  logger::log_info("Arranging the combined data...")
  final_data <- combined_data %>%
    dplyr::arrange(city, state_code, specialty_primary)

  # Log a sample of the final data at debug level
  logger::log_debug("Sample of the final data:")
  logger::log_debug(utils::capture.output(base::print(head(final_data))))

  # Write the final data to CSV using write_output_csv
  tryCatch(
    {
      logger::log_info(glue::glue("Writing final data to CSV at {output_csv_path} using write_output_csv"))
      write_output_csv(final_data,
        filename = base::basename(output_csv_path),
        output_dir = base::dirname(output_csv_path), verbose = TRUE
      )
      logger::log_info("Final data successfully written to CSV using write_output_csv.")
    },
    error = function(e) {
      logger::log_error(glue::glue("Error writing CSV file using write_output_csv: {e$message}"))
      base::stop("Error writing CSV file: ", e$message)
    }
  )

  # Log completion of the function
  logger::log_info("Function city_state_assign_scenarios() completed successfully.")

  # Print the specified message
  base::print("Please check the column called `case_assigned` for the assigned scenario. Scenarios will be spread out across generalists but specialists will get every scenario.")

  # Return the final data frame
  return(final_data)
}
