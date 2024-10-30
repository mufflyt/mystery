#' Check City-State Combinations for Minimum Generalists and Specialists with Logging and Summary
#'
#' This function checks if each city-state combination has the required number of generalists and specialists.
#' It logs inputs, transformations, outputs, and returns two data frames: one with the failing city-state-specialty combinations
#' and one with the successful combinations. Optionally, the results can be saved as CSV files.
#'
#' #### Key Features:
#' - **Generalists vs. Specialists**: You can specify the names of generalists and specialists, and the function checks if
#'   each city-state combination has the required number of each.
#' - **Logging**: Extensive logging ensures that inputs, transformations, and results are tracked.
#' - **CSV Output**: Optionally, the function writes both failing and successful city-state-specialty combinations to separate CSV files.
#' - **Summary Logging**: A summary of the `min_generalists`, `min_specialists`, and the results is logged at the end.
#'
#' @param data A data frame containing professional information. Must include at least the columns `city`, `state_code`, and `specialty_primary`.
#' @param min_generalists An integer specifying the minimum number of generalists required per city-state combination.
#' @param min_specialists An integer specifying the minimum number of specialists required per city-state combination.
#' @param generalist_name A string specifying the specialty name for generalists. Default is `"General Dermatology"`.
#' @param specialist_name A string specifying the specialty name for specialists. Default is `"Pediatric Dermatology"`.
#' @param failing_csv_path An optional string specifying the file path to save the failing combinations CSV. Default is `NULL` (no file saved).
#' @param successful_csv_path An optional string specifying the file path to save the successful combinations CSV. Default is `NULL` (no file saved).
#'
#' @return A list containing two data frames: `failing_combinations` and `successful_combinations`.
#'
#' @export
phase0_city_state_check_specialty_generalist_counts <- function(data,
                                                         min_generalists,
                                                         min_specialists,
                                                         generalist_name = "General Dermatology",
                                                         specialist_name = "Pediatric Dermatology",
                                                         failing_csv_path = NULL,
                                                         successful_csv_path = NULL) {

  # Set logging threshold to INFO
  logger::log_threshold(logger::INFO)

  # Log function inputs
  logger::log_info("Starting function city_state_check_specialty_generalist_counts()")
  logger::log_info(glue::glue("Input parameters: min_generalists = {min_generalists}, min_specialists = {min_specialists}, generalist_name = {generalist_name}, specialist_name = {specialist_name}, failing_csv_path = {failing_csv_path}, successful_csv_path = {successful_csv_path}"))

  # Input validation
  logger::log_info("Validating input data...")
  required_columns <- c("city", "state_code", "specialty_primary")
  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    logger::log_error(glue::glue("Missing required columns: {paste(missing_columns, collapse = ', ')}"))
    stop(glue::glue("Missing required columns: {paste(missing_columns, collapse = ', ')}"))
  }

  logger::log_info(glue::glue("Input data has {nrow(data)} rows and {ncol(data)} columns."))

  # Group by city, state_code, and specialty_primary, then count the number of professionals
  logger::log_info("Grouping data by city, state_code, and specialty_primary, and counting professionals...")
  specialty_counts <- data %>%
    dplyr::group_by(city, state_code, specialty_primary) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop')

  logger::log_info("Transformation complete. Sample of grouped data:")
  logger::log_debug(utils::capture.output(print(head(specialty_counts))))

  # Ensure that cities without any Pediatric Dermatologists are flagged
  all_city_state <- data %>%
    dplyr::select(city, state_code) %>%
    dplyr::distinct()

  check_results <- all_city_state %>%
    dplyr::left_join(
      specialty_counts %>% dplyr::filter(specialty_primary == generalist_name),
      by = c("city", "state_code")
    ) %>%
    dplyr::left_join(
      specialty_counts %>% dplyr::filter(specialty_primary == specialist_name),
      by = c("city", "state_code"),
      suffix = c("_generalist", "_specialist")
    ) %>%
    dplyr::mutate(
      generalist_check = dplyr::if_else(is.na(count_generalist) | count_generalist < min_generalists, FALSE, TRUE),
      specialist_check = dplyr::if_else(is.na(count_specialist) | count_specialist < min_specialists, FALSE, TRUE)
    )

  # Separate failing and successful combinations
  failing_combinations <- check_results %>%
    dplyr::filter(!generalist_check | !specialist_check) %>%
    dplyr::mutate(
      missing_generalists = dplyr::if_else(!generalist_check, TRUE, FALSE),
      missing_specialists = dplyr::if_else(!specialist_check, TRUE, FALSE)
    ) %>%
    dplyr::select(city, state_code, count_generalist, count_specialist, missing_generalists, missing_specialists)

  successful_combinations <- check_results %>%
    dplyr::filter(generalist_check & specialist_check) %>%
    dplyr::select(city, state_code, count_generalist, count_specialist)

  # Log the summary of failing and successful combinations
  logger::log_info(glue::glue("Found {nrow(failing_combinations)} failing city-state-specialty combinations."))
  logger::log_info(glue::glue("Found {nrow(successful_combinations)} successful city-state-specialty combinations."))

  # Output the failing combinations to a CSV if the file path is provided
  if (!is.null(failing_csv_path)) {
    logger::log_info(glue::glue("Writing failing combinations to CSV: {failing_csv_path} using write_output_csv"))
    write_output_csv(failing_combinations, filename = basename(failing_csv_path), output_dir = dirname(failing_csv_path), verbose = TRUE)
  }

  # Output the successful combinations to a CSV if the file path is provided
  if (!is.null(successful_csv_path)) {
    logger::log_info(glue::glue("Writing successful combinations to CSV: {successful_csv_path} using write_output_csv"))
    write_output_csv(successful_combinations, filename = basename(successful_csv_path), output_dir = dirname(successful_csv_path), verbose = TRUE)
  }

  # Log function completion
  logger::log_info("city_state_check_specialty_generalist_counts() completed successfully.")

  # Return both failing and successful combinations as a list
  return(list(failing_combinations = failing_combinations, successful_combinations = successful_combinations))
}
