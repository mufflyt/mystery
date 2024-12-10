#' #' Sample Generalists and Specialists by City-State Combination
#'
#' This function samples specialists and generalists from a given dataset based on city-state combinations.
#' It allows sampling up to three types of specialists and generalists with customizable sample sizes for each.
#' The results can be saved as a CSV file or returned as a dataframe.
#'
#' @param data A dataframe containing specialist information. It must have columns: `city`, `state_code`, `specialty_primary`, and `phone_number`.
#' @param generalist A character string specifying the generalist specialty to sample. Default is "General Dermatology".
#' @param specialist1 A character string specifying the first specialist specialty to sample. Default is "Pediatric Dermatology".
#' @param general_sample_size An integer specifying how many generalists to sample for each city-state combination. Default is 4.
#' @param specialist1_sample_size An integer specifying how many of the first specialists to sample for each city-state combination. Default is 1.
#' @param specialist2 A character string specifying a second specialist specialty to sample. Optional. Default is NULL.
#' @param specialist2_sample_size An integer specifying how many of the second specialists to sample. Default is 0.
#' @param specialist3 A character string specifying a third specialist specialty to sample. Optional. Default is NULL.
#' @param specialist3_sample_size An integer specifying how many of the third specialists to sample. Default is 0.
#' @param same_phone_number A logical value indicating whether to sample generalists and specialists with the same phone number (TRUE) or different phone numbers (FALSE). Default is TRUE.
#' @param output_csv_path A character string specifying the path to save the output CSV. If not provided, the result will only be returned.
#' @param seed An integer for setting a seed for reproducibility. Default is 1978.
#'
#' @return A dataframe containing the sampled generalists and specialists for each city-state combination.
#'
#' @examples
#' # Example 1: Basic usage with default generalist and specialist
#' data <- data.frame(
#'   city = rep(c("New York", "Los Angeles"), each = 6),
#'   state_code = rep(c("NY", "CA"), each = 6),
#'   specialty_primary = c(
#'     "General Dermatology", "Pediatric Dermatology", "General Dermatology",
#'     "General Dermatology", "Pediatric Dermatology", "General Dermatology",
#'     "General Dermatology", "General Dermatology", "Pediatric Dermatology",
#'     "General Dermatology", "General Dermatology", "Pediatric Dermatology"
#'   ),
#'   phone_number = rep(c("123", "456", "789"), 4)
#' )
#' result <- city_state_sample_specialists(data)
#' print(result)
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr slice_sample
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom purrr map_dfr
#' @importFrom logger log_info
#' @importFrom logger log_warn
#' @importFrom logger log_error
#'
#' @export
phase0_city_state_sample_specialists <- function(data,
                                          generalist = "General Dermatology",
                                          specialist1 = "Pediatric Dermatology",
                                          general_sample_size = 4,
                                          specialist1_sample_size = 2,
                                          specialist2 = NULL,
                                          specialist2_sample_size = 0,
                                          specialist3 = NULL,
                                          specialist3_sample_size = 0,
                                          same_phone_number = TRUE,
                                          output_csv_path = NULL,
                                          seed = 1978) {

  # Set seed for reproducibility
  set.seed(seed)
  logger::log_info("Seed set to {seed}.")

  # Log function inputs
  logger::log_info("Function called with the following arguments:")
  logger::log_info("  Generalist: {generalist}")
  logger::log_info("  Specialist1: {specialist1}")
  logger::log_info("  Generalist Sample Size: {general_sample_size}")
  logger::log_info("  Specialist1 Sample Size: {specialist1_sample_size}")
  logger::log_info("  Specialist2: {specialist2}")
  logger::log_info("  Specialist2 Sample Size: {specialist2_sample_size}")
  logger::log_info("  Specialist3: {specialist3}")
  logger::log_info("  Specialist3 Sample Size: {specialist3_sample_size}")
  logger::log_info("  Same Phone Number: {same_phone_number}")
  logger::log_info("  Output CSV Path: {output_csv_path}")

  # Input validation for data structure
  if (!is.data.frame(data)) {
    logger::log_error("The input 'data' must be a dataframe.")
    stop("The input 'data' must be a dataframe.")
  }

  # Check if required columns exist and have non-NA values
  required_columns <- c("city", "state_code", "specialty_primary", "phone_number")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    logger::log_error("Missing columns: {paste(missing_columns, collapse = ', ')}.")
    stop("Missing required columns in the dataset.")
  }

  if (any(is.na(data$city)) || any(is.na(data$state_code))) {
    logger::log_warn("Some 'city' or 'state_code' values are missing. These rows will be excluded.")
    data <- data %>%
      dplyr::filter(!is.na(city) & !is.na(state_code))
  }

  # Log input data dimensions
  logger::log_info("Input data dimensions: {nrow(data)} rows and {ncol(data)} columns.")

  # Step 1: Filter data for the selected generalist and specialists
  logger::log_info("Filtering city-state combinations with at least {general_sample_size} generalists and {specialist1_sample_size} specialists.")

  filtered_data <- data %>%
    dplyr::group_by(city, state_code) %>%
    dplyr::summarize(
      num_generalists = sum(specialty_primary == generalist, na.rm = TRUE),
      num_specialists1 = sum(specialty_primary == specialist1, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::filter(num_generalists >= general_sample_size & num_specialists1 >= specialist1_sample_size)

  # Log filtered data dimensions
  logger::log_info("Filtered data dimensions: {nrow(filtered_data)} rows.")

  # Proceed if there are valid rows in filtered data
  if (nrow(filtered_data) == 0) {
    logger::log_warn("No city-state combinations meet the required criteria.")
    return(data.frame())  # Return empty dataframe if no valid combinations
  }

  # Step 2: Sample generalists and specialists for each city-state combination
  tryCatch({
    sampled_data <- filtered_data %>%
      dplyr::group_split(city, state_code) %>%
      purrr::map_dfr(~ {
        city_state_data <- data %>%
          dplyr::filter(city == unique(.x$city) & state_code == unique(.x$state_code))

        if (same_phone_number) {
          # Filter to ensure generalists and specialists have the same phone number
          city_state_data <- city_state_data %>%
            dplyr::group_by(phone_number) %>%
            dplyr::filter(n() >= general_sample_size + specialist1_sample_size)
        }

        # Sample generalists
        generalists <- city_state_data %>%
          dplyr::filter(specialty_primary == generalist) %>%
          dplyr::slice_sample(n = min(nrow(.), general_sample_size), replace = FALSE)

        # Sample specialist1
        specialists1 <- city_state_data %>%
          dplyr::filter(specialty_primary == specialist1) %>%
          dplyr::slice_sample(n = min(nrow(.), specialist1_sample_size), replace = FALSE)

        # Combine the samples
        dplyr::bind_rows(generalists, specialists1)
      })

    logger::log_info("Sampling process complete. Sampled data dimensions: {nrow(sampled_data)} rows.")

  }, error = function(e) {
    logger::log_error("Error during sampling: {e$message}")
    stop("Error during sampling: ", e$message)
  })

  # Step 3: Write the output to a CSV if the path is provided
  if (!is.null(output_csv_path)) {
    tryCatch({
      logger::log_info("Writing output to CSV at {output_csv_path} using write_output_csv.")
      write_output_csv(sampled_data, filename = basename(output_csv_path), output_dir = dirname(output_csv_path), verbose = TRUE)
      logger::log_info("Output successfully written to: {output_csv_path}.")
    }, error = function(e) {
      logger::log_error("Error writing the output to CSV using write_output_csv: {e$message}")
      stop("Error writing the output to CSV: ", e$message)
    })
  }

  # Final log and message
  logger::log_info("Returning sampled data from the function. Final data dimensions: {nrow(sampled_data)} rows.")

  return(sampled_data)
}
