#' Phase 0: Genderize Physicians Data with Logging and Detailed Data Transformation
#'
#' @param physician_data A dataframe containing physician data with a column named 'first_name'
#' for genderization.
#' @param first_name_column Name of the column containing first names. Default is "first".
#' @param last_name_column Name of the column containing last names. Default is "last".
#' @param save_directory Directory to save output. Default is NULL.
#' @param verbose Whether to show detailed logging. Default is TRUE.
#'
#' @importFrom gender gender
#' @importFrom dplyr select rename distinct left_join mutate
#' @importFrom logger log_info log_error
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom assertthat assert_that
#' @export
phase0_genderize_physicians <- function(physician_data,
                                        first_name_column = "first",
                                        last_name_column = "last",
                                        save_directory = NULL,
                                        verbose = TRUE) {
  # Validate inputs specifically for physician_data
  validate_physician_inputs(physician_data, first_name_column, last_name_column, verbose)

  # Genderize names based on the first name column
  genderized_names <- genderize_first_names(physician_data, first_name_column, verbose)

  # Merge the genderized data with the original physician_data
  merged_physician_data <- merge_genderized_names(physician_data, genderized_names, first_name_column, verbose)

  # Log information about missing gender data
  log_missing_gender_info(merged_physician_data, verbose)

  # Inform user about the new column for gender data
  if (verbose) {
    logger::log_info(glue::glue(
      "The gender data has been added to the dataset in the column named 'gender'."
    ))
  }

  # Save the output if a directory is provided
  if (!is.null(save_directory)) {
    save_genderized_data(merged_physician_data, save_directory, verbose)
  } else if (verbose) {
    logger::log_info("No save directory provided. Skipping saving the output.")
  }

  return(merged_physician_data)
}

# Helper function: Validate physician_data input
validate_physician_inputs <- function(physician_data, first_name_column, last_name_column, verbose) {
  required_columns <- c(first_name_column, last_name_column)
  missing_cols <- setdiff(required_columns, colnames(physician_data))

  if (length(missing_cols) > 0) {
    stop(glue::glue("The physician_data data frame is missing these required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (verbose) {
    logger::log_info("Validation successful: All required columns are present in physician_data.")
  }
}

# Helper function: Genderize first names
genderize_first_names <- function(physician_data, first_name_column, verbose) {
  if (verbose) {
    logger::log_info("Starting genderization of first names from column '{first_name_column}'...")
  }

  # Extract unique first names
  unique_names <- physician_data %>%
    dplyr::select(all_of(first_name_column)) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # Use gender package to genderize names
  genderized_results <- gender::gender(
    names = unique_names,
    method = "ssa",  # Using SSA database
    years = c(1932, 2012)  # Common range for practicing physicians
  ) %>%
    dplyr::select(name, gender, proportion_male) %>%
    dplyr::rename(
      !!first_name_column := name,
      gender_probability = proportion_male
    ) %>%
    dplyr::mutate(
      gender = dplyr::case_when(
        gender == "female" ~ "Female",
        gender == "male" ~ "Male",
        TRUE ~ NA_character_
      )
    )

  if (verbose) {
    logger::log_info("Genderization completed for {nrow(genderized_results)} unique names.")

    # Log additional statistics
    names_with_gender <- sum(!is.na(genderized_results$gender))
    logger::log_info("Successfully assigned gender to {names_with_gender} out of {nrow(genderized_results)} unique names.")
  }

  return(genderized_results)
}

# Helper function: Merge genderized data
merge_genderized_names <- function(physician_data, genderized_names, first_name_column, verbose) {
  merged_data <- physician_data %>%
    dplyr::left_join(genderized_names, by = first_name_column)

  if (verbose) {
    logger::log_info("Merged genderized names with physician data.")
  }

  return(merged_data)
}

# Helper function: Log missing gender info
log_missing_gender_info <- function(physician_data, verbose) {
  missing_gender_count <- sum(is.na(physician_data$gender))
  if (verbose) {
    logger::log_info("{missing_gender_count} records are missing gender information.")
  }
}

# Helper function: Save genderized data
save_genderized_data <- function(physician_data, save_directory, verbose) {
  if (verbose) {
    logger::log_info("Saving genderized data to {save_directory}...")
  }

  readr::write_csv(physician_data, file.path(save_directory, "genderized_physicians.csv"))
}
