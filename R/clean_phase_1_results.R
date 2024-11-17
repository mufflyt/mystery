#' Clean and Process Phase 1 Results
#'
#' This function cleans, processes, and prepares the Phase 1 results data for further analysis. It provides
#' functionality for row replication, optional label assignment, and saving the cleaned data to a CSV file
#' in a format ready for REDCap or further statistical analysis.
#'
#' @param phase1_input A data frame containing Phase 1 results data. Must include at least the columns:
#'   `"names"`, `"practice_name"`, `"phone_number"`, and `"state_name"`.
#' @param replicate_rows An optional integer specifying how many times to replicate the rows. Use `2` for
#'   doubling rows, `3` for tripling, etc. Default is `NULL` (no replication).
#' @param replication_labels An optional character vector of labels (e.g., races) to assign to the replicated
#'   rows. The length of this vector must match the `replicate_rows` value. Default is `NULL`.
#' @param csv_file_path An optional path to save the cleaned data as a CSV file. If `NULL`, the cleaned data
#'   is saved with a timestamp in a `cleaned_data` directory.
#'
#' @return A cleaned and processed data frame ready for analysis or upload.
#'
#' @importFrom dplyr mutate select filter mutate_all if_else row_number slice_head left_join everything
#' @importFrom readr type_convert write_csv
#' @importFrom stringr str_replace str_remove regex str_detect
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' # Example 1: Basic cleaning with no replication or labels
#' cleaned_phase1 <- clean_phase_1_results(
#'   phase1_input = my_data,
#'   replicate_rows = NULL,
#'   replication_labels = NULL,
#'   csv_file_path = NULL
#' )
#' head(cleaned_phase1)
#'
#' # Example 2: Replicate rows three times with race labels
#' cleaned_phase1 <- clean_phase_1_results(
#'   phase1_input = my_data,
#'   replicate_rows = 3,
#'   replication_labels = c("White", "Black", "Asian"),
#'   csv_file_path = "replicated_data.csv"
#' )
#' head(cleaned_phase1)
#'
#' # Example 3: Replicate rows four times and save to a custom CSV file
#' cleaned_phase1 <- clean_phase_1_results(
#'   phase1_input = my_data,
#'   replicate_rows = 4,
#'   replication_labels = c("White", "Black", "Asian", "Hispanic"),
#'   csv_file_path = "output/phase1_cleaned.csv"
#' )
#' head(cleaned_phase1)
#' }
#' @export
clean_phase_1_results <- function(phase1_input, replicate_rows = NULL, replication_labels = NULL, csv_file_path = NULL) {
  # Log function start
  log_progress("Starting clean_phase_1_results function.")

  # Validate inputs
  validate_input_data(phase1_input, c("names", "practice_name", "phone_number", "state_name"))

  # Clean column names using tidyverse
  cleaned_input <- phase1_input %>%
    dplyr::mutate_all(~tolower(gsub("\\s+", "_", gsub("[^[:alnum:] ]", "", .))))
  log_progress("Column names cleaned using tidyverse.")

  # Convert column types
  cleaned_input <- safely_convert_column_types(cleaned_input)

  # Handle missing NPI numbers
  cleaned_input <- handle_npi_numbers(cleaned_input)

  # Replicate rows if requested
  if (!is.null(replicate_rows)) {
    cleaned_input <- replicate_rows_with_labels(cleaned_input, replicate_rows, replication_labels)
  }

  # Add unique identifiers
  cleaned_input <- add_unique_identifiers(cleaned_input)

  # Detect academic vs. private practice
  cleaned_input <- detect_academic_practice(cleaned_input)

  # Clean up phone numbers
  cleaned_input <- phone_number_cleanup(cleaned_input)

  # Add timezone column
  cleaned_input <- add_timezone_column(cleaned_input)

  # Unite columns for REDCap upload
  cleaned_input <- unite_columns_for_redcap(cleaned_input)

  # Log a preview of the transformed data
  log_progress("Preview of transformed data:")
  print(dplyr::slice_head(cleaned_input, n = 3))

  # Save the cleaned data to a CSV file
  save_cleaned_data(cleaned_input, csv_file_path)

  # Log function completion
  log_progress("clean_phase_1_results function completed.")
  beepr::beep(2)

  return(cleaned_input)
}



#' clean_phase_1_results <- function(phase1_input, replicate_rows = NULL, replication_labels = NULL, csv_file_path = NULL) {
#'   # Log function call and inputs
#'   conflicted::conflicts_prefer(base::setdiff)
#'   log_progress("Function called: clean_phase_1_results")
#'   log_progress(paste("Input data dimensions:", paste(dim(phase1_input), collapse = "x")))
#'   log_progress(paste("Replicate rows:", ifelse(is.null(replicate_rows), "None", replicate_rows)))
#'   if (!is.null(replication_labels)) {
#'     log_progress(paste("Replication labels:", paste(replication_labels, collapse = ", ")))
#'   }
#'
#'   # Validate inputs with error handling
#'   validate_input_data(phase1_input, c("names", "practice_name", "phone_number", "state_name"))
#'
#'   # Clean column names
#'   cleaned_input <- clean_column_names(phase1_input)
#'
#'   # Convert column types with error handling
#'   cleaned_input <- safely_convert_column_types(cleaned_input)
#'
#'   # Handle missing NPI numbers
#'   cleaned_input <- handle_npi_numbers(cleaned_input)
#'
#'   # Replicate rows if requested
#'   if (!is.null(replicate_rows)) {
#'     cleaned_input <- replicate_rows_with_labels(cleaned_input, replicate_rows, replication_labels)
#'   }
#'
#'   # Add unique identifiers
#'   cleaned_input <- add_unique_identifiers(cleaned_input)
#'
#'   # Detect academic vs. private practice
#'   cleaned_input <- detect_academic_practice(cleaned_input)
#'
#'   # Cleanup phone numbers
#'   cleaned_input <- phone_number_cleanup(cleaned_input)
#'
#'   # Adds time zone as a column based on the state.
#'   cleaned_input <- add_timezone_column(cleaned_input)
#'
#'   # Unite columns for REDCap upload
#'   cleaned_input <- unite_columns_for_redcap(cleaned_input)
#'
#'   # Log a preview of the transformed data
#'   log_progress("Transformed data preview:")
#'   print(dplyr::slice_head(cleaned_input, n = 3))
#'
#'   # Save the cleaned data using write_output_csv
#'   save_cleaned_data(cleaned_input, csv_file_path)
#'
#'   # Log the final output
#'   log_progress("Function clean_phase_1_results completed.")
#'
#'   # Indicate function completion
#'   beepr::beep(2)
#'
#'   return(cleaned_input)
#' }
#'
#' # Internal helper functions with logging and error handling
#'
#' # Utility function for logging
#' log_progress <- function(message) {
#'   cat("[LOG]", message, "\n")
#' }
#'
#' #' @noRd
#' validate_input_data <- function(input_data, required_columns) {
#'   log_progress("Validating input data...")
#'
#'   if (!is.data.frame(input_data)) {
#'     stop("Error: The input must be a data frame.")
#'   }
#'
#'   missing_columns <- setdiff(required_columns, names(input_data))
#'   if (length(missing_columns) > 0) {
#'     stop("Error: Missing columns:", paste(missing_columns, collapse = ", "))
#'   }
#'
#'   log_progress("Input data validation successful.")
#' }
#'
#' #' @noRd
#' clean_column_names <- function(input_data) {
#'   log_progress("Cleaning column names...")
#'   cleaned_input <- janitor::clean_names(input_data)
#'   log_progress("Column names cleaned.")
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' safely_convert_column_types <- function(input_data) {
#'   log_progress("Converting column types...")
#'   tryCatch({
#'     converted_input <- readr::type_convert(input_data)
#'     log_progress("Column types converted successfully.")
#'     return(converted_input)
#'   }, error = function(e) {
#'     stop("Error during column type conversion: ", e$message)
#'   })
#' }
#'
#' #' @noRd
#' handle_npi_numbers <- function(cleaned_input) {
#'   log_progress("Handling missing NPI numbers and creating 'random_id' if needed...")
#'
#'   cleaned_input <- dplyr::mutate(
#'     cleaned_input,
#'     random_id = dplyr::if_else(
#'       is.na(npi),
#'       sample(1000000000:9999999999, size = dplyr::n(), replace = TRUE),
#'       npi
#'     )
#'   )
#'
#'   log_progress("'random_id' generated where necessary.")
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' replicate_rows_with_labels <- function(cleaned_input, replicate_rows, replication_labels) {
#'   log_progress("Replicating rows...")
#'
#'   if (!is.null(replication_labels)) {
#'     if (length(replication_labels) != replicate_rows) {
#'       stop("Error: The number of replication labels must match the number of rows to replicate.")
#'     }
#'   }
#'
#'   cleaned_input <- cleaned_input[rep(1:nrow(cleaned_input), each = replicate_rows), ]
#'
#'   if (!is.null(replication_labels)) {
#'     cleaned_input <- dplyr::mutate(cleaned_input, label_column = rep(replication_labels, length.out = nrow(cleaned_input)))
#'     log_progress("Rows replicated and labels assigned.")
#'   } else {
#'     log_progress("Rows replicated without labels.")
#'   }
#'
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' add_unique_identifiers <- function(cleaned_input) {
#'   log_progress("Adding unique identifiers...")
#'
#'   cleaned_input <- dplyr::mutate(
#'     cleaned_input,
#'     id = dplyr::row_number(),
#'     id_number = paste0("id:", id),
#'     dr_name = paste("Dr.", last)
#'   )
#'
#'   log_progress("Unique identifiers added.")
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' phone_number_cleanup <- function(cleaned_input) {
#'   log_progress("Cleaning up phone numbers...")
#'
#'   cleaned_input <- cleaned_input %>%
#'     dplyr::mutate(
#'       new_phone = gsub("(\\d{3})(\\d{3})(\\d{4})", "\\1-\\2-\\3", phone), # Reformat phone number with dashes
#'       new_phone = stringr::str_replace(new_phone, stringr::regex("\\)", ignore_case = TRUE), "-"), # Replace closing parenthesis with dash
#'       new_phone = stringr::str_remove(new_phone, stringr::regex("^\\(", ignore_case = TRUE)), # Remove opening parenthesis
#'       new_phone = stringr::str_remove(new_phone, stringr::regex(" ", ignore_case = TRUE)) # Remove spaces
#'     )
#'
#'   log_progress("Phone numbers cleaned.")
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' detect_academic_practice <- function(cleaned_input) {
#'   log_progress("Detecting academic or private practice...")
#'
#'   academic_strings <- c(
#'     "Health", "Medical College", "Babies", "Children", "University", "Medical College", "Mayo", "Stanford", "Medical     University", "College", "OHSU", "UMASS", "CU",
#'     "Harvard", "Johns Hopkins", "Cleveland Clinic", "Clinic", "Mount Sinai", "Yale",
#'     "Duke", "UCSF", "UCLA", "NYU", "Weill Cornell", "Vanderbilt", "Emory", "Baylor",
#'     "Northwestern", "Columbia", "Georgetown", "Case Western", "Cedars-Sinai", "Tulane",
#'     "Washington University", "Penn", "UC San Diego", "UC Davis", "UC Irvine",
#'     "University of Chicago", "University of Michigan", "University of Texas",
#'     "University of Pittsburgh", "University of Washington", "University of Miami",
#'     "University of Colorado", "University of Florida", "University of Virginia",
#'     "University of Alabama", "University of Iowa", "University of Maryland",
#'     "University of Minnesota", "University of Wisconsin", "University of Utah",
#'     "University of Cincinnati", "University of Kentucky", "University of Kansas",
#'     "University of Oklahoma", "University of Arizona", "University of South Florida",
#'     "Indiana University", "Oregon Health & Science University", "Medical University of South Carolina"
#'   )
#'
#'   cleaned_input <- dplyr::mutate(
#'     cleaned_input,
#'     academic = dplyr::if_else(
#'       stringr::str_detect(practice_name, stringr::regex(stringr::str_c(academic_strings, collapse = "|"), ignore_case = TRUE)),
#'       "University", "Private Practice"
#'     )
#'   )
#'
#'   log_progress("Academic or private practice type detected.")
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' detect_academics_by_url <- function(data, url_column = "original_data.url") {
#'   # Log the beginning of the function
#'   message("Starting detect_academics_by_url function")
#'   message("Initial number of rows: ", nrow(data))
#'
#'   # Check for the specified URL column
#'   if (!url_column %in% colnames(data)) {
#'     stop("Specified URL column '", url_column, "' not found in the dataframe.")
#'   }
#'
#'   # Process the data with logging
#'   result <- data %>%
#'     dplyr::mutate(
#'       practice_setting = dplyr::if_else(
#'         stringr::str_detect(.data[[url_column]], stringr::regex("\\.org|\\.edu|\\.gov", ignore_case = TRUE)),
#'         "University",
#'         "Private Practice"
#'       )
#'     )
#'
#'   # Log completion and results
#'   message("Completed assigning practice settings.")
#'   message("Number of rows with 'University' setting: ", sum(result$practice_setting == "University"))
#'   message("Number of rows with 'Private Practice' setting: ", sum(result$practice_setting == "Private Practice"))
#'
#'   # Return the processed data
#'   return(result)
#' }
#'
#' #' @noRd
#' add_timezone_column <- function(input_data) {
#'   log_progress("Adding timezone column based on state_name or state_code...")
#'
#'   # Check if 'state_code' exists; if not, map from 'state_name'
#'   if (!"state_code" %in% colnames(input_data)) {
#'     log_progress("No 'state_code' column found, mapping from 'state_name'...")
#'
#'     state_timezones <- tibble::tibble(
#'       state_code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
#'                      "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
#'                      "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
#'                      "WI", "WY", "DC"),
#'       timezone = c("Central", "Alaska", "Mountain", "Central", "Pacific", "Mountain", "Eastern", "Eastern", "Eastern",
#'                    "Eastern", "Hawaii-Aleutian", "Mountain", "Central", "Eastern", "Central", "Central",
#'                    "Eastern", "Central", "Eastern", "Eastern", "Eastern", "Eastern", "Central", "Central", "Central",
#'                    "Mountain", "Central", "Pacific", "Eastern", "Eastern", "Mountain", "Eastern", "Eastern",
#'                    "Central", "Eastern", "Pacific", "Eastern", "Eastern", "Eastern", "Central", "Central",
#'                    "Eastern", "Central", "Mountain", "Eastern", "Eastern", "Pacific", "Eastern",
#'                    "Central", "Mountain", "Eastern")
#'     ) %>%
#'       # Modify the timezone column to append "timezone" after each value
#'       dplyr::mutate(timezone = paste(timezone, "Time Zone"))
#'
#'
#'     state_name_to_code <- tibble::tibble(
#'       state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
#'                      "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
#'                      "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
#'                      "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
#'                      "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
#'                      "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"),
#'       state_code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS","KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV","WI", "WY", "DC"))
#'
#'     input_data <- input_data %>%
#'       dplyr::left_join(state_name_to_code, by = "state_name")
#'   }
#'
#'   # Now perform the join based on the 'state_code' with timezones
#'   input_data_with_timezone <- input_data %>%
#'     dplyr::left_join(state_timezones, by = "state_code") %>%
#'     dplyr::mutate(timezone = ifelse(is.na(timezone), "Unknown", timezone))
#'
#'   log_progress("Timezone column added.")
#'   return(input_data_with_timezone)
#' }
#'
#' #' @noRd
#' unite_columns_for_redcap <- function(cleaned_input) {
#'   log_progress("Uniting columns for REDCap format...")
#'
#'   cleaned_input <- dplyr::mutate(
#'     cleaned_input,
#'     # redcap wants TWO id columns at the start.
#'     for_redcap = paste(id, id, dr_name, phone_number, case_assigned, specialty_primary, state_name, timezone, academic, npi, first, last, sep = ", ")
#'   ) %>%
#'     dplyr::select(for_redcap, dplyr::everything())
#'
#'   log_progress("Columns united for REDCap.")
#'   return(cleaned_input)
#' }
#'
#' #' @noRd
#' save_cleaned_data <- function(cleaned_input, csv_file_path) {
#'   log_progress("Saving cleaned data...")
#'
#'   if (is.null(csv_file_path)) {
#'     output_dir <- "cleaned_data"
#'     current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
#'     csv_file_path <- file.path(output_dir, paste0("clean_phase_1_results_", current_datetime, ".csv"))
#'
#'     if (!dir.exists(output_dir)) {
#'       dir.create(output_dir)
#'       log_progress(paste("Output directory created:", output_dir))
#'     }
#'   }
#'
#'   write_output_csv(cleaned_input, filename = basename(csv_file_path), output_dir = dirname(csv_file_path), verbose = TRUE)
#'   log_progress(paste("Cleaned data saved to:", csv_file_path))
#'
#'   print("You can now upload the `for_redcap` column to your REDCap database. Open up the data in Excel and copy and paste.")
#' }
