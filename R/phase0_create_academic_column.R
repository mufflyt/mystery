#' Create an Academic Column Based on Facility Names
#'
#' This function adds a new column, `academic`, to the provided data frame.
#' The column is populated based on the presence of specific academic terms
#' (such as "University", "Medical School", "Health System", etc.) in
#' specified columns. Rows where no match is found will have the value
#' "Private Practice", and rows where all the specified columns are `NA`
#' will have the value `NA`.
#'
#' The logic behind this categorization is as follows:
#'
#' - **"University"**: If any of the specified columns contain any of the
#' academic terms (case-insensitive) defined in the `search_strings` argument,
#' then that row is labeled as "University".
#'
#' - **"Private Practice"**: If no academic terms are found in the specified
#' columns for a row, then the row is labeled as "Private Practice".
#'
#' - **NA**: If all specified columns for a row contain `NA` values, the
#' `academic` column for that row will remain `NA`.
#'
#' This function is a good example of searching for data within multiple columns and assigning a value to a new output column based on whether specific terms are present.
#' It demonstrates how multiple columns can be examined simultaneously, and how a new categorical variable (`academic`) is derived based on keyword matching in these columns.
#'
#' @param input_data A data frame containing the data to process.
#' @param columns_to_search A character vector of column names to search
#' for academic terms.
#' @param academic_keywords A character vector of keywords representing
#' academic institutions or terms. The default includes common academic
#' terms such as "University", "Medical College", etc.
#' @param verbose Logical. If TRUE, detailed logging of the process will
#' be printed to the console. Default is FALSE.
#'
#' @return The input data frame with a new column `academic` indicating
#' whether the facility is academic or private practice, or `NA` if all
#' specified columns are `NA`.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_detect str_trim regex str_c
#' @importFrom logger log_info log_warn log_error log_debug
#'
#' @examples
#' # Example 1: Basic usage with default academic keywords and verbose logging
#' clinician_data <- data.frame(
#'   clinician_info_facility_name = c("University of California", "Mayo Clinic",
#'                                    "Private Clinic", NA),
#'   addresses_address_1 = c("200 Medical Ave", "123 Health St", "456 Private Rd", NA)
#' )
#' df_with_academic <- phase0_create_academic_column(clinician_data,
#'                                            c("clinician_info_facility_name",
#'                                              "addresses_address_1"),
#'                                            verbose = TRUE)
#' print(df_with_academic)
#'
#' # Example 2: Using custom academic keywords and suppressing verbose logging
#' custom_keywords <- c("Harvard", "Stanford", "Yale", "Medical School")
#' clinician_data <- data.frame(
#'   clinician_info_facility_name = c("Harvard Medical School", "Private Clinic"),
#'   addresses_address_1 = c("1 Medical Center", "123 Private Rd")
#' )
#' df_with_academic <- phase0_create_academic_column(clinician_data,
#'                                            c("clinician_info_facility_name",
#'                                              "addresses_address_1"),
#'                                            academic_keywords = custom_keywords,
#'                                            verbose = FALSE)
#' print(df_with_academic)
#'
#' # Example 3: Case with all columns as NA, resulting in NA in the academic column
#' clinician_data <- data.frame(
#'   clinician_info_facility_name = c(NA, NA),
#'   addresses_address_1 = c(NA, NA)
#' )
#' df_with_academic <- phase0_create_academic_column(clinician_data,
#'                                            c("clinician_info_facility_name",
#'                                              "addresses_address_1"),
#'                                            verbose = FALSE)
#' print(df_with_academic)
#' @export
phase0_create_academic_column <- function(input_data,
                                          columns_to_search,
                                          academic_keywords = c(
                                            "Medical College",
                                            "University of",
                                            "University",
                                            "Univ",
                                            "Children's",
                                            "Infirmary",
                                            "Medical School",
                                            "Medical Center",
                                            "Children",
                                            "Health System",
                                            "Foundation",
                                            "Sch of Med",
                                            "Mayo",
                                            "UAB",
                                            "Cancer Ctr",
                                            "Penn",
                                            "College of Medicine",
                                            "Cancer",
                                            "Cleveland Clinic",
                                            "Henry Ford",
                                            "Yale",
                                            "Brigham",
                                            "Health Sciences Center",
                                            "SUNY",
                                            "UCLA",
                                            "UAMS",
                                            "Medical Sciences",
                                            "Cedars",
                                            "Stanford",
                                            "George Washington",
                                            "Medstar",
                                            "USF",
                                            "GME",
                                            "Emory",
                                            "Morehouse",
                                            "Mercer",
                                            "Loyola",
                                            "Duke"
                                          ),
                                          private_practice_keywords = c(
                                            "LLC", "INC", "P.A.", "CORPORATION", "PLLC",
                                            "PARTNERSHIP", "LTD", "P C", "PS", "L L P",
                                            "INCORPORATED", "DO", "MD", "MBA", "PA"
                                          ),
                                          verbose = FALSE) {
  logger::log_info("Starting function 'phase0_create_academic_column'.")
  logger::log_info("Input data dimensions: {nrow(input_data)} rows, {ncol(input_data)} columns.")
  logger::log_info("Columns to search in: {paste(columns_to_search, collapse = ', ')}")
  logger::log_info("Academic search keywords: {paste(academic_keywords, collapse = ', ')}")
  logger::log_info("Private practice search keywords: {paste(private_practice_keywords, collapse = ', ')}")

  assertthat::assert_that(is.data.frame(input_data), msg = "The input data is not a valid data frame.")
  assertthat::assert_that(is.character(columns_to_search), msg = "The 'columns_to_search' argument must be a character vector.")

  # Verify that all columns in columns_to_search exist
  missing_columns <- setdiff(columns_to_search, colnames(input_data))
  if (length(missing_columns) > 0) {
    stop(
      glue::glue(
        "The following columns are missing from the input data: {paste(missing_columns, collapse = ', ')}"
      )
    )
  }

  # Perform the academic classification
  input_data <- input_data %>%
    dplyr::mutate(
      academic = dplyr::case_when(
        rowSums(is.na(input_data[, columns_to_search])) == length(columns_to_search) ~ NA_character_,
        rowSums(sapply(input_data[, columns_to_search], function(x) {
          if (verbose)
            logger::log_debug("Checking column: {deparse(substitute(x))}")
          stringr::str_detect(stringr::str_trim(x),
                              stringr::regex(
                                stringr::str_c(academic_keywords, collapse = "|"),
                                ignore_case = TRUE
                              ))
        })) > 0 ~ "University",
        TRUE ~ "Private Practice"
      )
    )

  logger::log_info("Transformation completed. New column 'academic' added to the data frame.")
  logger::log_info("Output data dimensions: {nrow(input_data)} rows, {ncol(input_data)} columns.")
  return(input_data)
}
