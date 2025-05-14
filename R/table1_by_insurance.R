#' Create a Table 1 for Physician Demographics by Insurance Acceptance
#'
#' This function creates a descriptive Table 1 of physician demographics with
#' separate columns for each insurance type (MEDICAID, BCBS, Medicare).
#'
#' @param physician_data A data frame containing physician demographic data and
#'        insurance information. Must include an 'insurance' column with values
#'        "MEDICAID", "BCBS", or "Medicare".
#' @param output_file Character string specifying path for output file.
#'        If NULL (default), no file will be created.
#' @param control_settings List of tableby.control settings to customize the table.
#'        If NULL (default), standard settings will be applied.
#' @param verbose Logical. If TRUE (default), generate detailed logging statements.
#'
#' @return An arsenal tableby object containing the formatted Table 1 with
#'         columns for each insurance type.
#'
#' @importFrom dplyr filter select mutate left_join bind_rows distinct
#' @importFrom tidyr pivot_wider
#' @import arsenal
#' @importFrom assertthat assert_that
#' @importFrom logger log_info log_debug log_error
#'
#' @examples
#' # Example 1: Create a basic Table 1 with default settings
#' library(readr)
#'
#' # Read sample data
#' file_path <- "Table_1_all_insurances_included.csv"
#' physician_data <- readr::read_csv(file_path)
#'
#' # Generate Table 1
#' table1_result <- table1_by_insurance(
#'   physician_data = physician_data,
#'   output_file = NULL,
#'   control_settings = NULL,
#'   verbose = TRUE
#' )
#'
#' # Print the results
#' summary(table1_result)
#'
#' # Example 2: Create Table 1 with custom control settings and save to file
#' custom_controls <- arsenal::tableby.control(
#'   test = TRUE,
#'   numeric.test = "kwt",
#'   cat.test = "chisq",
#'   numeric.stats = c("N", "mean", "sd"),
#'   cat.stats = c("countpct"),
#'   digits = 2
#' )
#'
#' table1_custom <- table1_by_insurance(
#'   physician_data = physician_data,
#'   output_file = "physician_demographics_by_insurance.docx",
#'   control_settings = custom_controls,
#'   verbose = TRUE
#' )
#'
#' # Example 3: Create Table 1 with non-default control settings
#' # and examine physician characteristics by insurance acceptance
#' mycontrols <- arsenal::tableby.control(
#'   test = TRUE,
#'   numeric.test = "kwt",
#'   cat.test = "chisq",
#'   numeric.stats = c("N", "medianq1q3"),
#'   cat.stats = c("countpct"),
#'   stats.labels = list(
#'     medianq1q3 = "Median (Q1, Q3)",
#'     N = "n"
#'   ),
#'   digits = 1,
#'   digits.p = 2,
#'   digits.pct = 1,
#'   cat.simplify = FALSE
#' )
#'
#' table1_by_insurance <- table1_by_insurance(
#'   physician_data = physician_data,
#'   output_file = "physician_by_insurance_table1.html",
#'   control_settings = mycontrols,
#'   verbose = TRUE
#' )
table1_by_insurance <- function(physician_data,
                                output_file = NULL,
                                control_settings = NULL,
                                verbose = TRUE) {

  # Set up logging
  logger::log_layout(logger::layout_glue_generator(
    format = "{level} [{time}] {msg}"
  ))

  # Input validation
  assertthat::assert_that(is.data.frame(physician_data),
                          msg = "physician_data must be a data frame")
  assertthat::assert_that("insurance" %in% names(physician_data),
                          msg = "physician_data must contain 'insurance' column")
  assertthat::assert_that("NPI" %in% names(physician_data),
                          msg = "physician_data must contain 'NPI' column")

  if (verbose) {
    logger::log_info("Starting physician demographics Table 1 creation by insurance type")
    logger::log_info("Input data dimensions: {nrow(physician_data)} rows, {ncol(physician_data)} columns")
    logger::log_debug("Column names: {paste(names(physician_data), collapse=', ')}")
  }

  # Process the data to identify physicians by insurance acceptance
  processed_data <- prepare_physician_data(physician_data, verbose = verbose)

  # Create indicator variable for stratification
  insurance_data <- processed_data %>%
    tidyr::pivot_longer(
      cols = c("accepts_MEDICAID", "accepts_BCBS", "accepts_Medicare"),
      names_to = "insurance_type",
      values_to = "acceptance"
    ) %>%
    dplyr::filter(acceptance == "Yes") %>%
    dplyr::mutate(
      insurance_type = dplyr::case_when(
        insurance_type == "accepts_MEDICAID" ~ "MEDICAID",
        insurance_type == "accepts_BCBS" ~ "BCBS",
        insurance_type == "accepts_Medicare" ~ "Medicare",
        TRUE ~ insurance_type
      )
    ) %>%
    dplyr::select(-acceptance)

  # Combine data sets
  overall_data <- processed_data %>%
    dplyr::mutate(insurance_type = "Overall")
  combined_data <- dplyr::bind_rows(insurance_data, overall_data)

  # Clean column names
  combined_data_clean <- clean_column_names(combined_data, verbose = verbose)

  if (verbose) {
    logger::log_info("Created combined dataset with insurance_type as stratification variable")
  }

  # Create stratified table
  final_table <- arsenal::tableby(insurance_type ~ ., data = combined_data_clean, control = control_settings)

  return(final_table)
}
