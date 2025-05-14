#' Generate overall table with optional stratification
#'
#' Generate an overall table summarizing demographics, with optional stratification
#' by a specified variable. This function creates a publication-ready table suitable
#' for inclusion in manuscripts and reports.
#'
#' @param input_file_path The path to the data file (in RDS, CSV, or XLS format).
#' @param output_directory The directory where the output table file will be saved.
#' @param title The title for the table (default is "Overall Table Summary").
#' @param strata_var Optional character string naming the stratification variable.
#'   If provided, the table will be stratified by this variable.
#' @param selected_columns Optional vector of selected columns to include in the table.
#'   If NULL, all columns (except strata_var if provided) will be used.
#' @param label_translations Optional named list for label translations to provide
#'   more descriptive variable names in the output table.
#'
#' @return Invisibly returns the summary object from arsenal::tableby.
#'
#' @details
#' This function creates a table summarizing demographics and other variables in a dataset.
#' If a stratification variable is provided, the table will show statistics separately
#' for each level of that variable, with statistical tests comparing the groups.
#' If no stratification variable is provided, an overall summary table is created.
#'
#' The function handles special characters in variable names and limits the number
#' of columns to avoid formula length issues. The output is saved as a PDF file
#' with a timestamp in the filename.
#'
#' @examples
#' \dontrun{
#' # Generate a simple overall table without stratification
#' generate_overall_table(
#'   input_file_path = "data/urodynamics_data.rds",
#'   output_directory = "tables",
#'   title = "Demographics Summary"
#' )
#'
#' # Generate a table stratified by "Was the procedure cancelled?"
#' generate_overall_table(
#'   input_file_path = "data/urodynamics_data.rds",
#'   output_directory = "tables",
#'   title = "Demographics Stratified by Procedure Cancellation Status",
#'   strata_var = "Was the procedure cancelled?"
#' )
#'
#' # Generate a table with specific columns and label translations
#' label_trans <- list(
#'   "Age:" = "Patient Age (years)",
#'   "BMI" = "Body Mass Index (kg/m²)"
#' )
#' generate_overall_table(
#'   input_file_path = "data/urodynamics_data.rds",
#'   output_directory = "tables",
#'   selected_columns = c("Age:", "BMI", "Race:", "Tobacco use:"),
#'   label_translations = label_trans
#' )
#' }
#'
#' @importFrom arsenal write2pdf tableby
#' @importFrom readr read_rds
#' @importFrom easyr read.any
#' @importFrom fs dir_create dir_exists
#' @importFrom logger log_info log_error log_debug log_warn
#' @importFrom stats as.formula
#' @export
generate_overall_table <- function(input_file_path,
                                   output_directory,
                                   title = "Overall Table Summary",
                                   strata_var = NULL,
                                   selected_columns = NULL,
                                   label_translations = NULL) {
  # Set up logger
  logger::log_formatter(logger::formatter_glue)
  logger::log_threshold(logger::INFO)

  # Log function start and inputs
  logger::log_info("Starting generate_overall_table function...")
  logger::log_info("Function call details:")
  logger::log_info("  - Input file: {input_file_path}")
  logger::log_info("  - Output directory: {output_directory}")
  logger::log_info("  - Title: {title}")

  if (!is.null(strata_var)) {
    logger::log_info("  - Stratification variable: {strata_var}")
  } else {
    logger::log_info("  - No stratification variable provided (overall table)")
  }

  # Ensure the output directory exists
  if (!fs::dir_exists(output_directory)) {
    logger::log_info("Creating output directory: {output_directory}")
    fs::dir_create(output_directory)
  }

  # Read the data
  logger::log_info("Reading data from file: {input_file_path}")
  tryCatch({
    data <- readr::read_rds(input_file_path)
  }, error = function(e) {
    logger::log_error("Error reading RDS file: {e$message}")
    logger::log_info("Attempting to read with easyr::read.any...")
    data <- easyr::read.any(input_file_path)
  })

  # Check if the data is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    error_msg <- "The input data is empty."
    logger::log_error(error_msg)
    stop(error_msg)
  }

  # Validate stratification variable if provided
  if (!is.null(strata_var)) {
    if (!strata_var %in% names(data)) {
      error_msg <- paste0("Stratification variable '", strata_var, "' not found in the dataset.")
      logger::log_error(error_msg)
      stop(error_msg)
    }
  }

  # Check if selected_columns argument is provided
  if (is.null(selected_columns)) {
    # If not provided, use all columns in the data except the strata variable (if any)
    logger::log_info("Using all columns in the data for the table.")
    selected_columns <- names(data)
    if (!is.null(strata_var)) {
      selected_columns <- setdiff(selected_columns, strata_var)
    }
  }

  # Use at most 30 columns to avoid formula being too complex
  if (length(selected_columns) > 30) {
    logger::log_warn("Too many variables selected. Limiting to first 30 columns.")
    selected_columns <- selected_columns[1:30]
  }

  # Prepare the formula for arsenal::tableby with backticks to handle special characters
  if (!is.null(strata_var)) {
    # Stratified table: strata_var ~ var1 + var2 + var3
    formula_str <- paste0("`", strata_var, "`", " ~ ", paste(paste0("`", selected_columns, "`"), collapse = " + "))
    logger::log_info("Creating stratified table with formula: {formula_str}")
  } else {
    # Overall table: ~ var1 + var2 + var3
    formula_str <- paste0("~ ", paste(paste0("`", selected_columns, "`"), collapse = " + "))
    logger::log_info("Creating overall table with formula: {formula_str}")
  }

  formula_obj <- stats::as.formula(formula_str)

  # Configure arsensal::tableby control parameters
  logger::log_info("Configuring table control parameters...")
  control_params <- arsenal::tableby.control(
    test = !is.null(strata_var),  # Include statistical tests only for stratified tables
    total = !is.null(strata_var), # Include total column only for stratified tables
    digits = 1L,
    digits.p = 3L,
    digits.count = 0L,
    numeric.stats = c("median", "q1q3"),
    cat.stats = c("countpct"),
    stats.labels = list(
      Nmiss = "N Missing",
      meansd = "Mean (SD)",
      medianq1q3 = "Median (Q1, Q3)",
      q1q3 = "Q1, Q3",
      countpct = "Count (%)"
    )
  )

  # Generate the table using arsenal::tableby
  logger::log_info("Generating the table using arsenal::tableby...")
  overall_table <- tryCatch({
    arsenal::tableby(
      formula_obj,
      data = data,
      control = control_params
    )
  }, error = function(e) {
    logger::log_error("Error generating the table: {e$message}")
    stop(paste("Error generating the table:", e$message))
  })

  # Generate the summary of the table
  logger::log_info("Generating the summary of the table...")
  table_summary <- summary(
    overall_table,
    text = TRUE,
    labelTranslations = label_translations,
    title = title,
    pfootnote = !is.null(strata_var)  # Include p-value footnote only for stratified tables
  )

  # Access the current date and time
  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Create the filename with function name and date-time
  if (!is.null(strata_var)) {
    # For stratified tables
    filename <- file.path(output_directory, paste0("stratified_table_",
                                                   gsub("[^a-zA-Z0-9]", "", strata_var),
                                                   "_", date_time))
  } else {
    # For overall tables
    filename <- file.path(output_directory, paste0("overall_table_", date_time))
  }
  logger::log_info("Output filename: {filename}")

  # Save the table as a PDF
  logger::log_info("Saving the table as a PDF: {filename}")
  arsenal::write2pdf(table_summary, filename, keep.md = TRUE, quiet = TRUE)

  # Log function end
  logger::log_info("Table generation completed.")

  # Return the summary object invisibly
  return(invisible(table_summary))
}
