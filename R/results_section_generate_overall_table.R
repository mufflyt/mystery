#' Generate Overall Table with Error Handling and Logging
#'
#' This function generates an overall table summarizing the demographics of the Table 1 data.
#' It logs all key steps, including inputs, outputs, data transformations, and file paths. The function
#' supports RDS, CSV, and XLS file formats and creates an overall summary table saved as a PDF file.
#'
#' @param input_file_path A string representing the path to the data file (RDS, CSV, or XLS format).
#' @param output_directory A string representing the directory where the output table file will be saved.
#' @param title A string specifying the title for the overall table summary (default is "Overall Table Summary").
#' @param selected_columns An optional vector of selected columns to include in the table. Default is NULL.
#' @param label_translations An optional named list for label translations to use in the table summary.
#' @return Nothing is returned. The function saves the output table as a PDF file.
#' @importFrom arsenal tableby write2pdf
#' @importFrom readr read_rds read_csv
#' @importFrom fs dir_create dir_exists
#' @importFrom dplyr select all_of
#' @importFrom logger log_info log_error
#' @importFrom glue glue
#' @importFrom tools file_ext
#' @export
#'
#' @examples
#' \dontrun{
#' generate_overall_table("data/Table1.rds", "output_tables")
#' generate_overall_table("data/Table1.csv", "output_tables", selected_columns = c("age", "gender"))
#' label_translations <- list(age = "Age (years)", gender = "Gender")
#' generate_overall_table("data/Table1.xlsx", "output_tables",
#'   title = "Demographic Summary",
#'   label_translations = label_translations
#' )
#' }
generate_overall_table <- function(input_file_path, output_directory, title = "Overall Table Summary",
                                   selected_columns = NULL, label_translations = NULL) {
  log_info("Starting generate_overall_table function...")
  log_inputs(input_file_path, output_directory, title, selected_columns, label_translations)

  # Ensure output directory exists
  ensure_output_directory(output_directory)

  # Read input data
  data_input <- read_input_data(input_file_path)

  # Validate data and select columns
  selected_data <- validate_and_select_columns(data_input, selected_columns)

  # Generate the overall table
  overall_table <- generate_table(selected_data)

  # Generate a filename and save the table as a PDF
  save_table_as_pdf(overall_table, output_directory, title, label_translations)

  log_info("Overall table generation completed.")
}

# Helper Functions

#' Save Arsenal Table to PDF
#'
#' This function saves an Arsenal table as a PDF.
#'
#' @param object An Arsenal table object.
#' @param filename The file path for saving the PDF.
#' @export
tm_write2pdf <- function(object, filename) {
  arsenal::write2pdf(object, filename, keep.md = TRUE, quiet = TRUE)
}


#' @noRd
log_inputs <- function(input_file_path, output_directory, title, selected_columns, label_translations) {
  log_info("Input file path: {input_file_path}")
  log_info("Output directory: {output_directory}")
  log_info("Title: {title}")
  log_info("Selected columns: {selected_columns}")
  log_info("Label translations: {label_translations}")
}

#' @noRd
ensure_output_directory <- function(output_directory) {
  if (!fs::dir_exists(output_directory)) {
    log_info("Creating output directory at {output_directory}")
    fs::dir_create(output_directory)
  }
}

#' @noRd
read_input_data <- function(input_file_path) {
  file_extension <- tools::file_ext(input_file_path)
  log_info("Reading data from file: {input_file_path} (file extension: {file_extension})")

  if (!requireNamespace("readxl", quietly = TRUE)) {
    warning("The 'readxl' package is needed.   Please install it.")
  }

  data_input <- tryCatch(
    {
      switch(file_extension,
        "rds" = readr::read_rds(input_file_path),
        "csv" = readr::read_csv(input_file_path),
        "xlsx" = readxl::read_excel(input_file_path),
        {
          log_error("Unsupported file format: {file_extension}")
          stop("Unsupported file format: ", file_extension)
        }
      )
    },
    error = function(e) {
      log_error("Error reading data from {input_file_path}: {e$message}")
      stop("Error reading data.")
    }
  )

  if (nrow(data_input) == 0 || ncol(data_input) == 0) {
    log_error("The input data is empty.")
    stop("The input data is empty.")
  }

  data_input
}

#' @noRd
validate_and_select_columns <- function(data_input, selected_columns) {
  if (!is.null(selected_columns)) {
    log_info("Selecting specific columns: {selected_columns}")
    selected_data <- tryCatch(
      {
        dplyr::select(data_input, dplyr::all_of(selected_columns))
      },
      error = function(e) {
        log_error("Error selecting columns: {e$message}")
        stop("Error selecting columns.")
      }
    )
  } else {
    log_info("Using all columns for the table.")
    selected_data <- data_input
  }

  log_info("Data summary:")
  print(str(selected_data))
  selected_data
}

#' @noRd
generate_table <- function(selected_data) {
  log_info("Generating the overall table with arsenal::tableby")
  tryCatch(
    {
      arsenal::tableby(
        ~.,
        data = selected_data,
        control = tableby.control(
          test = FALSE,
          total = FALSE,
          digits = 0L,
          digits.p = 2L,
          digits.count = 0L,
          numeric.simplify = FALSE,
          cat.simplify = FALSE,
          numeric.stats = c("median", "q1q3"),
          cat.stats = c("countpct"),
          stats.labels = list(
            Nmiss = "N Missing",
            meansd = "Mean (SD)",
            median = "Median",
            medianq1q3 = "Median (Q1, Q3)",
            q1q3 = "Q1, Q3",
            range = "Range",
            countpct = "Count (Pct)"
          )
        )
      )
    },
    error = function(e) {
      log_error("Error generating table with arsenal::tableby: {e$message}")
      stop("Error generating overall table.")
    }
  )
}

#' @noRd
save_table_as_pdf <- function(overall_table, output_directory, title, label_translations) {
  overall_summary <- summary(
    overall_table,
    text = TRUE,
    labelTranslations = label_translations,
    title = title,
    pfootnote = FALSE
  )

  log_info("Overall table summary:")
  print(overall_summary)

  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  filename <- file.path(output_directory, paste("arsenal_overall_table", date_time, ".pdf", sep = ""))

  log_info("Saving the overall table as a PDF: {filename}")
  tm_write2pdf(overall_summary, filename)
}
