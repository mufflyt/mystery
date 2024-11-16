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
#' @importFrom arsenal write2pdf tableby
#' @importFrom readr read_rds read_csv
#' @importFrom readxl read_excel
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
#' generate_overall_table("data/Table1.xlsx", "output_tables", title = "Demographic Summary",
#'                        label_translations = label_translations)
#' }

tm_write2pdf <- function(object, filename) {
  log_info("Saving Arsenal Table as a PDF at {filename}")
  arsenal::write2pdf(object, filename, keep.md = TRUE, quiet = TRUE)
}

generate_overall_table <- function(input_file_path, output_directory, title = "Overall Table Summary",
                                   selected_columns = NULL, label_translations = NULL) {

  # Log the start of the function and inputs
  log_info("Starting generate_overall_table function...")
  log_info("Input file path: {input_file_path}")
  log_info("Output directory: {output_directory}")
  log_info("Title: {title}")
  log_info("Selected columns: {selected_columns}")
  log_info("Label translations: {label_translations}")

  # Ensure output directory exists
  if (!fs::dir_exists(output_directory)) {
    log_info("Creating output directory at {output_directory}")
    fs::dir_create(output_directory)
  }

  # Read the input data based on file extension
  file_extension <- tools::file_ext(input_file_path)
  log_info("Reading data from file: {input_file_path} (file extension: {file_extension})")

  # Read data using appropriate functions
  data_input <- tryCatch({
    switch(
      file_extension,
      "rds" = readr::read_rds(input_file_path),
      "csv" = readr::read_csv(input_file_path),
      "xlsx" = readxl::read_excel(input_file_path),
      {
        log_error("Unsupported file format: {file_extension}")
        stop("Unsupported file format: ", file_extension)
      }
    )
  }, error = function(e) {
    log_error("Error reading data from {input_file_path}: {e$message}")
    stop("Error reading data.")
  })

  # Ensure the data is not empty
  if (nrow(data_input) == 0 || ncol(data_input) == 0) {
    log_error("The input data is empty.")
    stop("The input data is empty.")
  }

  # Select specific columns if provided
  if (!is.null(selected_columns)) {
    log_info("Selecting specific columns: {selected_columns}")
    selected_data <- tryCatch({
      dplyr::select(data_input, dplyr::all_of(selected_columns))
    }, error = function(e) {
      log_error("Error selecting columns: {e$message}")
      stop("Error selecting columns.")
    })
  } else {
    log_info("Using all columns for the table.")
    selected_data <- data_input
  }

  # Log a brief summary of the data
  log_info("Data summary:")
  print(str(selected_data))

  # Generate the overall table using arsenal::tableby
  log_info("Generating the overall table with arsenal::tableby")
  overall_table <- tryCatch({
    arsenal::tableby(
      ~ .,
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
  }, error = function(e) {
    log_error("Error generating table with arsenal::tableby: {e$message}")
    stop("Error generating overall table.")
  })

  # Generate and log summary of the overall table
  log_info("Generating summary of the overall table.")
  overall_summary <- summary(
    overall_table,
    text = TRUE,
    labelTranslations = label_translations,
    title = title,
    pfootnote = FALSE
  )

  # Log the overall summary
  log_info("Overall table summary:")
  print(overall_summary)

  # Generate a filename with the current date and time
  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  filename <- file.path(output_directory, paste("arsenal_overall_table", date_time, ".pdf", sep = ""))

  # Save the overall table as a PDF
  log_info("Saving the overall table as a PDF: {filename}")
  tm_write2pdf(overall_summary, filename)

  # Log function completion
  log_info("Overall table generation completed.")
}
