#' Process NPPES Data for One Year with Chunked Processing
#'
#' This function processes a single year's NPPES (National Plan and Provider Enumeration System) data,
#' filtering by specified taxonomy codes and saving the cleaned data to a CSV file. The data is processed
#' in chunks to handle large datasets efficiently, and optional functionality allows saving sample columns
#' to an Excel file for inspection.
#'
#' @param npi_file_path A character string specifying the path to the raw NPPES data CSV file.
#' @param output_csv_path A character string specifying the full file path where the cleaned data will be saved.
#' @param duckdb_file_path A character string specifying the path to the DuckDB database file.
#'   Default is "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb".
#' @param taxonomy_codes_1 A character vector of taxonomy codes used to filter rows based on
#'   `Healthcare Provider Taxonomy Code_1`.
#' @param taxonomy_codes_2 A character vector of taxonomy codes used to filter rows based on
#'   `Healthcare Provider Taxonomy Code_2`.
#' @param save_column_in_each_nppes_year A logical value indicating whether to save sample data
#'   to an Excel file for inspection. Default is `FALSE`.
#' @param excel_file_path A character string specifying the path to save the Excel file if
#'   `save_column_in_each_nppes_year` is `TRUE`. Default is `NULL`.
#'
#' @return A dataframe containing the cleaned NPPES data for one year. The data is also saved
#' to the specified CSV file.
#'
#' @details
#' This function processes large NPPES datasets by leveraging DuckDB for efficient chunk-wise
#' processing. The data is filtered based on taxonomy codes and cleaned before being appended
#' to the output CSV file. The function optionally saves a sample of the data to an Excel file
#' for inspection.
#'
#' @examples
#' # Example 1: Process NPPES data for one year and save to CSV
#' \dontrun{
#' nppes_get_data_for_one_year(
#'   npi_file_path = "nppes_raw_2022.csv",
#'   output_csv_path = "nppes_cleaned_2022.csv"
#' )
#' }
#'
#' # Example 2: Process NPPES data and filter by taxonomy codes
#' \dontrun{
#' nppes_get_data_for_one_year(
#'   npi_file_path = "nppes_raw_2022.csv",
#'   output_csv_path = "nppes_cleaned_filtered_2022.csv",
#'   taxonomy_codes_1 = c("207V00000X", "207VG0400X"),
#'   taxonomy_codes_2 = c("207V00000X", "207VG0400X")
#' )
#' }
#'
#' # Example 3: Save sample data to Excel for inspection
#' \dontrun{
#' nppes_get_data_for_one_year(
#'   npi_file_path = "nppes_raw_2022.csv",
#'   output_csv_path = "nppes_cleaned_2022.csv",
#'   save_column_in_each_nppes_year = TRUE,
#'   excel_file_path = "nppes_sample_2022.xlsx"
#' )
#' }
#'
#' @importFrom dplyr filter mutate select
#' @importFrom readr read_csv write_csv
#' @importFrom glue glue
#' @importFrom duckdb dbConnect dbDisconnect
#' @import assertthat
#'
#' @export
nppes_get_data_for_one_year <- function(
    npi_file_path,
    output_csv_path,
    duckdb_file_path = "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb",
    taxonomy_codes_1 = c(
      "207V00000X", "207VB0002X", "207VC0300X", "207VC0200X", "207VX0201X",
      "207VG0400X", "207VH0002X", "207VM0101X", "207VX0000X", "207VE0102X",
      "207VF0040X"
    ),
    taxonomy_codes_2 = c(
      "207V00000X", "207VB0002X", "207VC0300X", "207VC0200X", "207VX0201X",
      "207VG0400X", "207VH0002X", "207VM0101X", "207VX0000X", "207VE0102X",
      "207VF0040X"
    ),
    save_column_in_each_nppes_year = FALSE,
    excel_file_path = NULL) {
  # Validate inputs using assertthat
  assertthat::assert_that(file.exists(npi_file_path), msg = "`npi_file_path` must be a valid file path.")
  assertthat::assert_that(assertthat::is.string(output_csv_path), msg = "`output_csv_path` must be a string.")
  assertthat::assert_that(assertthat::is.string(duckdb_file_path), msg = "`duckdb_file_path` must be a string.")
  assertthat::assert_that(is.character(taxonomy_codes_1), msg = "`taxonomy_codes_1` must be a character vector.")
  assertthat::assert_that(is.character(taxonomy_codes_2), msg = "`taxonomy_codes_2` must be a character vector.")
  assertthat::assert_that(is.logical(save_column_in_each_nppes_year), msg = "`save_column_in_each_nppes_year` must be logical.")
  if (save_column_in_each_nppes_year) {
    assertthat::assert_that(!is.null(excel_file_path), msg = "`excel_file_path` must be provided when saving sample data.")
    assertthat::assert_that(assertthat::is.string(excel_file_path), msg = "`excel_file_path` must be a string.")
  }

  # Log initialization
  log_message <- function(message) {
    cat(sprintf("[%s] %s\n", Sys.time(), message))
  }
  log_message("Starting NPPES data processing for one year...")

  # Initialize DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), duckdb_file_path)
  on.exit(duckdb::dbDisconnect(con), add = TRUE)

  # Create a DuckDB table from the CSV
  query <- glue::glue("CREATE OR REPLACE TABLE nppes_data AS SELECT * FROM read_csv_auto('{npi_file_path}')")
  DBI::dbExecute(con, query)

  # Save a sample of data if required
  if (save_column_in_each_nppes_year) {
    sample_query <- "SELECT * FROM nppes_data LIMIT 100"
    sample_data <- DBI::dbGetQuery(con, sample_query)
    writexl::write_xlsx(sample_data, path = excel_file_path)
    log_message(glue::glue("Sample data saved to {excel_file_path}"))
  }

  # Process data in chunks
  total_rows_query <- "SELECT COUNT(*) FROM nppes_data"
  total_rows <- DBI::dbGetQuery(con, total_rows_query)[1, 1]
  chunk_size <- 100000
  num_chunks <- ceiling(total_rows / chunk_size)

  for (i in seq_len(num_chunks)) {
    log_message(glue::glue("Processing chunk {i}/{num_chunks}"))

    chunk_query <- glue::glue("SELECT * FROM nppes_data LIMIT {chunk_size} OFFSET {(i - 1) * chunk_size}")
    chunk <- DBI::dbGetQuery(con, chunk_query)

    # Filter by taxonomy codes
    filtered_chunk <- dplyr::filter(
      chunk,
      `Healthcare Provider Taxonomy Code_1` %in% taxonomy_codes_1 |
        `Healthcare Provider Taxonomy Code_2` %in% taxonomy_codes_2
    )

    # Append filtered chunk to output CSV
    readr::write_csv(filtered_chunk, output_csv_path, append = i > 1)
  }

  log_message(glue::glue("NPPES data processing completed. Cleaned data saved to {output_csv_path}"))
  return(readr::read_csv(output_csv_path))
}
