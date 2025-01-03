#' Process NPPES Data for One Year with Chunked Processing
#'
#' @param npi_file_path A character string specifying the path to the raw NPPES data CSV file.
#' @param output_csv_path A character string specifying the full file path where the cleaned data will be saved.
#' @param duckdb_file_path A character string specifying the path to the DuckDB database file.
#' @param taxonomy_codes_1 A character vector of taxonomy codes used to filter data in `Healthcare Provider Taxonomy Code_1`.
#' @param taxonomy_codes_2 A character vector of taxonomy codes used to filter data in `Healthcare Provider Taxonomy Code_2`.
#' @param save_column_in_each_nppes_year A logical value indicating whether to save a sample to an Excel file.
#' @param excel_file_path A character string specifying the path to save the Excel file if `save_column_in_each_nppes_year` is `TRUE`.
#' @return A data frame containing the cleaned NPPES data for one year, saved to the specified file path.
#' @importFrom assertthat assert_that is.string
#' @export
nppes_get_data_for_one_year <- function(
    npi_file_path,
    output_csv_path,
    duckdb_file_path = "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb",
    taxonomy_codes_1 = c("207V00000X", "207VB0002X", "207VC0300X", "207VC0200X", "207VX0201X", "207VG0400X", "207VH0002X", "207VM0101X", "207VX0000X", "207VE0102X", "207VF0040X"),
    taxonomy_codes_2 = c("207V00000X", "207VB0002X", "207VC0300X", "207VC0200X", "207VX0201X", "207VG0400X", "207VH0002X", "207VM0101X", "207VX0000X", "207VE0102X", "207VF0040X"),
    save_column_in_each_nppes_year = FALSE,
    excel_file_path = NULL) {
  # Helper function to log messages with timestamps
  log_message <- function(message) {
    cat(sprintf("[%s] %s\n", Sys.time(), message))
  }

  # Validate inputs using assertthat
  assertthat::assert_that(
    assertthat::is.string(npi_file_path) && file.exists(npi_file_path),
    msg = "Error: 'npi_file_path' must be a valid path to an existing file."
  )
  assertthat::assert_that(
    assertthat::is.string(output_csv_path),
    msg = "Error: 'output_csv_path' must be a valid string."
  )
  assertthat::assert_that(
    assertthat::is.string(duckdb_file_path),
    msg = "Error: 'duckdb_file_path' must be a valid string."
  )
  assertthat::assert_that(
    is.character(taxonomy_codes_1) && length(taxonomy_codes_1) > 0,
    msg = "Error: 'taxonomy_codes_1' must be a non-empty character vector."
  )
  assertthat::assert_that(
    is.character(taxonomy_codes_2) && length(taxonomy_codes_2) > 0,
    msg = "Error: 'taxonomy_codes_2' must be a non-empty character vector."
  )
  assertthat::assert_that(
    is.logical(save_column_in_each_nppes_year),
    msg = "Error: 'save_column_in_each_nppes_year' must be a logical value."
  )
  if (save_column_in_each_nppes_year) {
    assertthat::assert_that(
      !is.null(excel_file_path) && assertthat::is.string(excel_file_path),
      msg = "Error: 'excel_file_path' must be a valid string when 'save_column_in_each_nppes_year' is TRUE."
    )
  }

  # Start of function
  log_message("Starting the NPPES data processing for one year...")
  log_message("Inputs:")
  log_message(glue::glue("  npi_file_path: {npi_file_path}"))
  log_message(glue::glue("  output_csv_path: {output_csv_path}"))
  log_message(glue::glue("  duckdb_file_path: {duckdb_file_path}"))
  log_message(glue::glue("  taxonomy_codes_1: {paste(taxonomy_codes_1, collapse = ', ')}"))
  log_message(glue::glue("  taxonomy_codes_2: {paste(taxonomy_codes_2, collapse = ', ')}"))

  # Initialize the environment
  nppes_initialize_environment()

  # Connect to DuckDB
  con <- nppes_connect_to_duckdb(duckdb_file_path)
  on.exit(nppes_disconnect_from_duckdb(con), add = TRUE)

  # Create table from CSV and query sample data
  nppes_create_table_from_csv(con, npi_file_path)

  # If saving sample data to Excel, process and save
  if (save_column_in_each_nppes_year && !is.null(excel_file_path)) {
    nppes_query_sample_data(con, save_column_in_each_nppes_year = TRUE, excel_file_path = excel_file_path)
  } else {
    nppes_query_sample_data(con)
  }

  # Process the table in chunks
  chunk_size <- 100000 # Define chunk size
  total_rows <- nppes_get_total_row_count(con) # Get total row count from table
  num_chunks <- ceiling(total_rows / chunk_size)

  for (i in seq_len(num_chunks)) {
    log_message(glue::glue("Processing chunk {i}/{num_chunks}"))

    # Get the chunk of data
    chunk_data <- nppes_get_chunk(con, chunk_size = chunk_size, chunk_num = i)

    # Process the chunk, clean the data
    processed_data <- nppes_process_npi_table_chunk(chunk_data, taxonomy_codes_1, taxonomy_codes_2)
    cleaned_data <- nppes_collect_and_clean_data(processed_data)

    # Append the chunk to the CSV
    nppes_save_data_to_csv(cleaned_data, output_csv_path, append = TRUE)

    # Clear memory
    rm(processed_data, cleaned_data, chunk_data)
    invisible(gc()) # Garbage collection after processing each chunk
  }

  # Final log message
  log_message("NPPES data processing for one year completed.")
  log_message("The next function to run is `nppes_save_summary_statistics`.")

  # Return the final cleaned data as a data frame (can optionally collect all chunks in memory)
  return(readr::read_csv(output_csv_path))
}
