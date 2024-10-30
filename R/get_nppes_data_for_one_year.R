#' Get NPPES Data for One Year with Chunked Processing
#'
#' This function processes NPPES data for one year by reading a CSV file, filtering based on taxonomy codes,
#' and writing the results to an output CSV file. It processes the data in chunks to improve memory efficiency
#' and includes system beeps for progress and completion. Logs are provided at each step using the `log_message`
#' function.
#'
#' @param npi_file_path A character string specifying the path to the NPI CSV file. This file is expected to
#'        contain the raw NPPES data for one year.
#' @param output_csv_path A character string specifying the path to save the output cleaned data as a CSV file.
#'        The file will be created or overwritten during the process.
#' @param duckdb_file_path A character string specifying the path to the DuckDB database file. Default is a
#'        predefined file path.
#' @param taxonomy_codes_1 A character vector specifying the taxonomy codes to filter in
#'        `Healthcare Provider Taxonomy Code_1`. Default is a predefined set of taxonomy codes.
#' @param taxonomy_codes_2 A character vector specifying the taxonomy codes to filter in
#'        `Healthcare Provider Taxonomy Code_2`. Default is the same set as `taxonomy_codes_1`.
#' @param save_column_in_each_nppes_year A logical value indicating whether to save a sample of the data to
#'        an Excel file for each year. Default is `FALSE`.
#' @param excel_file_path A character string specifying the path to save the Excel file if
#'        `save_column_in_each_nppes_year` is `TRUE`. Default is `NULL`.
#'
#' @return A cleaned data frame containing the NPPES data for one year. The data is saved to a CSV file
#'         specified by the `output_csv_path` argument.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default taxonomy codes
#' result <- nppes_get_data_for_one_year(
#'   npi_file_path = "/path/to/npi_file.csv",
#'   output_csv_path = "/path/to/output.csv"
#' )
#'
#' # Example 2: Using custom taxonomy codes for filtering
#' result <- nppes_get_data_for_one_year(
#'   npi_file_path = "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/NPPES_Data_Disseminat_September_2024/npidata_pfile_20050523-20240811.csv",
#'   output_csv_path = "/path/to/output.csv",
#'   taxonomy_codes_1 = c("207X00000X", "207Y00000X"),
#'   taxonomy_codes_2 = c("207X00000X", "207Y00000X")
#' )
#'
#' # Example 3: Saving sample data to an Excel file
#' result <- nppes_get_data_for_one_year(
#'   npi_file_path = "/path/to/npi_file.csv",
#'   output_csv_path = "/path/to/output.csv",
#'   save_column_in_each_nppes_year = TRUE,
#'   excel_file_path = "/path/to/sample_data.xlsx"
#' )
#'
#' @details
#' This function processes large datasets in chunks to reduce memory usage, filtering the data based on
#' specified taxonomy codes. It logs progress using the `log_message` helper function and gives auditory feedback with system beeps.
#'
#' The function saves the cleaned data to the specified `output_csv_path` and can optionally save sample data to Excel.
#' After completion, a message is displayed indicating that `nppes_save_summary_statistics` is the next function to run.
#'
nppes_get_data_for_one_year <- function(
    npi_file_path,
    output_csv_path,
    duckdb_file_path = "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb",
    taxonomy_codes_1 = c("207V00000X", "207VB0002X", "207VC0300X", "207VC0200X", "207VX0201X", "207VG0400X", "207VH0002X", "207VM0101X", "207VX0000X", "207VE0102X", "207VF0040X"),
    taxonomy_codes_2 = c("207V00000X", "207VB0002X", "207VC0300X", "207VC0200X", "207VX0201X", "207VG0400X", "207VH0002X", "207VM0101X", "207VX0000X", "207VE0102X", "207VF0040X"),
    save_column_in_each_nppes_year = FALSE,
    excel_file_path = NULL
) {
  # Helper function to log messages with timestamps
  log_message <- function(message) {
    cat(sprintf("[%s] %s\n", Sys.time(), message))
  }

  # Log input parameters
  log_message("Starting the NPPES data processing for one year...")
  log_message("Inputs:")
  log_message(glue::glue("  npi_file_path: {npi_file_path}"))
  log_message(glue::glue("  output_csv_path: {output_csv_path}"))
  log_message(glue::glue("  duckdb_file_path: {duckdb_file_path}"))
  log_message(glue::glue("  taxonomy_codes_1: {paste(taxonomy_codes_1, collapse = ', ')}"))
  log_message(glue::glue("  taxonomy_codes_2: {paste(taxonomy_codes_2, collapse = ', ')}"))

  # Beep to indicate progress
  beepr::beep(1)

  # Initialize the environment
  nppes_initialize_environment()

  # Connect to DuckDB
  con <- nppes_connect_to_duckdb(duckdb_file_path)
  on.exit(nppes_disconnect_from_duckdb(con), add = TRUE)

  # Create table from CSV and query sample data
  nppes_create_table_from_csv(con, npi_file_path)

  # If saving sample data to Excel, run the query and save
  if (save_column_in_each_nppes_year && !is.null(excel_file_path)) {
    nppes_query_sample_data(con, save_column_in_each_nppes_year = TRUE, excel_file_path = excel_file_path)
  } else {
    nppes_query_sample_data(con)
  }

  # Process the table in chunks
  chunk_size <- 100000  # Define chunk size
  total_rows <- nppes_get_total_row_count(con)  # Get total row count from table
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
    invisible(gc())  # Garbage collection after processing each chunk
  }

  # Beep to indicate completion
  beepr::beep(2)

  # Final log message
  log_message("NPPES data processing for one year completed.")
  log_message("The next function to run is `nppes_save_summary_statistics`.")

  # Return the final cleaned data as a data frame (can optionally collect all chunks in memory)
  return(readr::read_csv(output_csv_path))
}
