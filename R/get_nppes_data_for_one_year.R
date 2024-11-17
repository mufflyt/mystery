#' Process NPPES Data for One Year with Chunked Processing
#'
#' This function processes NPPES (National Plan and Provider Enumeration System) data for a single year.
#' It reads raw data from a CSV file, filters it based on specified taxonomy codes, and writes the cleaned data
#' to an output CSV file. The function processes data in manageable chunks to optimize memory usage, logs progress,
#' and includes auditory feedback for significant stages.
#'
#' @param npi_file_path A character string specifying the path to the raw NPPES data CSV file.
#'   This file should contain the raw data for one year, typically downloaded from NPPES.
#' @param output_csv_path A character string specifying the full file path where the cleaned data will be saved.
#'   The file will be created or overwritten during the process.
#' @param duckdb_file_path A character string specifying the path to the DuckDB database file.
#'   This is used for temporary data storage during processing. Defaults to
#'   `"/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb"`.
#' @param taxonomy_codes_1 A character vector of taxonomy codes used to filter data in
#'   `Healthcare Provider Taxonomy Code_1`. Defaults to common codes for healthcare providers.
#' @param taxonomy_codes_2 A character vector of taxonomy codes used to filter data in
#'   `Healthcare Provider Taxonomy Code_2`. Defaults to the same set as `taxonomy_codes_1`.
#' @param save_column_in_each_nppes_year A logical value indicating whether to save a sample of
#'   the data to an Excel file for each year. Defaults to `FALSE`.
#' @param excel_file_path A character string specifying the path to save the Excel file if
#'   `save_column_in_each_nppes_year` is `TRUE`. Defaults to `NULL`.
#'
#' @return A data frame containing the cleaned NPPES data for one year. The processed data is also saved
#'   to the file specified by `output_csv_path`.
#'
#' @details
#' This function is designed for handling large NPPES datasets. By chunking the data, it avoids
#' memory-related issues while providing a structured way to process and clean the data.
#' Filtering is based on user-specified taxonomy codes for healthcare providers.
#'
#' Logs are generated at each step to track progress, and system beeps are used for auditory feedback
#' (requires the `beepr` package). If `save_column_in_each_nppes_year` is `TRUE`, the function
#' saves a sample of the processed data to an Excel file for verification or further analysis.
#'
#' @export
#'
#' @examples
#' # Example 1: Process data with default taxonomy codes and save to CSV
#' nppes_get_data_for_one_year(
#'   npi_file_path = "path/to/nppes_data_2024.csv",
#'   output_csv_path = "path/to/cleaned_data_2024.csv"
#' )
#'
#' # Example 2: Customize taxonomy codes and process data
#' nppes_get_data_for_one_year(
#'   npi_file_path = "path/to/nppes_data_2023.csv",
#'   output_csv_path = "path/to/cleaned_data_2023.csv",
#'   taxonomy_codes_1 = c("207X00000X", "207Y00000X"),
#'   taxonomy_codes_2 = c("207X00000X", "207Y00000X")
#' )
#'
#' # Example 3: Save a sample of the data to an Excel file
#' nppes_get_data_for_one_year(
#'   npi_file_path = "path/to/nppes_data_2022.csv",
#'   output_csv_path = "path/to/cleaned_data_2022.csv",
#'   save_column_in_each_nppes_year = TRUE,
#'   excel_file_path = "path/to/sample_data_2022.xlsx"
#' )
#'
#' # Example 4: Process data with a custom DuckDB path
#' nppes_get_data_for_one_year(
#'   npi_file_path = "path/to/nppes_data_2021.csv",
#'   output_csv_path = "path/to/cleaned_data_2021.csv",
#'   duckdb_file_path = "custom/path/to/duckdb_database.duckdb"
#' )
#'
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom beepr beep
#'
#' @note Ensure that the `npi_file_path` points to a valid CSV file containing raw NPPES data.
#'       The `taxonomy_codes_1` and `taxonomy_codes_2` parameters should be adjusted as needed
#'       for specific filtering requirements.
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
