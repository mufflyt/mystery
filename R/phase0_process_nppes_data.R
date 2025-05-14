#' Import, Filter, and Process NPPES (National Provider Identifier) Data
#'
#' This function imports NPPES data from a CSV file into a DuckDB database, filters
#' the data for individual providers in the US, and optionally saves the filtered
#' data to an RDS file.
#'
#' @param csv_file_path Character string. Path to the NPPES CSV file to import.
#' @param db_path Character string. Path where the DuckDB database file should be
#'        created or accessed.
#' @param raw_table_name Character string. Name for the raw imported table in DuckDB.
#'        Default is "nppes_raw".
#' @param filtered_table_name Character string. Name for the filtered table in DuckDB.
#'        Default is "npidata_filtered".
#' @param save_rds Logical. Whether to save the filtered data to an RDS file.
#'        Default is FALSE.
#' @param rds_output_path Character string. Path where the RDS file should be saved.
#'        Only used if save_rds = TRUE. Default is NULL.
#' @param verbose Logical. Whether to print additional information during processing.
#'        Default is FALSE.
#'
#' @return A list containing connection to the database and the name of the created
#'         table.
#'
#' @examples
#' # Example 1: Import and filter data without saving to RDS
#' nppes_result <- process_nppes_data(
#'   csv_file_path = "path/to/npidata_pfile.csv",
#'   db_path = "path/to/nppes_database.duckdb",
#'   filtered_table_name = "my_nppes_data",
#'   save_rds = FALSE,
#'   verbose = TRUE
#' )
#' # Use the database connection
#' nppes_data <- dplyr::tbl(nppes_result$connection, nppes_result$table_name)
#' # Don't forget to close the connection when finished
#' DBI::dbDisconnect(nppes_result$connection)
#'
#' # Example 2: Import, filter, and save to RDS with custom table names
#' nppes_result <- process_nppes_data(
#'   csv_file_path = "path/to/npidata_pfile.csv",
#'   db_path = "path/to/nppes_database.duckdb",
#'   raw_table_name = "nppes_import_2024",
#'   filtered_table_name = "nppes_physicians_2024",
#'   save_rds = TRUE,
#'   rds_output_path = "path/to/nppes_data.rds",
#'   verbose = TRUE
#' )
#'
#' # Example 3: Using the function with minimal parameters
#' nppes_result <- process_nppes_data(
#'   csv_file_path = "path/to/npidata_pfile.csv",
#'   db_path = "path/to/nppes_database.duckdb"
#' )
#' # Access the data in the database
#' provider_count <- dplyr::tbl(nppes_result$connection, nppes_result$table_name) %>%
#'   dplyr::count() %>%
#'   dplyr::collect() %>%
#'   dplyr::pull(n)
#' # Close the connection
#' DBI::dbDisconnect(nppes_result$connection)
#'
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl filter compute count collect pull
#' @importFrom assertthat assert_that
#' @importFrom logger log_info log_success log_error log_debug
#'
#' @export
process_nppes_data <- function(csv_file_path,
                               db_path,
                               raw_table_name = "nppes_raw",
                               filtered_table_name = "npidata_filtered",
                               save_rds = FALSE,
                               rds_output_path = NULL,
                               verbose = FALSE) {

  # Set up logging
  logger::log_layout(logger::layout_simple)
  log_level <- if (verbose) logger::DEBUG else logger::INFO
  logger::log_threshold(log_level)

  # Validate inputs
  validate_nppes_inputs(csv_file_path, db_path, raw_table_name,
                        filtered_table_name, save_rds, rds_output_path)

  # Log function parameters
  logger::log_info("Starting NPPES data processing with parameters:")
  logger::log_debug("CSV File Path: {csv_file_path}")
  logger::log_debug("DB Path: {db_path}")
  logger::log_debug("Raw Table Name: {raw_table_name}")
  logger::log_debug("Filtered Table Name: {filtered_table_name}")
  logger::log_debug("Save to RDS: {save_rds}")
  if (save_rds) {
    logger::log_debug("RDS Output Path: {rds_output_path}")
  }

  # Setup database connection
  nppes_db_connection <- setup_duckdb_connection(db_path)

  # Import data to DuckDB
  import_nppes_to_duckdb(nppes_db_connection, csv_file_path, raw_table_name)

  # Filter data
  filter_nppes_data(nppes_db_connection, raw_table_name, filtered_table_name)

  # Get row count
  provider_count <- count_nppes_providers(nppes_db_connection, filtered_table_name)
  logger::log_success("Imported and filtered {provider_count} providers to DuckDB table: {filtered_table_name}")

  # Save to RDS if requested
  if (save_rds) {
    save_nppes_to_rds(nppes_db_connection, filtered_table_name, rds_output_path)
  }

  # Return the connection and table name for further use
  return(list(
    connection = nppes_db_connection,
    table_name = filtered_table_name
  ))
}

#' Validate inputs for NPPES data processing
#'
#' @param csv_file_path CSV file path
#' @param db_path DuckDB database path
#' @param raw_table_name Raw table name
#' @param filtered_table_name Filtered table name
#' @param save_rds Whether to save to RDS
#' @param rds_output_path RDS output path
#'
#' @noRd
validate_nppes_inputs <- function(csv_file_path, db_path, raw_table_name,
                                  filtered_table_name, save_rds, rds_output_path) {
  assertthat::assert_that(is.character(csv_file_path),
                          msg = "CSV file path must be a character string")
  assertthat::assert_that(file.exists(csv_file_path),
                          msg = paste("CSV file not found at:", csv_file_path))
  assertthat::assert_that(is.character(db_path),
                          msg = "Database path must be a character string")
  assertthat::assert_that(is.character(raw_table_name),
                          msg = "Raw table name must be a character string")
  assertthat::assert_that(is.character(filtered_table_name),
                          msg = "Filtered table name must be a character string")
  assertthat::assert_that(raw_table_name != filtered_table_name,
                          msg = "Raw and filtered table names must be different")
  assertthat::assert_that(is.logical(save_rds),
                          msg = "save_rds must be logical (TRUE/FALSE)")

  # Validate table names for SQL compatibility
  validate_table_name(raw_table_name)
  validate_table_name(filtered_table_name)

  if (save_rds) {
    assertthat::assert_that(!is.null(rds_output_path),
                            msg = "rds_output_path must be provided when save_rds = TRUE")
    assertthat::assert_that(is.character(rds_output_path),
                            msg = "RDS output path must be a character string")

    # Verify directory exists
    rds_dir <- dirname(rds_output_path)
    assertthat::assert_that(dir.exists(rds_dir),
                            msg = paste("Output directory does not exist:", rds_dir))
  }
}

#' Validate table name for SQL compatibility
#'
#' @param table_name Table name to validate
#'
#' @noRd
validate_table_name <- function(table_name) {
  # Check for valid SQL table name
  assertthat::assert_that(grepl("^[a-zA-Z0-9_]+$", table_name),
                          msg = paste("Invalid table name:", table_name,
                                      "- Use only letters, numbers, and underscores"))
}

#' Set up DuckDB connection
#'
#' @param db_path Path to DuckDB database file
#'
#' @noRd
setup_duckdb_connection <- function(db_path) {
  logger::log_info("Connecting to DuckDB at {db_path}")
  tryCatch({
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
    logger::log_success("Successfully connected to DuckDB")
    return(connection)
  }, error = function(e) {
    logger::log_error("Failed to connect to DuckDB: {e$message}")
    stop("Failed to connect to DuckDB database", call. = FALSE)
  })
}

#' Import NPPES data to DuckDB
#'
#' @param connection DuckDB connection
#' @param csv_file_path Path to NPPES CSV file
#' @param raw_table_name Name for the raw data table
#'
#' @noRd
import_nppes_to_duckdb <- function(connection, csv_file_path, raw_table_name) {
  logger::log_info("Importing NPPES data from CSV to DuckDB table: {raw_table_name}")

  # Check if table already exists
  tables <- DBI::dbListTables(connection)
  if (raw_table_name %in% tables) {
    logger::log_warn("Table {raw_table_name} already exists, dropping it")
    DBI::dbExecute(connection, paste0("DROP TABLE ", raw_table_name))
  }

  import_query <- paste0(
    "CREATE TABLE ", raw_table_name, " AS ",
    "SELECT * FROM read_csv_auto('", csv_file_path, "', ",
    "all_varchar=TRUE, strict_mode=FALSE, ignore_errors=TRUE)"
  )

  tryCatch({
    DBI::dbExecute(connection, import_query)
    logger::log_success("Successfully imported raw data to table: {raw_table_name}")
  }, error = function(e) {
    logger::log_error("Failed to import NPPES data: {e$message}")
    DBI::dbDisconnect(connection)
    stop("Failed to import NPPES data to DuckDB", call. = FALSE)
  })
}

#' Filter NPPES data
#'
#' @param connection DuckDB connection
#' @param raw_table_name Name of the raw data table
#' @param filtered_table_name Name for the filtered data table
#'
#' @noRd
filter_nppes_data <- function(connection, raw_table_name, filtered_table_name) {
  logger::log_info("Filtering NPPES data for individual US providers")

  # Check if filtered table already exists
  tables <- DBI::dbListTables(connection)
  if (filtered_table_name %in% tables) {
    logger::log_warn("Table {filtered_table_name} already exists, dropping it")
    DBI::dbExecute(connection, paste0("DROP TABLE ", filtered_table_name))
  }

  tryCatch({
    filtered_data <- dplyr::tbl(connection, raw_table_name) %>%
      dplyr::filter(
        `Entity Type Code` == "1" &
          `Provider Business Mailing Address Country Code (If outside U.S.)` == "US" &
          `Provider Business Practice Location Address Country Code (If outside U.S.)` == "US"
      )

    filtered_data %>%
      dplyr::compute(name = filtered_table_name, temporary = FALSE)

    logger::log_info("Dropping raw data table to save space")
    DBI::dbExecute(connection, paste0("DROP TABLE ", raw_table_name))

    logger::log_success("Successfully filtered data to table: {filtered_table_name}")
  }, error = function(e) {
    logger::log_error("Failed to filter NPPES data: {e$message}")
    DBI::dbDisconnect(connection)
    stop("Failed to filter NPPES data", call. = FALSE)
  })
}

#' Count NPPES providers
#'
#' @param connection DuckDB connection
#' @param table_name Table name
#'
#' @noRd
count_nppes_providers <- function(connection, table_name) {
  logger::log_info("Counting records in filtered data")

  tryCatch({
    provider_count <- dplyr::tbl(connection, table_name) %>%
      dplyr::count() %>%
      dplyr::collect() %>%
      dplyr::pull(n)

    return(provider_count)
  }, error = function(e) {
    logger::log_error("Failed to count providers: {e$message}")
    return(NA)
  })
}

#' Save NPPES data to RDS file
#'
#' @param connection DuckDB connection
#' @param table_name Table name
#' @param output_path Path to save RDS file
#'
#' @noRd
save_nppes_to_rds <- function(connection, table_name, output_path) {
  logger::log_info("Collecting data for RDS export")

  tryCatch({
    logger::log_debug("Reading data from DuckDB table: {table_name}")
    nppes_provider_data <- dplyr::tbl(connection, table_name) %>%
      dplyr::collect()

    logger::log_info("Saving data to RDS file: {output_path}")
    saveRDS(
      nppes_provider_data,
      file = output_path,
      compress = TRUE
    )

    logger::log_success("Successfully saved data to RDS file: {output_path}")
  }, error = function(e) {
    logger::log_error("Failed to save data to RDS: {e$message}")
    stop("Failed to save NPPES data to RDS file", call. = FALSE)
  })
}
