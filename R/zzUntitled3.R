#' Process Open Payments Data with Optimized Database Operations
#'
#' @description
#' Processes Open Payments data files and loads them into a DuckDB database with
#' optimized performance. The function handles file discovery, metadata tracking,
#' incremental loading, and column mapping across different years of Open Payments data.
#'
#' @param base_dir Character string specifying the base directory containing Open Payments files.
#' @param db_path Character string specifying the path to the DuckDB database file.
#' @param years Character vector of years to process (e.g., c("2019", "2020", "2021")).
#' @param payment_type Character string specifying the payment type: "general", "research", or "ownership".
#' @param output_table_name Character string specifying the name for the output table.
#' @param force_reimport Logical, if TRUE will reimport all files even if they exist in the database.
#' @param file_pattern Character string with the file pattern to match. Default adapts to payment_type.
#' @param verbose Logical, if TRUE (default) will log detailed information.
#'
#' @return Character string with the name of the created merged table.
#'
#' @importFrom logger log_info log_success log_error log_warn log_debug log_threshold INFO ERROR
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select distinct
#' @importFrom duckdb duckdb dbConnect
#' @importFrom DBI dbExecute dbGetQuery dbListTables dbDisconnect
#' @importFrom purrr map
#' @importFrom fs file_size dir_ls
#'
#' @examples
#' # Basic usage with default settings for general payments
#' merged_table <- process_open_payments_data_optimized(
#'   base_dir = "/path/to/open_payments_data",
#'   db_path = "/path/to/output.duckdb",
#'   years = c("2021", "2022", "2023"),
#'   payment_type = "general",
#'   output_table_name = "op_general_payments",
#'   verbose = TRUE
#' )
#' 
#' # Process research payments with forced reimport
#' research_table <- process_open_payments_data_optimized(
#'   base_dir = "/path/to/open_payments_data",
#'   db_path = "/path/to/output.duckdb",
#'   years = c("2021", "2022"),
#'   payment_type = "research",
#'   output_table_name = "op_research_payments",
#'   force_reimport = TRUE,
#'   verbose = TRUE
#' )
#' 
#' # Process ownership data with custom file pattern and limited logging
#' ownership_table <- process_open_payments_data_optimized(
#'   base_dir = "/path/to/open_payments_data",
#'   db_path = "/path/to/output.duckdb",
#'   years = c("2020", "2021", "2022"),
#'   payment_type = "ownership",
#'   output_table_name = "op_ownership_interests",
#'   file_pattern = ".*OWNRSHP.*\\.csv$",
#'   force_reimport = FALSE,
#'   verbose = FALSE
#' )
process_open_payments_data_optimized <- function(base_dir, 
                                                 db_path, 
                                                 years = NULL, 
                                                 payment_type = "general", 
                                                 output_table_name = NULL, 
                                                 force_reimport = FALSE,
                                                 file_pattern = NULL,
                                                 verbose = TRUE) {
  # Initialize logger
  logger::log_threshold(ifelse(verbose, logger::INFO, logger::ERROR))
  
  # Log function start
  logger::log_info("Starting Open Payments data processing")
  logger::log_debug("Validating input parameters")
  
  # Validate input parameters
  assertthat::assert_that(is.character(base_dir))
  assertthat::assert_that(is.character(db_path))
  assertthat::assert_that(is.character(payment_type))
  assertthat::assert_that(payment_type %in% c("general", "research", "ownership"))
  assertthat::assert_that(is.logical(force_reimport))
  assertthat::assert_that(is.logical(verbose))
  
  if (!is.null(years)) {
    assertthat::assert_that(is.character(years))
  }
  
  if (is.null(output_table_name)) {
    # Generate default output table name based on payment type
    output_table_name <- paste0("op_", payment_type, "_all_years")
    logger::log_info("Using default output table name: {output_table_name}")
  } else {
    assertthat::assert_that(is.character(output_table_name))
  }
  
  # Determine file pattern based on payment type if not provided
  if (is.null(file_pattern)) {
    if (payment_type == "general") {
      file_pattern <- ".*_GNRL.*\\.csv$"
    } else if (payment_type == "research") {
      file_pattern <- ".*_RSRCH.*\\.csv$"
    } else if (payment_type == "ownership") {
      file_pattern <- ".*_OWNRSHP.*\\.csv$"
    }
    logger::log_info("Using file pattern: {file_pattern}")
  }
  
  # Connect to DuckDB
  logger::log_info("Connecting to DuckDB at {db_path}")
  connection <- connect_to_duckdb(db_path, verbose)
  
  # Initialize database if necessary
  initialize_database(connection, verbose)
  
  # Find Open Payments files
  payment_files <- find_payment_files(base_dir, years, file_pattern, verbose)
  
  # Check which files are already imported
  payment_files <- check_imported_files(connection, payment_files, verbose)
  
  # Import files to DuckDB
  imported_tables <- import_files_to_duckdb(connection, payment_files, verbose, 
                                            force_reimport, file_pattern)
  
  # Determine which columns to include in the merged data
  key_columns <- get_key_columns(payment_type)
  
  # Create column mapping across years
  column_mapping <- create_column_mapping(connection, 
                                          imported_tables$new_tables, 
                                          imported_tables$existing_tables, 
                                          key_columns, 
                                          verbose)
  
  # Merge payment data from all years
  merged_table <- merge_payment_data(connection, 
                                     imported_tables$new_tables, 
                                     imported_tables$existing_tables, 
                                     column_mapping, 
                                     output_table_name, 
                                     verbose)
  
  # Check for any missing tables
  table_names <- c(unlist(imported_tables$new_tables), 
                   unlist(imported_tables$existing_tables), 
                   merged_table)
  table_names <- table_names[!is.na(table_names)]
  
  # Verify tables exist
  existing_tables <- DBI::dbListTables(connection)
  missing_tables <- table_names[!table_names %in% existing_tables]
  
  # Report results
  if (length(missing_tables) == 0) {
    logger::log_info("✅ All {length(table_names)} tables were successfully created and are available.")
  } else {
    logger::log_error("❌ Missing tables detected: {paste(missing_tables, collapse = ', ')}")
    DBI::dbDisconnect(connection)
    stop("Some tables were not created successfully. Review the errors above.")
  }
  
  # Disconnect from database
  logger::log_info("Disconnecting from database")
  DBI::dbDisconnect(connection)
  
  # Return the merged table name
  return(merged_table)
}

#' Connect to DuckDB
#'
#' @param db_path Path to DuckDB database file
#' @param verbose Whether to print verbose output
#'
#' @return DuckDB connection
#' @noRd
connect_to_duckdb <- function(db_path, verbose) {
  tryCatch({
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
    logger::log_success("Connected to DuckDB database at {db_path}")
    return(connection)
  }, error = function(e) {
    logger::log_error("Failed to connect to DuckDB: {e$message}")
    stop("Database connection failed")
  })
}

#' Initialize database tables and schema
#'
#' @param connection DuckDB connection
#' @param verbose Whether to print verbose output
#'
#' @noRd
initialize_database <- function(connection, verbose) {
  logger::log_info("Initializing database tables and schema")
  
  # Check if metadata table exists
  tables <- DBI::dbListTables(connection)
  
  if (!"op_file_metadata" %in% tables) {
    logger::log_info("Creating file metadata table")
    
    create_metadata_table <- "
      CREATE TABLE op_file_metadata (
        filepath VARCHAR PRIMARY KEY,
        year VARCHAR,
        table_name VARCHAR,
        file_size_bytes BIGINT,
        file_hash VARCHAR,
        import_date TIMESTAMP,
        file_pattern VARCHAR
      )
    "
    
    DBI::dbExecute(connection, create_metadata_table)
    logger::log_success("Created file metadata table")
  }
  
  if (!"op_column_mappings" %in% tables) {
    logger::log_info("Creating column mappings table")
    
    create_mappings_table <- "
      CREATE TABLE op_column_mappings (
        year VARCHAR,
        standard_column VARCHAR,
        actual_column VARCHAR,
        PRIMARY KEY (year, standard_column)
      )
    "
    
    DBI::dbExecute(connection, create_mappings_table)
    logger::log_success("Created column mappings table")
  }
}

#' Find Open Payments files
#'
#' @param base_dir Base directory containing Open Payments files
#' @param years Years to find files for
#' @param file_pattern File pattern to match
#' @param verbose Whether to print verbose output
#'
#' @return Tibble of payment files
#' @noRd
find_payment_files <- function(base_dir, years, file_pattern, verbose) {
  logger::log_info("Finding Open Payments files in {base_dir}")
  
  # Find all files matching the pattern in the base directory and subdirectories
  all_files <- tryCatch({
    fs::dir_ls(base_dir, recurse = TRUE, regexp = file_pattern)
  }, error = function(e) {
    logger::log_error("Error finding files: {e$message}")
    return(character(0))
  })
  
  # If no files found, return empty tibble
  if (length(all_files) == 0) {
    logger::log_warn("No files found matching pattern: {file_pattern}")
    return(tibble::tibble(
      filepath = character(0),
      year = character(0),
      filesize_mb = numeric(0)
    ))
  }
  
  # Create a data frame with file information
  payment_files <- tibble::tibble(
    filepath = all_files,
    year = NA_character_,
    filesize_mb = as.numeric(fs::file_size(all_files)) / (1024 * 1024)
  )
  
  # Extract year from file paths
  # Common patterns in Open Payments files:
  # - PGYR2020_P062022 (Program Year 2020)
  # - RY2019 (Reporting Year 2019)
  # - _CY2018_ (Calendar Year 2018)
  
  # Try to extract years from filenames
  for (i in 1:nrow(payment_files)) {
    file_path <- payment_files$filepath[i]
    file_name <- basename(file_path)
    
    # Try different patterns
    year_match <- regmatches(file_name, regexpr("PGYR(20[0-9]{2})", file_name))
    if (length(year_match) == 0) {
      year_match <- regmatches(file_name, regexpr("RY(20[0-9]{2})", file_name))
    }
    if (length(year_match) == 0) {
      year_match <- regmatches(file_name, regexpr("CY(20[0-9]{2})", file_name))
    }
    if (length(year_match) == 0) {
      year_match <- regmatches(file_name, regexpr("(20[0-9]{2})", file_name))
    }
    
    if (length(year_match) > 0) {
      # Extract the year part
      year <- gsub("[^0-9]", "", year_match[1])
      payment_files$year[i] <- year
    }
  }
  
  # Filter by specified years if provided
  if (!is.null(years) && length(years) > 0) {
    payment_files <- dplyr::filter(payment_files, year %in% years)
    
    if (nrow(payment_files) > 0) {
      logger::log_info("Found {nrow(payment_files)} files for years: {paste(years, collapse = ', ')}")
    } else {
      logger::log_warn("No files found for specified years")
    }
  }
  
  return(payment_files)
}

#' Read specific NPI table
#'
#' @description
#' Reads and processes an NPI data table from a CSV file with comprehensive error handling.
#'
#' @param file_path Character string specifying the path to the CSV file.
#' @param column_types Optional list of column types for readr. If NULL, will be guessed.
#' @param verbose Logical, if TRUE (default) will log detailed information.
#'
#' @return A tibble containing the processed NPI data.
#'
#' @importFrom readr read_csv cols
#' @importFrom dplyr select mutate filter distinct
#' @importFrom assertthat assert_that
#' @importFrom logger log_info log_error log_debug log_warn
#'
#' @examples
#' # Basic usage with default parameters
#' npi_data <- read_npi_table(
#'   file_path = "data/MUP_DPR_RY23_P04_V10_DY22_NPI_csv_10.csv",
#'   verbose = TRUE
#' )
#' print(head(npi_data))
#'
#' # Specifying column types explicitly
#' npi_data <- read_npi_table(
#'   file_path = "data/MUP_DPR_RY23_P04_V10_DY22_NPI_csv_10.csv",
#'   column_types = readr::cols(
#'     NPI = readr::col_character(),
#'     Provider_Name = readr::col_character(),
#'     Score = readr::col_double()
#'   ),
#'   verbose = TRUE
#' )
#' print(nrow(npi_data))
#'
#' # Silent mode with minimal logging
#' npi_data <- read_npi_table(
#'   file_path = "data/MUP_DPR_RY23_P04_V10_DY22_NPI_csv_10.csv",
#'   verbose = FALSE
#' )
#' str(npi_data)
read_npi_table <- function(file_path, column_types = NULL, verbose = TRUE) {
  # Initialize logger
  logger::log_threshold(ifelse(verbose, logger::INFO, logger::ERROR))
  
  # Validate inputs
  assertthat::assert_that(is.character(file_path))
  assertthat::assert_that(length(file_path) == 1)
  assertthat::assert_that(is.logical(verbose))
  
  # Log function start
  logger::log_info("Starting to read NPI table from: {file_path}")
  
  # Check if file exists
  if (!file.exists(file_path)) {
    logger::log_error("File does not exist: {file_path}")
    stop("File not found: ", file_path)
  }
  
  # Attempt to read the file
  npi_table <- tryCatch({
    logger::log_debug("Attempting to read CSV file")
    
    # Try different reading methods
    try_reading_methods(file_path, column_types, verbose)
    
  }, error = function(e) {
    logger::log_error("All reading methods failed: {conditionMessage(e)}")
    stop("Failed to read NPI table: ", conditionMessage(e))
  })
  
  # Log success and return
  logger::log_info("Successfully processed NPI table with {nrow(npi_table)} rows and {ncol(npi_table)} columns")
  return(npi_table)
}

#' Try different methods to read the CSV file
#'
#' @param file_path Path to the CSV file
#' @param column_types Column types specification
#' @param verbose Whether to print verbose output
#'
#' @return Processed tibble
#' @noRd
try_reading_methods <- function(file_path, column_types, verbose) {
  # Method 1: Standard readr::read_csv
  npi_data <- tryCatch({
    if (verbose) logger::log_debug("Trying standard read_csv method")
    
    readr_table <- readr::read_csv(
      file = file_path,
      col_types = column_types,
      show_col_types = FALSE
    )
    
    validate_npi_data(readr_table, verbose)
    process_npi_data(readr_table, verbose)
  }, error = function(e) {
    if (verbose) logger::log_warn("Standard read_csv failed: {conditionMessage(e)}")
    NULL
  })
  
  if (!is.null(npi_data)) return(npi_data)
  
  # Method 2: Try with readr but different options
  npi_data <- tryCatch({
    if (verbose) logger::log_debug("Trying read_csv with different options")
    
    readr_table <- readr::read_csv(
      file = file_path,
      col_types = column_types,
      show_col_types = FALSE,
      lazy = FALSE,
      progress = FALSE,
      guess_max = 10000
    )
    
    validate_npi_data(readr_table, verbose)
    process_npi_data(readr_table, verbose)
  }, error = function(e) {
    if (verbose) logger::log_warn("Alternative read_csv failed: {conditionMessage(e)}")
    NULL
  })
  
  if (!is.null(npi_data)) return(npi_data)
  
  # Method 3: Try with base R read.csv
  npi_data <- tryCatch({
    if (verbose) logger::log_debug("Trying base R read.csv")
    
    base_table <- read.csv(
      file = file_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Convert to tibble
    base_tibble <- tibble::as_tibble(base_table)
    
    validate_npi_data(base_tibble, verbose)
    process_npi_data(base_tibble, verbose)
  }, error = function(e) {
    if (verbose) logger::log_warn("Base R read.csv failed: {conditionMessage(e)}")
    NULL
  })
  
  if (!is.null(npi_data)) return(npi_data)
  
  # Method 4: Last resort with data.table
  if (requireNamespace("data.table", quietly = TRUE)) {
    npi_data <- tryCatch({
      if (verbose) logger::log_debug("Trying data.table fread")
      
      dt_table <- data.table::fread(
        file = file_path,
        data.table = FALSE,
        showProgress = FALSE
      )
      
      # Convert to tibble
      dt_tibble <- tibble::as_tibble(dt_table)
      
      validate_npi_data(dt_tibble, verbose)
      process_npi_data(dt_tibble, verbose)
    }, error = function(e) {
      if (verbose) logger::log_error("data.table fread failed: {conditionMessage(e)}")
      NULL
    })
    
    if (!is.null(npi_data)) return(npi_data)
  }
  
  # If all methods failed, throw an error
  stop("All reading methods failed for file: ", file_path)
}

#' Validate NPI data structure
#'
#' @param npi_data NPI data to validate
#' @param verbose Whether to print verbose output
#'
#' @return Validated data
#' @noRd
validate_npi_data <- function(npi_data, verbose) {
  if (verbose) logger::log_debug("Validating NPI data structure")
  
  # Check if data is a data frame
  assertthat::assert_that(is.data.frame(npi_data))
  
  # Check if the data has any rows
  if (nrow(npi_data) == 0) {
    logger::log_warn("NPI data has 0 rows")
  } else {
    logger::log_debug("NPI data has {nrow(npi_data)} rows")
  }
  
  # Check expected columns
  expected_columns <- c("NPI")
  missing_columns <- expected_columns[!expected_columns %in% names(npi_data)]
  
  if (length(missing_columns) > 0) {
    logger::log_warn("Missing expected columns: {paste(missing_columns, collapse=', ')}")
  } else if (verbose) {
    logger::log_debug("All expected columns present")
  }
  
  return(npi_data)
}

#' Process NPI data
#'
#' @param npi_data NPI data to process
#' @param verbose Whether to print verbose output
#'
#' @return Processed data
#' @noRd
process_npi_data <- function(npi_data, verbose) {
  if (verbose) logger::log_info("Processing NPI data")
  
  # Handle any transformations for the data
  processed_table <- npi_data
  
  # Example transformations (modify as needed for your actual data)
  if ("NPI" %in% names(processed_table)) {
    if (verbose) logger::log_debug("Ensuring NPI column is character type")
    processed_table <- dplyr::mutate(processed_table, NPI = as.character(NPI))
  }
  
  # Remove any duplicate rows
  original_rows <- nrow(processed_table)
  processed_table <- dplyr::distinct(processed_table)
  if (nrow(processed_table) < original_rows && verbose) {
    logger::log_info("Removed {original_rows - nrow(processed_table)} duplicate rows")
  }
  
  # Log completion
  if (verbose) logger::log_debug("Data processing complete")
  
  return(processed_table)
}


# Load required packages
library(DBI)
library(duckdb)
library(dplyr)
library(logger)
library(assertthat)
library(purrr)
library(fs)

# Define paths
base_dir <- "/Volumes/Video Projects Muffly 1/open_payments_data"
db_path <- "/Volumes/Video Projects Muffly 1/open_payments_merged.duckdb"

# Process general payment data for recent years
# Now add 2019-2020 data without reimporting 2021-2023
merged_data_all_years <- process_open_payments_data_optimized(
  base_dir = base_dir,
  db_path = db_path,
  # years = c("2019", "2020", "2021", "2022", "2023"),
  payment_type = "general",
  output_table_name = "op_general_all_years",
  verbose = TRUE
)
