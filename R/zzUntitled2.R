#' Process and Merge Open Payments Data Using DuckDB with Optimization
#'
#' This function imports Open Payments data from CSV files into a DuckDB database,
#' with optimizations to avoid re-reading files that have already been imported.
#'
#' @param base_dir Character string. Path to the base directory containing Open Payments data.
#' @param db_path Character string. Path where the DuckDB database file should be
#'        created or accessed.
#' @param years Character vector. Years to process (e.g., c("2020", "2021", "2022")).
#'        If NULL, all available years will be processed.
#' @param payment_type Character string. Type of payment data to process: "general",
#'        "research", or "ownership". Default is "general".
#' @param file_pattern Character string. Pattern to identify payment files.
#'        Default is automatically determined based on payment_type.
#' @param selected_columns Character vector. Specific columns to include in the merged dataset.
#'        If NULL, a predefined set of key columns will be used.
#' @param output_table_name Character string. Name for the merged data table in DuckDB.
#'        Default is "open_payments_merged".
#' @param save_csv Logical. Whether to save the merged data to a CSV file.
#'        Default is FALSE.
#' @param output_csv_path Character string. Path where the CSV file should be saved.
#'        Only used if save_csv = TRUE. Default is NULL.
#' @param verbose Logical. Whether to print additional information during processing.
#'        Default is TRUE.
#' @param force_reimport Logical. Whether to force reimporting files even if they
#'        already exist in the database. Default is FALSE.
#'
#' @return A list containing the DuckDB connection and the name of the created table.
#'
#' @examples
#' # Example 1: Basic usage with default settings
#' \dontrun{
#' payments_data <- process_open_payments_data_optimized(
#'   base_dir = "/path/to/data",
#'   db_path = "/path/to/database.duckdb",
#'   years = c("2021", "2022", "2023"),
#'   payment_type = "general",
#'   verbose = TRUE,
#'   force_reimport = FALSE
#' )
#' }
#'
#' # Example 2: Process research payments with CSV export
#' \dontrun{
#' research_data <- process_open_payments_data_optimized(
#'   base_dir = "/path/to/data",
#'   db_path = "/path/to/database.duckdb",
#'   years = c("2020", "2021", "2022"),
#'   payment_type = "research",
#'   output_table_name = "research_payments",
#'   save_csv = TRUE,
#'   output_csv_path = "/path/to/export/research_payments.csv",
#'   verbose = TRUE,
#'   force_reimport = FALSE
#' )
#' }
#'
#' # Example 3: Process only specific columns across all available years
#' \dontrun{
#' custom_data <- process_open_payments_data_optimized(
#'   base_dir = "/path/to/data",
#'   db_path = "/path/to/database.duckdb",
#'   payment_type = "ownership",
#'   selected_columns = c("Physician_Profile_ID", "Physician_Last_Name",
#'                        "Total_Amount_Invested_USDollars", "Program_Year"),
#'   output_table_name = "physician_investments",
#'   verbose = TRUE,
#'   force_reimport = FALSE
#' )
#' }
#'
#' @importFrom DBI dbConnect dbExecute dbDisconnect dbListTables dbGetQuery dbSendQuery
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl filter mutate pull select collect arrange
#' @importFrom logger log_info log_success log_error log_debug log_warn
#' @importFrom assertthat assert_that
#' @importFrom purrr map_dfr map
#' @importFrom fs file_size dir_ls
#' @importFrom tibble tibble as_tibble
#' @importFrom tools md5sum
#'
#' @export
process_open_payments_data_optimized <- function(base_dir,
                                                 db_path,
                                                 years = NULL,
                                                 payment_type = "general",
                                                 file_pattern = NULL,
                                                 selected_columns = NULL,
                                                 output_table_name = "open_payments_merged",
                                                 save_csv = FALSE,
                                                 output_csv_path = NULL,
                                                 verbose = TRUE,
                                                 force_reimport = FALSE) {
  
  # Set up logging
  logger::log_layout(logger::layout_simple)
  log_level <- if (verbose) logger::DEBUG else logger::INFO
  logger::log_threshold(log_level)
  
  # Validate inputs
  validate_inputs(base_dir, db_path, years, payment_type, file_pattern, 
                  output_table_name, save_csv, output_csv_path, verbose, force_reimport)
  
  # Determine file pattern based on payment type if not provided
  if (is.null(file_pattern)) {
    type_pattern <- switch(payment_type,
                           "general" = "GNRL",
                           "research" = "RSRCH",
                           "ownership" = "OWNRSHP",
                           "GNRL")
    file_pattern <- paste0("OP_DTL_", type_pattern, "_PGYR\\d{4}.*\\.csv$")
  }
  
  # Log function parameters
  logger::log_info("Starting Open Payments data processing with parameters:")
  logger::log_debug("Base Directory: {base_dir}")
  logger::log_debug("DB Path: {db_path}")
  if (!is.null(years)) {
    logger::log_debug("Years: {paste(years, collapse = ', ')}")
  }
  logger::log_debug("Payment Type: {payment_type}")
  logger::log_debug("File Pattern: {file_pattern}")
  logger::log_debug("Output Table Name: {output_table_name}")
  logger::log_debug("Force Reimport: {force_reimport}")
  
  # Set up database connection
  db_conn <- setup_duckdb_connection(db_path, verbose)
  
  # Create required database tables if they don't exist
  initialize_database_tables(db_conn, verbose)
  
  # Find payment files
  payment_files <- find_payment_files(base_dir, file_pattern, years, verbose)
  
  # Stop if no files found
  if (nrow(payment_files) == 0) {
    logger::log_error("No payment files found. Process aborted.")
    DBI::dbDisconnect(db_conn)
    return(NULL)
  }
  
  # Check which files are already imported
  if (!force_reimport) {
    payment_files <- check_imported_files(db_conn, payment_files, verbose)
  } else {
    # Mark all files for import if force_reimport is TRUE
    logger::log_info("Force reimport is enabled. All files will be reimported.")
    payment_files$already_imported <- FALSE
    payment_files$reimport <- TRUE
    payment_files$existing_table <- NA_character_
  }
  
  # Import files to DuckDB
  imported_tables <- import_files_to_duckdb(
    connection = db_conn, 
    payment_files = payment_files, 
    verbose = verbose, 
    force_reimport = force_reimport, 
    file_pattern = file_pattern
  )
  
  # Define key columns based on payment type if no specific columns provided
  if (is.null(selected_columns)) {
    selected_columns <- get_key_columns(payment_type)
    logger::log_info("Using default columns for payment type '{payment_type}'")
    if (verbose) {
      logger::log_debug("Selected columns: {paste(selected_columns, collapse = ', ')}")
    }
  } else {
    logger::log_info("Using {length(selected_columns)} custom columns")
    if (verbose) {
      logger::log_debug("Selected columns: {paste(selected_columns, collapse = ', ')}")
    }
  }
  
  # Create mapping of column names across years
  all_years <- unique(c(names(imported_tables$new_tables), names(imported_tables$existing_tables)))
  column_mapping <- create_column_mapping(
    connection = db_conn, 
    new_tables = imported_tables$new_tables, 
    existing_tables = imported_tables$existing_tables, 
    selected_columns = selected_columns, 
    verbose = verbose
  )
  
  # Merge data from all years with consistent column names
  merged_table <- merge_payment_data(
    connection = db_conn, 
    new_tables = imported_tables$new_tables, 
    existing_tables = imported_tables$existing_tables, 
    column_mapping = column_mapping, 
    output_table_name = output_table_name, 
    verbose = verbose
  )
  
  # Export to CSV if requested
  if (save_csv && !is.null(output_csv_path)) {
    export_to_csv(db_conn, output_table_name, output_csv_path, verbose)
  }
  
  # Return the connection and table name for further use
  result <- list(
    connection = db_conn,
    table_name = output_table_name
  )
  
  logger::log_success("Open Payments data processing completed successfully")
  logger::log_info("Result contains connection object and table name '{output_table_name}'")
  
  return(result)
}

#' Validate input parameters
#'
#' @param base_dir Base directory
#' @param db_path DuckDB database path
#' @param years Years to process
#' @param payment_type Payment type
#' @param file_pattern File pattern
#' @param output_table_name Output table name
#' @param save_csv Whether to save CSV
#' @param output_csv_path Output CSV path
#' @param verbose Whether to print verbose output
#'
#' @noRd
validate_inputs <- function(base_dir, db_path, years, payment_type, file_pattern, 
                            output_table_name, save_csv, output_csv_path, verbose, force_reimport = FALSE) {
  
  logger::log_debug("Validating input parameters")
  
  # Validate base directory
  assertthat::assert_that(is.character(base_dir), 
                          msg = "base_dir must be a character string")
  assertthat::assert_that(dir.exists(base_dir), 
                          msg = paste0("Directory not found: ", base_dir))
  
  # Validate db_path
  assertthat::assert_that(is.character(db_path), 
                          msg = "db_path must be a character string")
  
  # Validate years if provided
  if (!is.null(years)) {
    assertthat::assert_that(is.character(years), 
                            msg = "years must be a character vector")
  }
  
  # Validate payment_type
  assertthat::assert_that(is.character(payment_type), 
                          msg = "payment_type must be a character string")
  assertthat::assert_that(payment_type %in% c("general", "research", "ownership"), 
                          msg = "payment_type must be one of: general, research, ownership")
  
  # Validate file_pattern if provided
  if (!is.null(file_pattern)) {
    assertthat::assert_that(is.character(file_pattern), 
                            msg = "file_pattern must be a character string")
  }
  
  # Validate output_table_name
  assertthat::assert_that(is.character(output_table_name), 
                          msg = "output_table_name must be a character string")
  
  # Validate save_csv and output_csv_path
  assertthat::assert_that(is.logical(save_csv), 
                          msg = "save_csv must be a logical value")
  
  if (save_csv) {
    assertthat::assert_that(!is.null(output_csv_path), 
                            msg = "output_csv_path must be provided when save_csv is TRUE")
    assertthat::assert_that(is.character(output_csv_path), 
                            msg = "output_csv_path must be a character string")
  }
  
  # Validate verbose
  assertthat::assert_that(is.logical(verbose),
                          msg = "verbose must be a logical value")
  
  # Validate force_reimport
  assertthat::assert_that(is.logical(force_reimport),
                          msg = "force_reimport must be a logical value")
  
  logger::log_debug("Input validation completed")
}

#' Set up DuckDB connection
#'
#' @param db_path Path to DuckDB database
#' @param verbose Whether to print verbose output
#'
#' @return DuckDB connection
#' @noRd
setup_duckdb_connection <- function(db_path, verbose) {
  logger::log_info("Connecting to DuckDB at {db_path}")
  
  tryCatch({
    # Create connection to DuckDB
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
    logger::log_success("Successfully connected to DuckDB")
    
    # Configure DuckDB for optimal performance
    if (verbose) {
      logger::log_debug("Setting DuckDB configuration parameters")
    }
    
    DBI::dbExecute(connection, "PRAGMA memory_limit='8GB'")
    DBI::dbExecute(connection, "PRAGMA threads=4")
    DBI::dbExecute(connection, "PRAGMA force_compression='ZSTD'")
    
    return(connection)
  }, error = function(e) {
    logger::log_error("Failed to connect to DuckDB: {e$message}")
    stop("Failed to connect to DuckDB database")
  })
}

#' Initialize database tables
#'
#' @param connection DuckDB connection
#' @param verbose Whether to print verbose output
#'
#' @noRd
initialize_database_tables <- function(connection, verbose) {
  logger::log_info("Initializing database tables")
  
  # Check and create metadata table for tracking imported files
  tables <- DBI::dbListTables(connection)
  
  if (!"op_file_metadata" %in% tables) {
    logger::log_info("Creating metadata table for tracking imported files")
    
    # Create metadata table - FIXED: Changed file_size_bytes to BIGINT to handle large file sizes
    metadata_query <- paste0(
      "CREATE TABLE op_file_metadata (",
      "filepath VARCHAR PRIMARY KEY, ",
      "year VARCHAR, ",
      "table_name VARCHAR, ",
      "file_size_bytes BIGINT, ", # Changed from INTEGER to BIGINT
      "file_hash VARCHAR, ",
      "import_date TIMESTAMP, ",
      "file_pattern VARCHAR",
      ")"
    )
    
    DBI::dbExecute(connection, metadata_query)
    logger::log_success("Created metadata table: op_file_metadata")
  } else {
    # Check and update existing metadata table schema if needed
    check_and_update_metadata_schema(connection, verbose)
  }
  
  # Check and create column mappings table
  if (!"op_column_mappings" %in% tables) {
    # Create column mappings table
    create_query <- paste0(
      "CREATE TABLE op_column_mappings (",
      "year VARCHAR, ",
      "standard_column VARCHAR, ",
      "actual_column VARCHAR, ",
      "PRIMARY KEY (year, standard_column)",
      ")"
    )
    
    DBI::dbExecute(connection, create_query)
    logger::log_success("Created column mappings table: op_column_mappings")
  }
  
  logger::log_debug("Database table initialization completed")
}

#' Check and update metadata schema
#'
#' @param connection DuckDB connection
#' @param verbose Whether to print verbose output
#'
#' @noRd
check_and_update_metadata_schema <- function(connection, verbose) {
  # Check if we need to update the file_size_bytes column type
  logger::log_debug("Checking metadata table schema")
  
  # Get the current schema
  schema_query <- "PRAGMA table_info('op_file_metadata')"
  schema_info <- DBI::dbGetQuery(connection, schema_query)
  
  # Look for file_size_bytes column
  size_col_info <- schema_info[schema_info$name == "file_size_bytes", ]
  
  # If column exists and is INTEGER, update it to BIGINT
  if (nrow(size_col_info) > 0 && size_col_info$type == "INTEGER") {
    logger::log_warn("Metadata table has INTEGER type for file_size_bytes, updating to BIGINT")
    
    # Create a temporary table with the new schema
    DBI::dbExecute(connection, paste0(
      "CREATE TABLE op_file_metadata_new (",
      "filepath VARCHAR PRIMARY KEY, ",
      "year VARCHAR, ",
      "table_name VARCHAR, ",
      "file_size_bytes BIGINT, ",
      "file_hash VARCHAR, ",
      "import_date TIMESTAMP, ",
      "file_pattern VARCHAR",
      ")"
    ))
    
    # Copy data from old table to new table
    DBI::dbExecute(connection, paste0(
      "INSERT INTO op_file_metadata_new ",
      "SELECT filepath, year, table_name, CAST(file_size_bytes AS BIGINT), ",
      "file_hash, import_date, file_pattern ",
      "FROM op_file_metadata"
    ))
    
    # Drop old table and rename new table
    DBI::dbExecute(connection, "DROP TABLE op_file_metadata")
    DBI::dbExecute(connection, "ALTER TABLE op_file_metadata_new RENAME TO op_file_metadata")
    
    logger::log_success("Successfully updated metadata table schema")
  } else {
    logger::log_debug("Metadata table schema is already up to date")
  }
}

#' Find payment files in the base directory
#'
#' @param base_dir Base directory
#' @param file_pattern File pattern to match
#' @param years Years to filter for
#' @param verbose Whether to print verbose output
#'
#' @return Tibble of payment files
#' @noRd
find_payment_files <- function(base_dir, file_pattern, years, verbose) {
  logger::log_info("Searching for Open Payments files matching pattern: {file_pattern}")
  
  # Search recursively for files matching the pattern
  all_files <- fs::dir_ls(path = base_dir, 
                          recurse = TRUE, 
                          regexp = file_pattern, 
                          type = "file")
  
  if (length(all_files) == 0) {
    logger::log_warn("No files found matching pattern: {file_pattern}")
    return(tibble::tibble(
      filepath = character(0),
      year = character(0),
      filesize_mb = numeric(0)
    ))
  }
  
  # Create a data frame with file information
  file_sizes <- as.numeric(fs::file_size(all_files))
  payment_files <- tibble::tibble(
    filepath = as.character(all_files),
    year = NA_character_,
    filesize_mb = file_sizes / (1024 * 1024)
  )
  
  # Extract year from filename using regex
  years_extracted <- character(nrow(payment_files))
  for (i in seq_len(nrow(payment_files))) {
    filename <- basename(payment_files$filepath[i])
    year_match <- regexpr("PGYR(\\d{4})", filename)
    if (year_match > 0) {
      years_extracted[i] <- substr(filename, year_match + 4, year_match + 7)
    }
  }
  payment_files$year <- years_extracted
  
  # Filter for specified years if provided
  if (!is.null(years) && length(years) > 0) {
    payment_files <- dplyr::filter(payment_files, year %in% years)
    logger::log_debug("Filtered files for requested years: {paste(years, collapse = ', ')}")
  }
  
  # Log found files
  if (nrow(payment_files) > 0) {
    total_size_mb <- sum(payment_files$filesize_mb)
    logger::log_info("Found {nrow(payment_files)} files totaling {round(total_size_mb, 2)} MB")
    
    # Sort by year for nicer display
    payment_files <- dplyr::arrange(payment_files, year)
    
    if (verbose) {
      print(payment_files)
    }
  } else {
    logger::log_warn("No files found for specified years")
  }
  
  return(payment_files)
}

#' Check which files are already imported
#'
#' @param connection DuckDB connection
#' @param payment_files Tibble of payment files to check
#' @param verbose Whether to print verbose output
#'
#' @return Updated payment_files tibble with import status
#' @noRd
check_imported_files <- function(connection, payment_files, verbose) {
  logger::log_info("Checking which files are already imported")
  
  # Add columns to track import status if they don't exist
  payment_files$already_imported <- FALSE
  payment_files$reimport <- FALSE
  payment_files$existing_table <- NA_character_
  
  # If no files to check, return early
  if (nrow(payment_files) == 0) {
    return(payment_files)
  }
  
  # Create a table with the files to check
  files_to_check <- paste0("'", payment_files$filepath, "'", collapse = ",")
  
  # Query metadata table for existing files
  query <- paste0("SELECT filepath, year, table_name, file_size_bytes, file_hash FROM op_file_metadata ",
                  "WHERE filepath IN (", files_to_check, ")")
  
  tryCatch({
    imported_files <- DBI::dbGetQuery(connection, query)
    
    if (nrow(imported_files) > 0) {
      logger::log_info("Found {nrow(imported_files)} previously imported files")
      
      # Check if file sizes match (to detect if files have changed)
      for (i in 1:nrow(imported_files)) {
        file_path <- imported_files$filepath[i]
        db_size <- imported_files$file_size_bytes[i]
        db_hash <- imported_files$file_hash[i]
        
        # Find the file in our payment_files list
        file_idx <- which(payment_files$filepath == file_path)
        
        if (length(file_idx) > 0) {
          # Get current file size
          current_size <- as.numeric(fs::file_size(file_path))
          
          # If sizes don't match, mark for reimport
          if (current_size != db_size) {
            logger::log_warn("File size changed for {basename(file_path)}: was {db_size} bytes, now {current_size} bytes")
            payment_files$reimport[file_idx] <- TRUE
          } else {
            # Optionally verify hash if sizes match (slower but more reliable)
            if (verbose) {
              current_hash <- as.character(tools::md5sum(file_path))
              if (current_hash != db_hash) {
                logger::log_warn("File hash changed for {basename(file_path)}")
                payment_files$reimport[file_idx] <- TRUE
              } else {
                logger::log_debug("File {basename(file_path)} verified by hash")
              }
            }
            
            # Mark as already imported if not reimporting
            if (!payment_files$reimport[file_idx]) {
              payment_files$already_imported[file_idx] <- TRUE
              payment_files$existing_table[file_idx] <- imported_files$table_name[i]
            }
          }
        }
      }
      
      # Count files that need to be imported vs reused
      files_to_import <- sum(!payment_files$already_imported | payment_files$reimport)
      files_to_reuse <- sum(payment_files$already_imported & !payment_files$reimport)
      
      logger::log_info("{files_to_import} files need to be imported, {files_to_reuse} can be reused")
      
      return(payment_files)
    } else {
      # No previously imported files found
      logger::log_info("No previously imported files found")
      return(payment_files)
    }
  }, error = function(e) {
    logger::log_warn("Error checking imported files: {e$message}")
    return(payment_files)
  })
}

#' Import Open Payments files to DuckDB
#'
#' @param connection DuckDB connection
#' @param payment_files Tibble of payment files to import
#' @param verbose Whether to print verbose output
#' @param force_reimport Whether to force reimporting files
#' @param file_pattern The file pattern used for matching files
#'
#' @return List containing new_tables and existing_tables
#' @noRd
import_files_to_duckdb <- function(connection, payment_files, verbose, force_reimport, file_pattern) {
  logger::log_info("Importing files to DuckDB")
  
  new_tables <- list()
  existing_tables <- list()
  
  # Identify files that need to be imported
  files_to_import <- payment_files[!payment_files$already_imported | payment_files$reimport | force_reimport, ]
  files_already_imported <- payment_files[payment_files$already_imported & !payment_files$reimport & !force_reimport, ]
  
  if (nrow(files_to_import) > 0) {
    logger::log_info("Importing {nrow(files_to_import)} files to DuckDB")
    
    for (i in 1:nrow(files_to_import)) {
      file_info <- files_to_import[i, ]
      filepath <- file_info$filepath
      year <- file_info$year
      filesize_mb <- file_info$filesize_mb
      
      # Create a standardized table name for each file
      table_name <- paste0("op_temp_", year)
      
      logger::log_info("Importing file {i} of {nrow(files_to_import)}: {basename(filepath)} ({round(filesize_mb, 2)} MB)")
      
      # Attempt import with retry logic
      import_success <- import_file_with_retry(connection, filepath, table_name, verbose)
      
      # If import was successful, update metadata table and add to new_tables list
      if (import_success) {
        # Add to the new_tables list
        new_tables[[year]] <- table_name
        
        # Update metadata
        update_file_metadata(connection, filepath, year, table_name, file_pattern, verbose)
      } else {
        logger::log_error("Failed to import data for year {year}")
      }
    }
  } else {
    logger::log_info("No new files to import")
  }
  
  # Add existing tables to the list
  if (nrow(files_already_imported) > 0) {
    logger::log_info("Using {nrow(files_already_imported)} previously imported tables")
    
    for (i in 1:nrow(files_already_imported)) {
      year <- files_already_imported$year[i]
      table_name <- files_already_imported$existing_table[i]
      
      # Validate that the table actually exists
      tables <- DBI::dbListTables(connection)
      if (table_name %in% tables) {
        existing_tables[[year]] <- table_name
        logger::log_info("Using existing table for year {year}: {table_name}")
      } else {
        logger::log_warn("Existing table {table_name} for year {year} not found, will need to reimport")
        # Mark this file for reimport next time
        payment_files$already_imported[payment_files$year == year] <- FALSE
      }
    }
  }
  
  logger::log_info("Imported {length(new_tables)} new tables, using {length(existing_tables)} existing tables")
  return(list(
    new_tables = new_tables,
    existing_tables = existing_tables
  ))
}

#' Import file with retry mechanisms
#'
#' @param connection DuckDB connection
#' @param filepath File path to import
#' @param table_name Table name to create
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating if import was successful
#' @noRd
import_file_with_retry <- function(connection, filepath, table_name, verbose) {
  # First try with auto detection (fastest)
  import_query <- paste0(
    "CREATE TABLE ", table_name, " AS ",
    "SELECT * FROM read_csv_auto('", filepath, "', ",
    "all_varchar=TRUE, strict_mode=FALSE, ignore_errors=TRUE, header=TRUE)"
  )
  
  import_success <- FALSE
  logger::log_debug("Attempting import with automatic type detection")
  
  tryCatch({
    DBI::dbExecute(connection, import_query)
    new_tables[[year]] <- table_name
    import_success <- TRUE
    logger::log_success("Successfully imported data to table: {table_name}")
  }, error = function(e) {
    logger::log_warn("Failed to import with auto method: {e$message}")
    
    # Try a more robust import approach with explicit settings
    logger::log_info("Trying alternative import method...")
    alternative_query <- paste0(
      "CREATE TABLE ", table_name, " AS ",
      "SELECT * FROM read_csv('", filepath, "', ",
      "header=TRUE, delim=',', quote='\"', escape='\"', auto_detect=TRUE, nullstr='NA', ignore_errors=TRUE)"
    )
    
    tryCatch({
      DBI::dbExecute(connection, alternative_query)
      import_success <- TRUE
      logger::log_success("Successfully imported with alternative method to table: {table_name}")
    }, error = function(e2) {
      logger::log_error("Alternative import method also failed: {e2$message}")
      
      # Last resort method - try with smallest batch size and maximum error skipping
      logger::log_info("Trying last resort import method...")
      last_resort_query <- paste0(
        "CREATE TABLE ", table_name, " AS ",
        "SELECT * FROM read_csv('", filepath, "', ",
        "header=TRUE, delim=',', quote='\"', escape='\"', ",
        "all_varchar=TRUE, ignore_errors=TRUE, sample_size=1000, ",
        "batch_size=50000, maximum_line_size=0)"
      )
      
      tryCatch({
        DBI::dbExecute(connection, last_resort_query)
        import_success <- TRUE
        logger::log_success("Successfully imported with last resort method to table: {table_name}")
      }, error = function(e3) {
        logger::log_error("All import methods failed for {basename(filepath)}: {e3$message}")
      })
    })
  })
  
  # Verify import and log row count if successful
  if (import_success) {
    tryCatch({
      count_query <- paste0("SELECT COUNT(*) AS row_count FROM ", table_name)
      count_result <- DBI::dbGetQuery(connection, count_query)
      row_count <- count_result$row_count[1]
      logger::log_info("Imported {row_count} rows to table {table_name}")
    }, error = function(e) {
      logger::log_warn("Could not get row count for table {table_name}: {e$message}")
    })
  }
  
  return(import_success)
}

#' Update file metadata in database
#'
#' @param connection DuckDB connection
#' @param filepath File path
#' @param year Year
#' @param table_name Table name
#' @param file_pattern File pattern
#' @param verbose Whether to print verbose output
#'
#' @noRd
update_file_metadata <- function(connection, filepath, year, table_name, file_pattern, verbose) {
  logger::log_debug("Updating metadata for file: {basename(filepath)}")
  
  # Get file hash
  file_hash <- as.character(tools::md5sum(filepath))
  
  # Get file size as BIGINT
  file_size <- as.numeric(fs::file_size(filepath))
  
  # Update metadata table
  upsert_query <- paste0(
    "INSERT OR REPLACE INTO op_file_metadata ",
    "(filepath, year, table_name, file_size_bytes, file_hash, import_date, file_pattern) ",
    "VALUES ('", filepath, "', '", year, "', '", table_name, "', ", 
    file_size, ", '", file_hash, "', CURRENT_TIMESTAMP, '", 
    file_pattern, "')"
  )
  
  tryCatch({
    DBI::dbExecute(connection, upsert_query)
    logger::log_debug("Updated metadata for file: {basename(filepath)}")
  }, error = function(e) {
    logger::log_error("Failed to update metadata for file {basename(filepath)}: {e$message}")
    
    # Try to diagnose the issue
    if (grepl("Type .* with value .* can't be cast", e$message)) {
      logger::log_error("This appears to be a type conversion error. Checking metadata table schema...")
      
      # Print the current schema to diagnose
      schema_query <- "PRAGMA table_info('op_file_metadata')"
      schema_info <- DBI::dbGetQuery(connection, schema_query)
      logger::log_debug("Current metadata table schema:")
      print(schema_info)
      
      # Try to insert with explicit type casting
      logger::log_info("Trying with explicit BIGINT casting...")
      alternative_upsert <- paste0(
        "INSERT OR REPLACE INTO op_file_metadata ",
        "(filepath, year, table_name, file_size_bytes, file_hash, import_date, file_pattern) ",
        "VALUES ('", filepath, "', '", year, "', '", table_name, "', ", 
        "CAST(", file_size, " AS BIGINT), '", file_hash, "', CURRENT_TIMESTAMP, '", 
        file_pattern, "')"
      )
      
      tryCatch({
        DBI::dbExecute(connection, alternative_upsert)
        logger::log_success("Successfully updated metadata with BIGINT casting")
      }, error = function(e2) {
        logger::log_error("Alternative metadata update also failed: {e2$message}")
      })
    }
  })
}

#' Create column mapping across years
#'
#' @param connection DuckDB connection
#' @param new_tables List of newly imported tables
#' @param existing_tables List of existing tables
#' @param selected_columns Columns to include in mapping
#' @param verbose Whether to print verbose output
#'
#' @return List of column mappings by year
#' @noRd
create_column_mapping <- function(connection, new_tables, existing_tables, 
                                  selected_columns, verbose) {
  logger::log_info("Creating column mapping across years")
  
  column_mapping <- list()
  
  # Extract years from tables
  all_years <- unique(c(names(new_tables), names(existing_tables)))
  
  # Common column name variations and potential mappings
  column_variations <- list(
    "Physician_Profile_ID" = c("Physician_Profile_ID", "Covered_Recipient_Profile_ID", "Physician_ID"),
    "Physician_First_Name" = c("Physician_First_Name", "Covered_Recipient_First_Name", "First_Name"),
    "Physician_Last_Name" = c("Physician_Last_Name", "Covered_Recipient_Last_Name", "Last_Name"),
    "Physician_Middle_Name" = c("Physician_Middle_Name", "Covered_Recipient_Middle_Name", "Middle_Name"),
    "Teaching_Hospital_ID" = c("Teaching_Hospital_ID", "Hospital_ID"),
    "Recipient_State" = c("Recipient_State", "State", "Physician_State", "Covered_Recipient_State"),
    "Total_Amount_of_Payment_USDollars" = c("Total_Amount_of_Payment_USDollars", 
                                            "Total_Amount_of_Payment_US_Dollars", 
                                            "Payment_Amount", 
                                            "Total_Payment_USD"),
    "Date_of_Payment" = c("Date_of_Payment", "Payment_Date"),
    "Nature_of_Payment_or_Transfer_of_Value" = c("Nature_of_Payment_or_Transfer_of_Value", 
                                                 "Payment_Nature", 
                                                 "Payment_Type"),
    "Program_Year" = c("Program_Year", "Year", "Reporting_Year"),
    "Total_Amount_Invested_USDollars" = c("Total_Amount_Invested_USDollars", 
                                          "Total_Amount_Invested", 
                                          "Investment_Amount_USD"),
    "Value_of_Interest" = c("Value_of_Interest", "Interest_Value")
  )
  
  # First check if we have cached mappings in the database
  existing_mappings <- get_cached_column_mappings(connection, all_years)
  
  # For each year, find the available columns and map to standardized names
  for (year in all_years) {
    # Check if we have a cached mapping for this year
    if (year %in% names(existing_mappings)) {
      column_mapping[[year]] <- existing_mappings[[year]]
      logger::log_info("Using cached column mapping for year {year}")
      
      if (verbose) {
        logger::log_debug("Year {year} cached column mapping:")
        print(column_mapping[[year]])
      }
      
      next
    }
    
    # Determine table name for this year
    table_name <- NULL
    if (year %in% names(new_tables)) {
      table_name <- new_tables[[year]]
    } else if (year %in% names(existing_tables)) {
      table_name <- existing_tables[[year]]
    }
    
    if (is.null(table_name)) {
      logger::log_warn("No table found for year {year}, skipping mapping")
      next
    }
    
    # Get columns in this table
    query <- paste0("SELECT * FROM ", table_name, " LIMIT 1")
    sample_data <- DBI::dbGetQuery(connection, query)
    available_columns <- colnames(sample_data)
    
    # Map columns for this year
    year_mapping <- list()
    
    for (std_col in selected_columns) {
      # Check if standard column exists in this table
      if (std_col %in% available_columns) {
        year_mapping[[std_col]] <- std_col
        next
      }
      
      # Try variations if defined
      if (std_col %in% names(column_variations)) {
        variations <- column_variations[[std_col]]
        for (var in variations) {
          if (var %in% available_columns) {
            year_mapping[[std_col]] <- var
            break
          }
        }
      }
      
      # If still not found, try case-insensitive matching
      if (is.null(year_mapping[[std_col]])) {
        lower_std_col <- tolower(std_col)
        lower_available <- tolower(available_columns)
        match_idx <- which(lower_available == lower_std_col)
        if (length(match_idx) > 0) {
          year_mapping[[std_col]] <- available_columns[match_idx[1]]
        } else {
          # Try fuzzy matching for remaining columns
          for (avail_col in available_columns) {
            if (grepl(gsub("_", ".*", std_col), avail_col, ignore.case = TRUE)) {
              year_mapping[[std_col]] <- avail_col
              break
            }
          }
        }
      }
      
      # If still not found, log it
      if (is.null(year_mapping[[std_col]])) {
        logger::log_warn("Column '{std_col}' not found in {year} data")
      }
    }
    
    column_mapping[[year]] <- year_mapping
    
    # Cache this mapping for future use
    cache_column_mapping(connection, year, year_mapping)
    
    if (verbose) {
      logger::log_debug("Year {year} column mapping:")
      print(year_mapping)
    }
  }
  
  return(column_mapping)
}

#' Get cached column mappings from database
#'
#' @param connection DuckDB connection
#' @param years Years to get mappings for
#'
#' @return List of cached column mappings by year
#' @noRd
get_cached_column_mappings <- function(connection, years) {
  # Query for existing mappings
  years_str <- paste0("'", years, "'", collapse = ",")
  
  # Only proceed if we have years to query
  if (length(years) == 0) {
    return(list())
  }
  
  query <- paste0("SELECT year, standard_column, actual_column FROM op_column_mappings ",
                  "WHERE year IN (", years_str, ")")
  
  mappings <- tryCatch({
    DBI::dbGetQuery(connection, query)
  }, error = function(e) {
    logger::log_warn("Error querying cached column mappings: {e$message}")
    return(data.frame())
  })
  
  if (nrow(mappings) == 0) {
    return(list())
  }
  
  # Convert to nested list format
  result <- list()
  
  for (year in unique(mappings$year)) {
    year_mappings <- mappings[mappings$year == year, ]
    result[[year]] <- setNames(
      as.list(year_mappings$actual_column),
      year_mappings$standard_column
    )
  }
  
  return(result)
}

#' Cache column mapping for future use
#'
#' @param connection DuckDB connection
#' @param year Year
#' @param mapping Column mapping for the year
#'
#' @noRd
cache_column_mapping <- function(connection, year, mapping) {
  # Delete existing mappings for this year
  delete_query <- paste0("DELETE FROM op_column_mappings WHERE year = '", year, "'")
  DBI::dbExecute(connection, delete_query)
  
  # Insert new mappings
  for (std_col in names(mapping)) {
    actual_col <- mapping[[std_col]]
    
    if (!is.null(actual_col)) {
      insert_query <- paste0(
        "INSERT INTO op_column_mappings (year, standard_column, actual_column) ",
        "VALUES ('", year, "', '", std_col, "', '", actual_col, "')"
      )
      
      DBI::dbExecute(connection, insert_query)
    }
  }
  
  logger::log_debug("Cached column mapping for year {year}")
}

#' Merge payment data from all years
#'
#' @param connection DuckDB connection
#' @param new_tables List of newly imported tables
#' @param existing_tables List of existing tables 
#' @param column_mapping Column mapping across years
#' @param output_table_name Name for merged output table
#' @param verbose Whether to print verbose output
#'
#' @return Name of the created merge table
#' @noRd
merge_payment_data <- function(connection, new_tables, existing_tables, 
                               column_mapping, output_table_name, verbose) {
  logger::log_info("Merging data from all years into table: {output_table_name}")
  
  # Check if output table already exists
  tables <- DBI::dbListTables(connection)
  if (output_table_name %in% tables) {
    logger::log_warn("Output table {output_table_name} already exists, dropping it")
    DBI::dbExecute(connection, paste0("DROP TABLE IF EXISTS ", output_table_name))
  }
  
  # Create the merged table
  all_years <- unique(c(names(new_tables), names(existing_tables)))
  
  # If no tables to merge, create an empty output table
  if (length(all_years) == 0) {
    logger::log_warn("No tables available to merge, creating empty output table")
    
    # Create a minimal table structure
    create_empty_table <- paste0(
      "CREATE TABLE ", output_table_name, " (",
      "Physician_Profile_ID VARCHAR, ",
      "Physician_First_Name VARCHAR, ",
      "Physician_Last_Name VARCHAR, ",
      "Total_Amount_of_Payment_USDollars VARCHAR, ",
      "Program_Year VARCHAR, ",
      "Source_Table VARCHAR",
      ")"
    )
    
    DBI::dbExecute(connection, create_empty_table)
    logger::log_info("Created empty output table: {output_table_name}")
    return(output_table_name)
  }
  
  # Strategy 1: Try to use UNION ALL approach first (most efficient)
  merge_success <- merge_with_union_all(connection, new_tables, existing_tables, 
                                        column_mapping, output_table_name, verbose)
  
  # If that fails, try the incremental approach
  if (!merge_success) {
    logger::log_info("Trying alternative merge strategy...")
    merge_success <- merge_with_incremental_inserts(connection, new_tables, existing_tables,
                                                    column_mapping, output_table_name, verbose)
  }
  
  # If still not successful, create an empty table with standard columns
  if (!merge_success) {
    logger::log_error("All merge strategies failed, creating empty output table")
    
    # Create a minimal table structure
    create_empty_table <- paste0(
      "CREATE TABLE ", output_table_name, " (",
      "Physician_Profile_ID VARCHAR, ",
      "Physician_First_Name VARCHAR, ",
      "Physician_Last_Name VARCHAR, ",
      "Total_Amount_of_Payment_USDollars VARCHAR, ",
      "Program_Year VARCHAR, ",
      "Source_Table VARCHAR",
      ")"
    )
    
    DBI::dbExecute(connection, create_empty_table)
    logger::log_info("Created empty output table: {output_table_name}")
  } else {
    # Get row count of merged table
    count_query <- paste0("SELECT COUNT(*) AS row_count FROM ", output_table_name)
    count_result <- DBI::dbGetQuery(connection, count_query)
    row_count <- count_result$row_count[1]
    
    logger::log_success("Successfully merged data into table: {output_table_name}")
    logger::log_info("Merged table contains {row_count} rows")
  }
  
  return(output_table_name)
}

#' Merge with UNION ALL approach
#'
#' @param connection DuckDB connection
#' @param new_tables List of newly imported tables
#' @param existing_tables List of existing tables
#' @param column_mapping Column mapping across years
#' @param output_table_name Name for merged output table
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating if merge was successful
#' @noRd
merge_with_union_all <- function(connection, new_tables, existing_tables,
                                 column_mapping, output_table_name, verbose) {
  logger::log_debug("Attempting merge with UNION ALL approach")
  
  # Generate and execute SQL for each year and union the results
  sql_parts <- c()
  
  all_years <- unique(c(names(new_tables), names(existing_tables)))
  
  # Get all standard columns across all mappings
  all_std_cols <- unique(unlist(lapply(column_mapping, names)))
  
  # Only proceed if we have columns to work with
  if (length(all_std_cols) == 0) {
    logger::log_error("No column mappings available for any year")
    return(FALSE)
  }
  
  for (year in all_years) {
    # Determine the table name for this year
    table_name <- NULL
    if (year %in% names(new_tables)) {
      table_name <- new_tables[[year]]
    } else if (year %in% names(existing_tables)) {
      table_name <- existing_tables[[year]]
    }
    
    if (is.null(table_name) || is.null(column_mapping[[year]])) {
      logger::log_warn("Missing table or mapping for year {year}, skipping")
      next
    }
    
    year_mapping <- column_mapping[[year]]
    
    # Build SELECT clause with column mappings
    select_parts <- c()
    for (std_col in all_std_cols) {
      org_col <- NULL
      if (std_col %in% names(year_mapping)) {
        org_col <- year_mapping[[std_col]]
      }
      
      if (!is.null(org_col)) {
        select_parts <- c(select_parts, 
                          paste0('"', org_col, '" AS "', std_col, '"'))
      } else {
        # Add NULL for missing columns
        select_parts <- c(select_parts, 
                          paste0('NULL AS "', std_col, '"'))
      }
    }
    
    # Add literal year if Program_Year is missing
    if (!"Program_Year" %in% names(year_mapping) || is.null(year_mapping[["Program_Year"]])) {
      select_parts <- c(select_parts, paste0("'", year, "' AS \"Program_Year\""))
    }
    
    # Add literal source table
    select_parts <- c(select_parts, paste0("'", table_name, "' AS \"Source_Table\""))
    
    # Build the SELECT query for this year
    year_sql <- paste0(
      "SELECT ", paste(select_parts, collapse = ", "),
      " FROM ", table_name
    )
    
    sql_parts <- c(sql_parts, year_sql)
  }
  
  if (length(sql_parts) == 0) {
    logger::log_error("No tables to merge")
    return(FALSE)
  }
  
  # Combine all year queries with UNION ALL
  full_sql <- paste0(
    "CREATE TABLE ", output_table_name, " AS ",
    paste(sql_parts, collapse = " UNION ALL ")
  )
  
  # Execute the merge query
  tryCatch({
    if (verbose) {
      logger::log_debug("Executing UNION ALL merge SQL")
    }
    
    DBI::dbExecute(connection, full_sql)
    return(TRUE)
  }, error = function(e) {
    logger::log_warn("UNION ALL merge failed: {e$message}")
    return(FALSE)
  })
}

#' Merge with incremental inserts approach
#'
#' @param connection DuckDB connection
#' @param new_tables List of newly imported tables
#' @param existing_tables List of existing tables
#' @param column_mapping Column mapping across years
#' @param output_table_name Name for merged output table
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating if merge was successful
#' @noRd
merge_with_incremental_inserts <- function(connection, new_tables, existing_tables,
                                           column_mapping, output_table_name, verbose) {
  logger::log_debug("Attempting merge with incremental inserts approach")
  
  # Get all unique standard columns from all mappings
  all_std_cols <- unique(unlist(lapply(column_mapping, names)))
  
  if (length(all_std_cols) == 0) {
    logger::log_error("No column mappings available for any year")
    return(FALSE)
  }
  
  # Add the Source_Table column
  all_std_cols <- c(all_std_cols, "Source_Table")
  
  # Create an empty table with all required columns
  column_defs <- paste0('"', all_std_cols, '" VARCHAR', collapse = ", ")
  create_empty_table <- paste0(
    "CREATE TABLE ", output_table_name, " (", column_defs, ")"
  )
  
  tryCatch({
    DBI::dbExecute(connection, create_empty_table)
    logger::log_success("Created empty target table with all columns")
    
    # Insert data from each year
    all_years <- unique(c(names(new_tables), names(existing_tables)))
    successful_inserts <- 0
    
    for (year in all_years) {
      # Determine the table name for this year
      table_name <- NULL
      if (year %in% names(new_tables)) {
        table_name <- new_tables[[year]]
      } else if (year %in% names(existing_tables)) {
        table_name <- existing_tables[[year]]
      }
      
      if (is.null(table_name) || is.null(column_mapping[[year]])) {
        logger::log_warn("Missing table or mapping for year {year}, skipping")
        next
      }
      
      year_mapping <- column_mapping[[year]]
      
      # Build INSERT query
      insert_sql <- build_insert_query(output_table_name, table_name, year_mapping, 
                                       all_std_cols, year, verbose)
      
      tryCatch({
        DBI::dbExecute(connection, insert_sql)
        logger::log_info("Inserted data from year {year}")
        successful_inserts <- successful_inserts + 1
      }, error = function(e) {
        logger::log_error("Failed to insert data from year {year}: {e$message}")
        
        # Try with chunks if the full insert fails
        logger::log_info("Trying chunked insert for year {year}")
        chunked_success <- insert_in_chunks(connection, output_table_name, table_name, 
                                            year_mapping, all_std_cols, year, verbose)
        
        if (chunked_success) {
          successful_inserts <- successful_inserts + 1
        }
      })
    }
    
    if (successful_inserts > 0) {
      logger::log_success("Successfully merged data from {successful_inserts} year(s)")
      return(TRUE)
    } else {
      logger::log_error("No data was successfully merged")
      return(FALSE)
    }
    
  }, error = function(e) {
    logger::log_error("Failed to create target table: {e$message}")
    return(FALSE)
  })
}

#' Build INSERT query for merging
#'
#' @param output_table Output table name
#' @param source_table Source table name
#' @param mapping Column mapping
#' @param all_columns All columns in the output table
#' @param year Year value
#' @param verbose Whether to print verbose output
#'
#' @return INSERT SQL query
#' @noRd
build_insert_query <- function(output_table, source_table, mapping, all_columns, year, verbose) {
  # Quote all column names
  output_cols_str <- paste0('"', all_columns, '"', collapse = ", ")
  
  # Build SELECT part for source table
  select_parts <- c()
  for (std_col in all_columns) {
    if (std_col == "Source_Table") {
      # Add source table name
      select_parts <- c(select_parts, paste0("'", source_table, "'"))
    } else {
      org_col <- NULL
      if (std_col %in% names(mapping)) {
        org_col <- mapping[[std_col]]
      }
      
      if (!is.null(org_col)) {
        select_parts <- c(select_parts, paste0('"', org_col, '"'))
      } else if (std_col == "Program_Year" && (!"Program_Year" %in% names(mapping) || is.null(mapping[["Program_Year"]]))) {
        # Add year as Program_Year if missing
        select_parts <- c(select_parts, paste0("'", year, "'"))
      } else {
        # Add NULL for missing columns
        select_parts <- c(select_parts, "NULL")
      }
    }
  }
  
  # Build the INSERT query
  insert_sql <- paste0(
    "INSERT INTO ", output_table, " (", output_cols_str, ") ",
    "SELECT ", paste(select_parts, collapse = ", "),
    " FROM ", source_table
  )
  
  if (verbose) {
    logger::log_debug("Built INSERT query for year {year}")
  }
  
  return(insert_sql)
}

#' Insert data in chunks to handle large tables
#'
#' @param connection DuckDB connection
#' @param output_table Output table name
#' @param source_table Source table name
#' @param mapping Column mapping
#' @param all_columns All columns in the output table
#' @param year Year value
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating if chunked insert was successful
#' @noRd
insert_in_chunks <- function(connection, output_table, source_table, mapping, 
                             all_columns, year, verbose) {
  # Get row count of source table
  count_query <- paste0("SELECT COUNT(*) AS row_count FROM ", source_table)
  count_result <- DBI::dbGetQuery(connection, count_query)
  total_rows <- count_result$row_count[1]
  
  # Define chunk size
  chunk_size <- 500000  # Adjust based on memory constraints
  
  if (total_rows <= 0) {
    logger::log_warn("Source table {source_table} is empty, skipping")
    return(FALSE)
  }
  
  # Calculate number of chunks
  num_chunks <- ceiling(total_rows / chunk_size)
  logger::log_info("Inserting {total_rows} rows in {num_chunks} chunks")
  
  # Process each chunk
  successful_chunks <- 0
  
  for (chunk in 1:num_chunks) {
    offset <- (chunk - 1) * chunk_size
    
    # Build chunked query
    chunked_select <- build_insert_query(output_table, source_table, mapping, all_columns, year, FALSE)
    
    # Add LIMIT and OFFSET
    chunked_select <- paste0(
      chunked_select, " LIMIT ", chunk_size, " OFFSET ", offset
    )
    
    # Execute the chunked insert
    tryCatch({
      DBI::dbExecute(connection, chunked_select)
      successful_chunks <- successful_chunks + 1
      logger::log_debug("Inserted chunk {chunk}/{num_chunks} for year {year}")
    }, error = function(e) {
      logger::log_error("Failed to insert chunk {chunk}/{num_chunks} for year {year}: {e$message}")
    })
  }
  
  if (successful_chunks > 0) {
    success_pct <- round(successful_chunks / num_chunks * 100, 1)
    logger::log_info("Inserted {successful_chunks}/{num_chunks} chunks ({success_pct}%) for year {year}")
    return(TRUE)
  } else {
    logger::log_error("Failed to insert any chunks for year {year}")
    return(FALSE)
  }
}

#' Get key columns based on payment type
#'
#' @param payment_type Payment type: general, research, or ownership
#'
#' @return Character vector of key columns
#' @noRd
get_key_columns <- function(payment_type) {
  # Define key columns based on payment type
  if (payment_type == "general") {
    key_columns <- c(
      "Physician_Profile_ID",
      "Physician_First_Name",
      "Physician_Middle_Name",
      "Physician_Last_Name",
      "Recipient_Primary_Business_Street_Address_Line1",
      "Recipient_City",
      "Recipient_State",
      "Recipient_Zip_Code",
      "Recipient_Country",
      "Recipient_Type",
      "Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name",
      "Total_Amount_of_Payment_USDollars",
      "Date_of_Payment",
      "Form_of_Payment_or_Transfer_of_Value",
      "Nature_of_Payment_or_Transfer_of_Value",
      "Program_Year"
    )
  } else if (payment_type == "research") {
    key_columns <- c(
      "Physician_Profile_ID",
      "Physician_First_Name",
      "Physician_Middle_Name",
      "Physician_Last_Name",
      "Recipient_City",
      "Recipient_State",
      "Recipient_Zip_Code",
      "Recipient_Country",
      "Recipient_Type",
      "Research_Institution_Name",
      "Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name",
      "Total_Amount_of_Payment_USDollars",
      "Date_of_Payment",
      "Program_Year"
    )
  } else if (payment_type == "ownership") {
    key_columns <- c(
      "Physician_Profile_ID",
      "Physician_First_Name",
      "Physician_Middle_Name",
      "Physician_Last_Name", 
      "Recipient_Primary_Business_Street_Address_Line1",
      "Recipient_City",
      "Recipient_State",
      "Recipient_Zip_Code",
      "Recipient_Country",
      "Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name",
      "Total_Amount_Invested_USDollars",
      "Value_of_Interest",
      "Terms_of_Interest",
      "Program_Year"
    )
  } else {
    # Default columns if payment type is not recognized
    key_columns <- c(
      "Physician_Profile_ID",
      "Physician_First_Name",
      "Physician_Last_Name",
      "Recipient_State",
      "Total_Amount_of_Payment_USDollars",
      "Date_of_Payment",
      "Program_Year"
    )
  }
  
  return(key_columns)
}

#' Export merged data to CSV
#'
#' @param connection DuckDB connection
#' @param table_name Table to export
#' @param output_path Path to save CSV file
#' @param verbose Whether to print verbose output
#'
#' @noRd
export_to_csv <- function(connection, table_name, output_path, verbose) {
  logger::log_info("Exporting merged data to CSV: {output_path}")
  
  # Ensure directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    logger::log_info("Created output directory: {output_dir}")
  }
  
  # Export to CSV using optimized approach
  export_query <- paste0(
    "COPY (SELECT * FROM ", table_name, ") TO '", output_path, 
    "' (DELIMITER ',', HEADER, FORMAT CSV, COMPRESSION 'gzip')"
  )
  
  tryCatch({
    DBI::dbExecute(connection, export_query)
    
    # Get file size
    file_size_mb <- round(as.numeric(fs::file_size(output_path)) / (1024 * 1024), 2)
    logger::log_success("Successfully exported data to {output_path} ({file_size_mb} MB)")
  }, error = function(e) {
    logger::log_error("Failed to export to CSV: {e$message}")
    
    # Try a chunked export if the single export fails
    logger::log_info("Trying chunked CSV export...")
    chunked_export_success <- export_to_csv_in_chunks(connection, table_name, output_path, verbose)
    
    if (chunked_export_success) {
      logger::log_success("Successfully exported data in chunks to {output_path}")
    }
  })
}

#' Export data to CSV in chunks
#'
#' @param connection DuckDB connection
#' @param table_name Table to export
#' @param output_path Path to save CSV file
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating if export was successful
#' @noRd
export_to_csv_in_chunks <- function(connection, table_name, output_path, verbose) {
  # Get row count of table
  count_query <- paste0("SELECT COUNT(*) AS row_count FROM ", table_name)
  count_result <- DBI::dbGetQuery(connection, count_query)
  total_rows <- count_result$row_count[1]
  
  # Define chunk size
  chunk_size <- 500000  # Adjust based on memory constraints
  
  # Calculate number of chunks
  num_chunks <- ceiling(total_rows / chunk_size)
  logger::log_info("Exporting {total_rows} rows in {num_chunks} chunks")
  
  # Create output directory if it doesn't exist
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    logger::log_info("Created output directory: {output_dir}")
  }
  
  # Export each chunk to a separate file
  chunk_files <- character(num_chunks)
  
  for (chunk in 1:num_chunks) {
    offset <- (chunk - 1) * chunk_size
    
    # Create chunk file path
    chunk_file <- paste0(tools::file_path_sans_ext(output_path), 
                         "_chunk", chunk, "of", num_chunks, 
                         ".", tools::file_ext(output_path))
    
    # Export chunk
    chunk_query <- paste0(
      "COPY (SELECT * FROM ", table_name, " LIMIT ", chunk_size, " OFFSET ", offset, ") ",
      "TO '", chunk_file, "' (DELIMITER ',', HEADER, FORMAT CSV)"
    )
    
    tryCatch({
      DBI::dbExecute(connection, chunk_query)
      chunk_files[chunk] <- chunk_file
      logger::log_debug("Exported chunk {chunk}/{num_chunks} to {basename(chunk_file)}")
    }, error = function(e) {
      logger::log_error("Failed to export chunk {chunk}/{num_chunks}: {e$message}")
    })
  }
  
  # Combine chunks if all exports were successful
  if (all(file.exists(chunk_files))) {
    logger::log_info("Combining {length(chunk_files)} chunks into a single file")
    
    tryCatch({
      # Combine files, keeping header only from first file
      first_chunk <- readLines(chunk_files[1])
      header <- first_chunk[1]
      combined_data <- c(header)
      
      for (i in 1:length(chunk_files)) {
        chunk_data <- readLines(chunk_files[i])
        if (i > 1) {
          # Skip header for all but first chunk
          chunk_data <- chunk_data[-1]
        }
        combined_data <- c(combined_data, chunk_data)
        
        # Remove chunk file after reading
        file.remove(chunk_files[i])
      }
      
      # Write combined file
      writeLines(combined_data, output_path)
      
      file_size_mb <- round(as.numeric(fs::file_size(output_path)) / (1024 * 1024), 2)
      logger::log_success("Successfully combined chunks into {output_path} ({file_size_mb} MB)")
      
      return(TRUE)
    }, error = function(e) {
      logger::log_error("Failed to combine chunks: {e$message}")
      return(FALSE)
    })
  } else {
    logger::log_error("Not all chunks were exported successfully")
    return(FALSE)
  }
}

# Load required libraries
library(dplyr)
library(logger)
library(duckdb)
library(fs)
library(assertthat)
library(purrr)
library(tibble)
library(tools)

# Set your data paths
# Define paths
base_dir <- "/Volumes/Video Projects Muffly 1/open_payments_data"
db_path <- "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/unzipped_p_files/nppes_my_duckdb.duckdb"




# Process data for years 2021-2023
payments_data <- process_open_payments_data_optimized(
  base_dir = base_dir,
  db_path = db_path,
  years = c("2021", "2022", "2023"),
  payment_type = "general",
  output_table_name = "open_payments_merged",  # Give it a specific name
  verbose = TRUE
)

# Get a list of all tables
all_tables <- DBI::dbListTables(payments_data$connection)
print(all_tables)

# Store the connection and table name
conn <- payments_data$connection
table_name <- payments_data$table_name

# Check if the table exists and has data
table_exists <- table_name %in% DBI::dbListTables(conn)
if (table_exists) {
  row_count <- DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) AS n FROM ", table_name))$n
  print(paste("Table", table_name, "exists with", row_count, "rows"))
  
  # Now query the data
  result <- tbl(conn, table_name) %>%
    filter(Program_Year == "2022") %>%
    collect()
} else {
  print(paste("Table", table_name, "does not exist!"))
}

# Close the connection when done
dbDisconnect(db_connection)

