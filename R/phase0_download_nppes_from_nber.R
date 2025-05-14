#' Download and Process NPPES Data from NBER Repository
#'
#' @description
#' This function downloads National Provider Identifier (NPI) data files from the
#' National Bureau of Economic Research (NBER) repository. It handles downloading,
#' extraction, and validation of files containing historical snapshots of
#' healthcare providers in the United States.
#'
#' @param dest_dir Character string. Directory where files will be downloaded and
#'        extracted. Default is "nppes_data" in the current working directory.
#' @param file_list Character vector. List of specific filenames to download. If NULL
#'        (default), downloads a predefined set of historical NPPES files from
#'        2007-2022.
#' @param download_only Logical. If TRUE, files will only be downloaded but not
#'        extracted. Default is FALSE.
#' @param skip_existing Logical. If TRUE, skips files that already exist in the
#'        destination directory. Default is TRUE.
#' @param max_retries Numeric. Maximum number of download retry attempts for each
#'        file. Default is 3.
#' @param timeout Numeric. Download timeout in seconds. Default is 600 (10 minutes).
#' @param parallel Logical. If TRUE, downloads files in parallel using parallel
#'        processing. Default is FALSE.
#' @param n_cores Numeric. Number of cores to use for parallel processing.
#'        Default is 2. Only used if parallel = TRUE.
#' @param verbose Logical. Whether to print detailed progress information.
#'        Default is TRUE.
#'
#' @return A tibble containing information about the processed files with columns:
#'         filename, status (success/failed), path, and notes.
#'
#' @examples
#' # Example 1: Download all historical NPPES files with default settings
#' \dontrun{
#' download_results <- download_nppes_from_nber(
#'   dest_dir = "data/nppes_historical",
#'   verbose = TRUE
#' )
#' # Check which files were downloaded successfully
#' download_results %>%
#'   dplyr::filter(status == "success")
#' }
#'
#' # Example 2: Download only specific years
#' \dontrun{
#' specific_files <- download_nppes_from_nber(
#'   dest_dir = "data/nppes_historical",
#'   file_list = c("NPPES_Data_Dissemination_April_2019.zip",
#'                "NPPES_Data_Dissemination_April_2020.zip"),
#'   verbose = TRUE
#' )
#' }
#'
#' # Example 3: Download files without extracting them
#' \dontrun{
#' zip_files <- download_nppes_from_nber(
#'   dest_dir = "data/nppes_historical",
#'   file_list = c("NPPES_Data_Dissemination_April_2018.zip"),
#'   download_only = TRUE,
#'   verbose = TRUE
#' )
#' }
#'
#' @import dplyr
#' @importFrom assertthat assert_that is.flag has_extension
#' @importFrom logger log_info log_success log_error log_debug log_layout layout_simple
#'             log_threshold DEBUG INFO
#' @importFrom utils download.file unzip
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport parLapply
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom stringr str_detect
#' @importFrom readr read_csv
#' @importFrom purrr map map_dfr
#'
#' @export
download_nppes_from_nber <- function(dest_dir = "nppes_data",
                                     file_list = NULL,
                                     download_only = FALSE,
                                     skip_existing = TRUE,
                                     max_retries = 3,
                                     timeout = 600,
                                     parallel = FALSE,
                                     n_cores = 2,
                                     verbose = TRUE) {

  # Configure logging
  setup_nppes_logging(verbose)

  # Validate input parameters
  validate_nppes_download_params(
    dest_dir, file_list, download_only, skip_existing,
    max_retries, timeout, parallel, n_cores, verbose
  )

  # Create download directory if it doesn't exist
  create_nppes_directory(dest_dir)

  # Base URL for the NBER repository
  base_url <- "http://data.nber.org/nppes/zip-orig/"

  # Use default file list if none provided
  if (is.null(file_list)) {
    file_list <- get_default_nppes_file_list()
    logger::log_info("Using default file list with {length(file_list)} files")
  } else {
    logger::log_info("Using custom file list with {length(file_list)} files")
  }

  # Log download parameters
  log_nppes_download_parameters(
    dest_dir, length(file_list), download_only,
    skip_existing, max_retries, timeout, parallel, n_cores
  )

  # Process files (parallel or sequential)
  if (parallel && length(file_list) > 1) {
    result_list <- process_nppes_files_parallel(
      file_list, base_url, dest_dir, download_only, skip_existing,
      max_retries, timeout, n_cores, verbose
    )
  } else {
    result_list <- process_nppes_files_sequential(
      file_list, base_url, dest_dir, download_only, skip_existing,
      max_retries, timeout, verbose
    )
  }

  # Combine results into a tibble
  results_tibble <- bind_nppes_download_results(result_list)

  # Summarize results
  summarize_nppes_download_results(results_tibble)

  # Return results
  return(results_tibble)
}

#' Get default list of NPPES files to download
#'
#' @return Character vector of filenames
#'
#' @noRd
get_default_nppes_file_list <- function() {
  c(
    "NPPES_Data_Disseminat_April_2021.zip",
    "NPPES_Data_Disseminat_April_2022.zip",
    "NPPES_Data_Dissemination_Apr_2009.zip",
    "NPPES_Data_Dissemination_Apr_2012.zip",
    "NPPES_Data_Dissemination_Apr_2013.zip",
    "NPPES_Data_Dissemination_Apr_2014.zip",
    "NPPES_Data_Dissemination_Apr_2016.zip",
    "NPPES_Data_Dissemination_April_2011.zip",
    "NPPES_Data_Dissemination_April_2015.zip",
    "NPPES_Data_Dissemination_April_2017.zip",
    "NPPES_Data_Dissemination_April_2018.zip",
    "NPPES_Data_Dissemination_April_2019.zip",
    "NPPES_Data_Dissemination_Feb_2010.zip",
    "NPPES_Data_Dissemination_February_2020.zip",
    "NPPES_Data_Dissemination_July_2020.zip",
    "NPPES_Data_Dissemination_June_2009.zip",
    "NPPES_Data_Dissemination_May_2008.zip",
    "NPPES_Data_Dissemination_Nov_2007.zip",
    "NPPES_Data_Dissemination_October_2020.zip"
  )
}

#' Set up logging configuration
#'
#' @param verbose Logical. Whether to use debug or info level
#'
#' @noRd
setup_nppes_logging <- function(verbose) {
  logger::log_layout(logger::layout_simple)
  log_level <- if (verbose) logger::DEBUG else logger::INFO
  logger::log_threshold(log_level)
}

#' Validate input parameters
#'
#' @param dest_dir Directory for downloads
#' @param file_list List of files to download
#' @param download_only Whether to skip extraction
#' @param skip_existing Whether to skip existing files
#' @param max_retries Maximum download retries
#' @param timeout Download timeout in seconds
#' @param parallel Whether to use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param verbose Logging verbosity
#'
#' @noRd
validate_nppes_download_params <- function(dest_dir, file_list, download_only,
                                           skip_existing, max_retries, timeout,
                                           parallel, n_cores, verbose) {
  # Validate string parameters
  assertthat::assert_that(is.character(dest_dir),
                          msg = "dest_dir must be a character string")

  if (!is.null(file_list)) {
    assertthat::assert_that(is.character(file_list),
                            msg = "file_list must be a character vector")
    assertthat::assert_that(all(tools::file_ext(file_list) == "zip"),
                            msg = "All files in file_list must have .zip extension")
  }

  # Validate logical parameters
  assertthat::assert_that(assertthat::is.flag(download_only),
                          msg = "download_only must be logical (TRUE/FALSE)")
  assertthat::assert_that(assertthat::is.flag(skip_existing),
                          msg = "skip_existing must be logical (TRUE/FALSE)")
  assertthat::assert_that(assertthat::is.flag(parallel),
                          msg = "parallel must be logical (TRUE/FALSE)")
  assertthat::assert_that(assertthat::is.flag(verbose),
                          msg = "verbose must be logical (TRUE/FALSE)")

  # Validate numeric parameters
  assertthat::assert_that(is.numeric(max_retries), max_retries >= 1,
                          msg = "max_retries must be a positive number")
  assertthat::assert_that(is.numeric(timeout), timeout > 0,
                          msg = "timeout must be a positive number")
  assertthat::assert_that(is.numeric(n_cores), n_cores >= 1,
                          msg = "n_cores must be a positive number")

  # Check if n_cores exceeds available cores
  if (parallel) {
    available_cores <- parallel::detectCores() - 1
    if (available_cores < 1) available_cores <- 1

    if (n_cores > available_cores) {
      logger::log_warn("Requested {n_cores} cores, but only {available_cores} available. Using {available_cores} cores.")
      n_cores <- available_cores
    }
  }
}

#' Create directory for NPPES downloads
#'
#' @param dest_dir Directory path
#'
#' @noRd
create_nppes_directory <- function(dest_dir) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    logger::log_info("Created download directory: {dest_dir}")
  } else {
    logger::log_debug("Download directory already exists: {dest_dir}")
  }
}

#' Log download parameters
#'
#' @param dest_dir Directory for downloads
#' @param file_count Number of files to download
#' @param download_only Whether to skip extraction
#' @param skip_existing Whether to skip existing files
#' @param max_retries Maximum download retries
#' @param timeout Download timeout in seconds
#' @param parallel Whether to use parallel processing
#' @param n_cores Number of cores for parallel processing
#'
#' @noRd
log_nppes_download_parameters <- function(dest_dir, file_count, download_only,
                                          skip_existing, max_retries, timeout,
                                          parallel, n_cores) {
  logger::log_info("NPPES Download Configuration:")
  logger::log_debug("- Destination directory: {dest_dir}")
  logger::log_debug("- Files to download: {file_count}")
  logger::log_debug("- Download only (no extraction): {download_only}")
  logger::log_debug("- Skip existing files: {skip_existing}")
  logger::log_debug("- Maximum retries: {max_retries}")
  logger::log_debug("- Download timeout: {timeout} seconds")
  logger::log_debug("- Parallel processing: {parallel}")
  if (parallel) {
    logger::log_debug("- Number of cores: {n_cores}")
  }
}

#' Process NPPES files in parallel
#'
#' @param file_list List of files to download
#' @param base_url Base URL for downloads
#' @param dest_dir Destination directory
#' @param download_only Whether to skip extraction
#' @param skip_existing Whether to skip existing files
#' @param max_retries Maximum download retries
#' @param timeout Download timeout in seconds
#' @param n_cores Number of cores for parallel processing
#' @param verbose Logging verbosity
#'
#' @return List of download results
#'
#' @noRd
process_nppes_files_parallel <- function(file_list, base_url, dest_dir,
                                         download_only, skip_existing,
                                         max_retries, timeout, n_cores, verbose) {
  logger::log_info("Processing {length(file_list)} files in parallel using {n_cores} cores")

  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl))

  # Export required functions and packages to the cluster
  parallel::clusterExport(cl, varlist = c(
    "download_and_extract_npi_file", "download_npi_file", "extract_npi_file",
    "generate_download_result", "retry_download"
  ), envir = environment())

  # Make sure all nodes have the logger package loaded
  parallel::clusterEvalQ(cl, {
    if (!requireNamespace("logger", quietly = TRUE)) {
      install.packages("logger")
    }
    library(logger)
    log_threshold(if (verbose) DEBUG else INFO)
  })

  # Process files in parallel
  results <- parallel::parLapply(cl, file_list, function(file_name) {
    download_and_extract_npi_file(
      file_name, base_url, dest_dir, !download_only,
      skip_existing, max_retries, timeout
    )
  })

  return(results)
}

#' Process NPPES files sequentially
#'
#' @param file_list List of files to download
#' @param base_url Base URL for downloads
#' @param dest_dir Destination directory
#' @param download_only Whether to skip extraction
#' @param skip_existing Whether to skip existing files
#' @param max_retries Maximum download retries
#' @param timeout Download timeout in seconds
#' @param verbose Logging verbosity
#'
#' @return List of download results
#'
#' @noRd
process_nppes_files_sequential <- function(file_list, base_url, dest_dir,
                                           download_only, skip_existing,
                                           max_retries, timeout, verbose) {
  logger::log_info("Processing {length(file_list)} files sequentially")

  # Initialize list to store results
  results <- vector("list", length(file_list))

  # Process each file
  for (i in seq_along(file_list)) {
    file_name <- file_list[i]
    logger::log_info("[{i}/{length(file_list)}] Processing {file_name}")

    results[[i]] <- download_and_extract_npi_file(
      file_name, base_url, dest_dir, !download_only,
      skip_existing, max_retries, timeout
    )
  }

  return(results)
}

#' Download and extract a single NPI data file
#'
#' @param file_name Name of the zip file to download
#' @param base_url Base URL for the NBER repository
#' @param dest_dir Local directory for storing downloaded files
#' @param extract Whether to extract the zip file after downloading
#' @param skip_existing Whether to skip processing if file exists
#' @param max_retries Maximum download retries
#' @param timeout Download timeout in seconds
#'
#' @return A list with download result information
#'
#' @noRd
download_and_extract_npi_file <- function(file_name, base_url, dest_dir,
                                          extract = TRUE, skip_existing = TRUE,
                                          max_retries = 3, timeout = 600) {
  # Construct full URL and destination path
  file_url <- paste0(base_url, file_name)
  dest_path <- file.path(dest_dir, file_name)

  # Check if file already exists and should be skipped
  if (skip_existing && file.exists(dest_path)) {
    logger::log_info("File already exists: {dest_path}")

    if (extract) {
      extract_dir <- file.path(dest_dir, tools::file_path_sans_ext(file_name))
      extraction_result <- extract_npi_file(dest_path, extract_dir, skip_existing)

      if (extraction_result$status == "success") {
        return(generate_download_result(
          file_name, "success", dest_path,
          paste0("File already existed and was extracted to ", extract_dir)
        ))
      } else {
        return(generate_download_result(
          file_name, "partial", dest_path,
          paste0("File already existed but extraction failed: ", extraction_result$message)
        ))
      }
    } else {
      return(generate_download_result(
        file_name, "success", dest_path, "File already existed, no extraction requested"
      ))
    }
  }

  # Download file with retries
  download_result <- retry_download(
    file_url, dest_path, max_retries, timeout
  )

  # If download failed, return failure result
  if (download_result$status != "success") {
    return(generate_download_result(
      file_name, "failed", NA, paste0("Download failed: ", download_result$message)
    ))
  }

  # If no extraction requested, return success
  if (!extract) {
    return(generate_download_result(
      file_name, "success", dest_path, "Downloaded successfully, no extraction requested"
    ))
  }

  # Extract file
  extract_dir <- file.path(dest_dir, tools::file_path_sans_ext(file_name))
  extraction_result <- extract_npi_file(dest_path, extract_dir, FALSE)

  if (extraction_result$status == "success") {
    return(generate_download_result(
      file_name, "success", extract_dir,
      paste0("Downloaded and extracted to ", extract_dir)
    ))
  } else {
    return(generate_download_result(
      file_name, "partial", dest_path,
      paste0("Downloaded but extraction failed: ", extraction_result$message)
    ))
  }
}

#' Retry download with multiple attempts
#'
#' @param file_url URL to download from
#' @param dest_path Path to save file
#' @param max_retries Maximum retry attempts
#' @param timeout Download timeout in seconds
#'
#' @return List with download result
#'
#' @noRd
retry_download <- function(file_url, dest_path, max_retries, timeout) {
  for (attempt in 1:max_retries) {
    logger::log_debug("Download attempt {attempt}/{max_retries} for {file_url}")

    download_result <- download_npi_file(file_url, dest_path, timeout)

    if (download_result$status == "success") {
      return(download_result)
    }

    logger::log_warn("Attempt {attempt} failed: {download_result$message}")

    # Wait before retrying (increasing backoff)
    if (attempt < max_retries) {
      wait_time <- 2^attempt
      logger::log_debug("Waiting {wait_time} seconds before retry...")
      Sys.sleep(wait_time)
    }
  }

  return(list(
    status = "failed",
    message = paste0("Failed after ", max_retries, " attempts")
  ))
}

#' Download an NPI file
#'
#' @param file_url URL to download from
#' @param dest_path Path to save file
#' @param timeout Download timeout in seconds
#'
#' @return List with download result
#'
#' @noRd
download_npi_file <- function(file_url, dest_path, timeout) {
  tryCatch({
    utils::download.file(
      url = file_url,
      destfile = dest_path,
      method = "auto",
      quiet = FALSE,
      mode = "wb",
      timeout = timeout
    )

    # Verify file size
    file_size <- file.info(dest_path)$size
    if (is.na(file_size) || file_size == 0) {
      file.remove(dest_path)
      return(list(
        status = "failed",
        message = "Downloaded file is empty"
      ))
    }

    logger::log_success("Downloaded successfully: {dest_path} ({round(file_size/1024/1024, 2)} MB)")
    return(list(
      status = "success",
      message = "Downloaded successfully",
      path = dest_path,
      size = file_size
    ))
  }, error = function(e) {
    # Remove partial downloads
    if (file.exists(dest_path)) {
      file.remove(dest_path)
    }

    return(list(
      status = "failed",
      message = e$message
    ))
  })
}

#' Extract an NPI zip file
#'
#' @param zip_path Path to zip file
#' @param extract_dir Directory to extract to
#' @param skip_if_exists Skip if extraction directory exists and has files
#'
#' @return List with extraction result
#'
#' @noRd
extract_npi_file <- function(zip_path, extract_dir, skip_if_exists = TRUE) {
  # Check if extraction directory already has files
  if (skip_if_exists && dir.exists(extract_dir)) {
    files_in_dir <- list.files(extract_dir)
    if (length(files_in_dir) > 0) {
      logger::log_info("Extraction directory already contains files: {extract_dir}")
      return(list(
        status = "success",
        message = "Extraction directory already contains files",
        path = extract_dir
      ))
    }
  }

  # Create extraction directory if it doesn't exist
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
  }

  logger::log_info("Extracting {zip_path} to {extract_dir}...")

  tryCatch({
    utils::unzip(zip_path, exdir = extract_dir)

    # Verify extraction
    files_in_dir <- list.files(extract_dir)
    if (length(files_in_dir) == 0) {
      return(list(
        status = "failed",
        message = "Extraction produced no files"
      ))
    }

    logger::log_success("Extracted {length(files_in_dir)} files to {extract_dir}")
    return(list(
      status = "success",
      message = paste0("Extracted ", length(files_in_dir), " files"),
      path = extract_dir,
      file_count = length(files_in_dir)
    ))
  }, error = function(e) {
    return(list(
      status = "failed",
      message = e$message
    ))
  })
}

#' Generate download result
#'
#' @param filename Filename
#' @param status Download status
#' @param path File path
#' @param notes Additional notes
#'
#' @return List with result information
#'
#' @noRd
generate_download_result <- function(filename, status, path, notes) {
  list(
    filename = filename,
    status = status,
    path = path,
    notes = notes,
    timestamp = Sys.time()
  )
}

#' Bind download results into a tibble
#'
#' @param result_list List of download results
#'
#' @return Tibble with combined results
#'
#' @noRd
bind_nppes_download_results <- function(result_list) {
  tibble::tibble(
    filename = purrr::map_chr(result_list, ~ .x$filename),
    status = purrr::map_chr(result_list, ~ .x$status),
    path = purrr::map_chr(result_list, ~ ifelse(is.na(.x$path), NA_character_, as.character(.x$path))),
    notes = purrr::map_chr(result_list, ~ .x$notes),
    timestamp = purrr::map(result_list, ~ .x$timestamp)
  )
}

#' Summarize download results
#'
#' @param results_tibble Tibble with download results
#'
#' @noRd
summarize_nppes_download_results <- function(results_tibble) {
  success_count <- sum(results_tibble$status == "success")
  partial_count <- sum(results_tibble$status == "partial")
  failed_count <- sum(results_tibble$status == "failed")
  total_count <- nrow(results_tibble)

  logger::log_info("NPPES Download Summary:")
  logger::log_info("- Total files: {total_count}")
  logger::log_info("- Successfully processed: {success_count}")

  if (partial_count > 0) {
    logger::log_warn("- Partially successful: {partial_count}")
  }

  if (failed_count > 0) {
    logger::log_error("- Failed: {failed_count}")
  }

  success_percentage <- round(success_count / total_count * 100, 1)
  logger::log_info("- Success rate: {success_percentage}%")
}

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c("filename", "status", "path", "notes", "timestamp"))
