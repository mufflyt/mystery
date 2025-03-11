#' Search and Process NPI Numbers with Detailed Logging
#'
#' @param npi_dataframe A data frame with a column 'npi' containing NPI numbers
#' @param records_per_chunk Integer. Records per CSV file chunk. Default: 10
#' @param save_directory Character. Directory to save chunks. Default: current directory
#'
#' @return A combined data frame of NPI search results
#' @importFrom dplyr filter bind_rows
#' @importFrom npi npi_flatten npi_search
#' @importFrom purrr map_dfr safely
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default settings
#' npi_list <- data.frame(npi = c("1447273792", "1023030814"))
#' clinician_info <- phase0_search_npi_by_number(
#'   npi_dataframe = npi_list
#' )
#'
#' # Example 2: Custom chunk size and directory
#' temp_dir <- tempdir()
#' clinician_info <- phase0_search_npi_by_number(
#'   npi_dataframe = npi_list,
#'   records_per_chunk = 5,
#'   save_directory = temp_dir
#' )
#'
#' # Example 3: Handle larger dataset with logging
#' npi_large <- data.frame(
#'   npi = c("1447273792", "1023030814", "1750380234")
#' )
#' clinician_info <- phase0_search_npi_by_number(
#'   npi_dataframe = npi_large,
#'   records_per_chunk = 2,
#'   save_directory = "npi_results"
#' )
phase0_search_npi_by_number <- function(npi_dataframe,
                                        records_per_chunk = 10,
                                        save_directory = NULL) {
  # Input validation with assertthat
  assertthat::assert_that(is.data.frame(npi_dataframe))
  assertthat::assert_that("npi" %in% colnames(npi_dataframe))

  save_directory <- ifelse(is.null(save_directory), getwd(), save_directory)

  # Log inputs
  log_inputs(npi_dataframe, records_per_chunk, save_directory)

  # Validate NPIs
  valid_npis <- validate_npi_dataframe(npi_dataframe)

  if (nrow(valid_npis) == 0) {
    logger::log_error("No valid NPIs found in input dataframe")
    return(data.frame())
  }

  # Search NPIs safely
  clinician_info <- search_npi_records(valid_npis$npi)

  if (nrow(clinician_info) > 0) {
    save_chunked_results(clinician_info, save_directory, records_per_chunk)
  } else {
    logger::log_info("No clinician information to save")
  }

  logger::log_info("Function completed successfully")
  return(clinician_info)
}

#' @noRd
log_inputs <- function(npi_dataframe, records_per_chunk, save_directory) {
  logger::log_info(glue::glue(
    "Inputs: {nrow(npi_dataframe)} NPIs, chunk_size={records_per_chunk}, dir={save_directory}"
  ))
}

#' @noRd
validate_npi_dataframe <- function(npi_dataframe) {
  valid_npis <- npi_dataframe %>%
    dplyr::filter(
      !is.na(npi),
      nchar(as.character(npi)) == 10,
      grepl("^[0-9]{10}$", as.character(npi))
    )

  logger::log_info(glue::glue("Found {nrow(valid_npis)} valid NPIs"))
  return(valid_npis)
}

#' @noRd
search_npi_records <- function(npi_numbers) {
  safe_search <- purrr::safely(function(npi) {
    logger::log_info(glue::glue("Searching NPI: {npi}"))
    search_result <- npi::npi_search(number = npi)

    if (!inherits(search_result, "npi_results")) {
      logger::log_error(glue::glue("Invalid result for NPI {npi}"))
      return(NULL)
    }

    flattened <- tryCatch({
      npi::npi_flatten(search_result, cols = c("basic", "taxonomies"))
    }, error = function(e) {
      logger::log_error(glue::glue("Flatten error for NPI {npi}: {e$message}"))
      NULL
    })

    return(flattened)
  })

  results <- purrr::map_dfr(npi_numbers, function(npi) {
    result <- safe_search(npi)
    if (!is.null(result$result)) result$result else data.frame()
  })

  logger::log_info(glue::glue("Retrieved data for {nrow(results)} NPIs"))
  return(results)
}

#' @noRd
save_chunked_results <- function(clinician_info, save_directory, records_per_chunk) {
  if (!dir.exists(save_directory)) {
    dir.create(save_directory, recursive = TRUE)
  }

  chunk_indices <- seq(1, nrow(clinician_info), by = records_per_chunk)

  for (i in chunk_indices) {
    end_idx <- min(i + records_per_chunk - 1, nrow(clinician_info))
    chunk <- clinician_info[i:end_idx, ]

    filename <- glue::glue("npi_results_{format(Sys.time(), '%Y%m%d%H%M%S')}_chunk_{i}.csv")
    filepath <- file.path(save_directory, filename)

    readr::write_csv(chunk, filepath)
    logger::log_info(glue::glue("Saved chunk to {filepath}"))
  }
}
