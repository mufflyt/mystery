#' Clean NPI Entries Function
#'
#' This function cleans NPI search results by normalizing credentials, applying filters
#' for taxonomies, and summarizing entries by NPI. It includes console logging of key steps.
#'
#' @param npi_entries A dataframe containing NPI search results.
#' @param basic_credentials A character vector of credentials to filter by (default is `c("MD", "DO")`).
#' @param taxonomy_filter A string for filtering taxonomies (default is `"Obstetrics & Gynecology"`).
#' @param verbose A logical value that controls whether detailed logging is shown during the process.
#'
#' @return A cleaned dataframe with summarized NPI entries.
#' @importFrom dplyr mutate filter select group_by summarise first left_join
#' @importFrom stringr str_remove_all str_squish str_detect regex
#' @importFrom assertthat assert_that has_name
#' @importFrom logger log_info
#' @examples
#' # Example 1: Basic cleaning of NPI entries with default parameters
#' clean_npi_entries(npi_results)
#'
#' # Example 2: Cleaning NPI entries, filtering for a specific taxonomy
#' clean_npi_entries(npi_results, taxonomy_filter = "Anesthesiology")
#'
#' # Example 3: Cleaning NPI entries, specifying different credentials
#' clean_npi_entries(npi_results, basic_credentials = c("PA", "NP"))
#' @export
phase0_clean_npi_entries <- function(npi_entries, basic_credentials = c("MD", "DO"),
                              taxonomy_filter = "Obstetrics & Gynecology", verbose = TRUE) {

  # Validate inputs using assertthat
  assertthat::assert_that(
    is.character(basic_credentials),
    msg = "`basic_credentials` must be a character vector."
  )
  assertthat::assert_that(
    is.string(taxonomy_filter),
    msg = "`taxonomy_filter` must be a single string."
  )
  assertthat::assert_that(
    has_name(npi_entries, "basic_credential"),
    msg = "`npi_entries` must contain a `basic_credential` column."
  )
  assertthat::assert_that(
    has_name(npi_entries, "taxonomies_desc"),
    msg = "`npi_entries` must contain a `taxonomies_desc` column."
  )
  assertthat::assert_that(
    has_name(npi_entries, "addresses_state"),
    msg = "`npi_entries` must contain an `addresses_state` column."
  )
  assertthat::assert_that(
    has_name(npi_entries, "addresses_address_purpose"),
    msg = "`npi_entries` must contain an `addresses_address_purpose` column."
  )

  # Start logging
  if (verbose) logger::log_info("Starting clean_npi_entries function")
  if (verbose) logger::log_info("Input basic credentials: {paste(basic_credentials, collapse = ', ')}")
  if (verbose) logger::log_info("Input taxonomy filter: {taxonomy_filter}")

  # Log the initial input data
  if (verbose) logger::log_info("Initial NPI entries: {nrow(npi_entries)} rows")

  # Function to normalize credentials (remove punctuation and extra spaces)
  normalize_credential <- function(credential) {
    credential <- stringr::str_remove_all(credential, "[[:punct:]]") # Remove punctuation
    credential <- stringr::str_squish(credential) # Remove extra spaces
    credential <- toupper(credential) # Convert to uppercase
    return(credential)
  }

  # Normalize the basic credentials and filter
  if (verbose) logger::log_info("Normalizing basic credentials and applying filter")
  npi_entries <- npi_entries %>%
    dplyr::mutate(basic_credential = normalize_credential(basic_credential)) %>%
    dplyr::filter(basic_credential %in% normalize_credential(basic_credentials))

  if (verbose) logger::log_info("After filtering by basic credentials: {nrow(npi_entries)} rows remaining")

  # Filter taxonomies if a taxonomy_filter argument is provided
  if (!is.null(taxonomy_filter)) {
    if (verbose) logger::log_info("Applying taxonomy filter: {taxonomy_filter}")
    npi_entries <- npi_entries %>%
      dplyr::filter(stringr::str_detect(taxonomies_desc, stringr::regex(taxonomy_filter, ignore_case = TRUE)))

    if (verbose) logger::log_info("After filtering by taxonomy: {nrow(npi_entries)} rows remaining")
  }

  # Remove unnecessary columns (identifiers, basic_status)
  if (verbose) logger::log_info("Removing unnecessary columns (identifiers_desc, basic_status)")
  if ("identifiers_desc" %in% names(npi_entries) && "basic_status" %in% names(npi_entries)) {
    npi_entries <- npi_entries %>%
      dplyr::select(-identifiers_desc, -basic_status)
  }

  # Convert state abbreviations to full state names using phase0_convert_state_abbreviations
  if (verbose) logger::log_info("Converting state abbreviations to full state names using phase0_convert_state_abbreviations")
  npi_entries <- npi_entries %>%
    dplyr::mutate(addresses_state = phase0_convert_state_abbreviations(addresses_state))

  # Filter to keep only LOCATION purpose entries
  if (verbose) logger::log_info("Filtering for LOCATION address purpose")
  cleaned_npi <- npi_entries %>%
    dplyr::filter(addresses_address_purpose == "LOCATION")

  if (verbose) logger::log_info("After summarizing: {nrow(cleaned_npi)} rows")

  # Log the final output
  if (verbose) logger::log_info("Final cleaned NPI entries: {nrow(cleaned_npi)} rows")

  # Return cleaned NPI entries
  return(cleaned_npi)
}
