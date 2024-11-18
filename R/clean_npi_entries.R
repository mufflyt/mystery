#' Clean NPI Entries Function
#'
#' This function cleans NPI search results by normalizing credentials, applying filters
#' for taxonomies, and summarizing entries by NPI. It includes console logging of key steps.
#'
#' @param npi_entries A dataframe containing NPI search results.
#' @param basic_credentials A character vector of credentials to filter by (default is `c("MD", "DO")`).
#' @param taxonomy_filter A string for filtering taxonomies (default is `"Obstetrics & Gynecology"`).
#'
#' @return A cleaned dataframe with summarized NPI entries.
#' @importFrom dplyr mutate filter select group_by summarise first
#' @importFrom stringr str_remove_all str_squish str_detect regex
#' @importFrom logger log_info
#' @export
#' @examples
#' # Example 1: Basic cleaning of NPI entries with default parameters
#' clean_npi_entries(npi_results)
#'
#' # Example 2: Cleaning NPI entries, filtering for a specific taxonomy
#' clean_npi_entries(npi_results, taxonomy_filter = "Anesthesiology")
#'
#' # Example 3: Cleaning NPI entries, specifying different credentials
#' clean_npi_entries(npi_results, basic_credentials = c("PA", "NP"))
clean_npi_entries <- function(npi_entries, basic_credentials = c("MD", "DO"), taxonomy_filter = "Obstetrics & Gynecology") {
  # Start logging
  logger::log_info("Starting clean_npi_entries function")
  logger::log_info("Input basic credentials: {basic_credentials}")
  logger::log_info("Input taxonomy filter: {taxonomy_filter}")

  # Log the initial input data
  logger::log_info("Initial NPI entries: {nrow(npi_entries)} rows")

  # Function to normalize credentials (remove punctuation and extra spaces)
  normalize_credential <- function(credential) {
    credential <- stringr::str_remove_all(credential, "[[:punct:]]") # Remove punctuation
    credential <- stringr::str_squish(credential)                    # Remove extra spaces
    credential <- toupper(credential)                                # Convert to uppercase
    return(credential)
  }

  # Create a lookup table for state abbreviations to full names
  state_lookup <- tibble::tibble(
    abbreviation = state.abb,
    full_name = state.name
  )

  # Normalize the basic credentials and filter
  logger::log_info("Normalizing basic credentials and applying filter")
  npi_entries <- npi_entries %>%
    dplyr::mutate(basic_credential = normalize_credential(basic_credential)) %>%
    dplyr::filter(basic_credential %in% normalize_credential(basic_credentials))

  logger::log_info("After filtering by basic credentials: {nrow(npi_entries)} rows remaining")

  # Filter taxonomies if a taxonomy_filter argument is provided
  if (!is.null(taxonomy_filter)) {
    logger::log_info("Applying taxonomy filter: {taxonomy_filter}")
    npi_entries <- npi_entries %>%
      dplyr::filter(stringr::str_detect(taxonomies_desc, stringr::regex(taxonomy_filter, ignore_case = TRUE)))

    logger::log_info("After filtering by taxonomy: {nrow(npi_entries)} rows remaining")
  }

  # Remove unnecessary columns (identifiers, basic_status)
  logger::log_info("Removing identifiers_desc and basic_status columns")
  npi_entries <- npi_entries %>%
    dplyr::select(-identifiers_desc, -basic_status)

  # Convert state abbreviations to full state names
  logger::log_info("Converting state abbreviations to full state names using a lookup table")
  npi_entries <- npi_entries %>%
    dplyr::left_join(state_lookup, by = c("addresses_state" = "abbreviation")) %>%
    dplyr::mutate(addresses_state = ifelse(!is.na(full_name), full_name, addresses_state)) %>%
    dplyr::select(-full_name) # Drop the helper column after mapping

  # Filter to keep only LOCATION purpose entries
  logger::log_info("Filtering for LOCATION address purpose")
  cleaned_npi <- npi_entries %>%
    dplyr::filter(addresses_address_purpose == "LOCATION")

  logger::log_info("After summarizing: {nrow(cleaned_npi)} rows")

  # Log the final output
  logger::log_info("Final cleaned NPI entries: {nrow(cleaned_npi)} rows")

  # Return cleaned NPI entries
  return(cleaned_npi)
}
