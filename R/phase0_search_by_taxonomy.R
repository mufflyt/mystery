#' Search NPI by Taxonomy
#'
#' This function performs a search on the NPI registry for a list of taxonomy descriptions and returns a combined dataframe of results.
#' It processes the returned data and cleans up unnecessary fields.
#'
#' @param taxonomy_to_search A character vector of taxonomy descriptions to search for in the NPI registry.
#'
#' @return A dataframe of cleaned NPI data, filtered by taxonomy and other criteria.
#' @importFrom dplyr bind_rows distinct mutate filter select arrange
#' @importFrom npi npi_search npi_flatten
#' @importFrom stringr str_remove_all str_to_lower str_detect regex
#' @export
search_by_taxonomy <- function(taxonomy_to_search) {
  # Create an empty data frame to store search results
  combined_data <- data.frame()
  cat("Starting search_by_taxonomy\n")

  # Loop over each taxonomy description
  for (taxonomy in taxonomy_to_search) {
    cat("Searching for taxonomy:", taxonomy, "\n")
    tryCatch({
      # Perform the search for the current taxonomy
      result <- npi::npi_search(
        taxonomy_description = taxonomy,
        country_code = "US",
        enumeration_type = "ind",
        limit = 1200
      )
      cat("Search completed for taxonomy:", taxonomy, "\n")

      if (!is.null(result)) {
        cat("Processing data for taxonomy:", taxonomy, "\n")
        # Process and filter the data for the current taxonomy
        data_taxonomy <- npi::npi_flatten(result)

        # Data transformations and filtering
        data_taxonomy <- dplyr::mutate(data_taxonomy, search_term = taxonomy)
        data_taxonomy <- dplyr::filter(data_taxonomy, addresses_country_name == "United States")
        data_taxonomy <- dplyr::mutate(data_taxonomy, basic_credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]"))
        data_taxonomy <- dplyr::filter(data_taxonomy, stringr::str_to_lower(basic_credential) %in% stringr::str_to_lower(c("MD", "DO")))
        data_taxonomy <- dplyr::arrange(data_taxonomy, basic_last_name)
        data_taxonomy <- dplyr::filter(data_taxonomy, stringr::str_detect(taxonomies_desc, taxonomy))

        # Selecting necessary columns, removing unwanted fields
        data_taxonomy <- dplyr::select(data_taxonomy, npi, basic_first_name, basic_last_name, basic_middle_name, basic_sole_proprietor,
                                       basic_gender, basic_enumeration_date, taxonomies_desc, taxonomies_primary,
                                       addresses_address_1, addresses_city, addresses_state, addresses_postal_code,
                                       addresses_telephone_number, search_term)

        # Append the data for the current taxonomy to the combined data frame
        combined_data <- dplyr::bind_rows(combined_data, data_taxonomy)
        cat("Data appended for taxonomy:", taxonomy, "\n")
      } else {
        cat("No data found for taxonomy:", taxonomy, "\n")
      }
    }, error = function(e) {
      message(sprintf("Error in search for %s:\n%s", taxonomy, e$message))
    })
  }

  return(combined_data)
}
