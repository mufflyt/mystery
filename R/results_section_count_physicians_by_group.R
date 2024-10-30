#' Count Physicians by State or Subdivision
#'
#' This function counts the number of physicians available per state or US Census Bureau subdivision.
#' It can optionally save the counts to a CSV file.
#'
#' @param data A dataframe containing the physician data.
#' @param state_name_column The column name in the dataframe containing state names or abbreviations (default is "state_code").
#' @param phone_column The column name in the dataframe containing phone numbers (default is "phone_number").
#' @param first_name_column The column name in the dataframe containing first names (default is "first").
#' @param last_name_column The column name in the dataframe containing last names (default is "last").
#' @param group_by A string indicating whether to group by "state" or "subdivision" (default is "state").
#' @param output_to_csv (Optional) A file path to save the state/subdivision counts to a CSV file. If NULL, no file is saved.
#'
#' @return A tibble with the counts of physicians per state or subdivision.
#'
#' @importFrom dplyr mutate filter group_by summarize arrange if_else
#' @importFrom purrr map flatten_chr
#' @importFrom readr write_csv
#' @importFrom exploratory statecode
#' @examples
#' # Example 1: Count physicians by state
#' count_physicians_by_group(taxonomy_and_aaos_data)
#'
#' # Example 2: Count physicians by U.S. Census Bureau subdivision
#' count_physicians_by_group(taxonomy_and_aaos_data, group_by = "subdivision")
#'
#' @export
count_physicians_by_group <- function(data,
                                      state_name_column = "state_code",
                                      phone_column = "phone_number",
                                      first_name_column = "first",
                                      last_name_column = "last",
                                      group_by = "state",  # New argument to group by state or subdivision
                                      output_to_csv = NULL) {

  # Log the inputs to the function
  logger::log_info(glue::glue("Data dimensions: {nrow(data)} rows and {ncol(data)} columns"))
  logger::log_info(glue::glue("State name column: {state_name_column}, Phone column: {phone_column}, First name column: {first_name_column}, Last name column: {last_name_column}, Group by: {group_by}"))

  # US Census Bureau Divisions (subdivisions)
  us_census_subdivisions <- list(
    "New England" = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont"),
    "Middle Atlantic" = c("new york", "new jersey", "pennsylvania"),
    "East North Central" = c("ohio", "indiana", "illinois", "michigan", "wisconsin"),
    "West North Central" = c("minnesota", "iowa", "missouri", "north dakota", "south dakota", "nebraska", "kansas"),
    "South Atlantic" = c("delaware", "maryland", "district of columbia", "virginia", "west virginia", "north carolina", "south carolina", "georgia", "florida"),
    "East South Central" = c("kentucky", "tennessee", "alabama", "mississippi"),
    "West South Central" = c("arkansas", "louisiana", "oklahoma", "texas"),
    "Mountain" = c("montana", "idaho", "wyoming", "colorado", "new mexico", "arizona", "utah", "nevada"),
    "Pacific" = c("alaska", "washington", "oregon", "california", "hawaii")
  )

  # Map states to their respective subdivisions
  state_to_subdivision <- purrr::map(names(us_census_subdivisions), ~set_names(rep(.x, length(us_census_subdivisions[[.x]])), us_census_subdivisions[[.x]])) %>%
    purrr::flatten_chr()

  # Step 1: Convert state abbreviations to full names
  data <- convert_state_names(data, state_name_column)

  # Step 2: Filter and count physicians, by either state or subdivision
  if (group_by == "state") {
    all_states <- tibble::tibble(state_code = tolower(state.name))
    state_counts <- count_physicians_per_state(data, state_name_column, phone_column, first_name_column, last_name_column, all_states)
  } else if (group_by == "subdivision") {
    data <- data %>%
      dplyr::mutate(subdivision = state_to_subdivision[tolower(!!rlang::sym(state_name_column))]) %>%
      dplyr::filter(!is.na(subdivision))  # Filter out any states not mapped
    state_counts <- data %>%
      dplyr::group_by(subdivision) %>%
      dplyr::summarize(total_available = dplyr::n())
  }

  # Save the counts to a CSV if required
  if (!is.null(output_to_csv)) {
    readr::write_csv(state_counts, output_to_csv)
    logger::log_info(glue::glue("State or subdivision counts saved to CSV file at: {output_to_csv}"))
  }

  return(state_counts)
}
