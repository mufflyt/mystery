#' Count Physicians by State or Subdivision with Logging
#'
#' This function counts physicians grouped by either state or U.S. Census subdivision,
#' using the `us_census_bureau_regions_df` dataset. It logs inputs, transformations,
#' and outputs. The results can optionally be saved to a CSV file.
#'
#' @param data A dataframe containing physician data, including state, phone, and name columns.
#' @param state_name_column A string specifying the column name for state information in the dataset.
#'   Default is \code{"state_code"}.
#' @param phone_column A string specifying the column name for phone numbers in the dataset.
#'   Default is \code{"phone_number"}.
#' @param first_name_column A string specifying the column name for physicians' first names.
#'   Default is \code{"first"}.
#' @param last_name_column A string specifying the column name for physicians' last names.
#'   Default is \code{"last"}.
#' @param group_by A string specifying how to group the counts. Options are \code{"state"} for
#'   state-level counts and \code{"subdivision"} for U.S. Census subdivision counts.
#'   Default is \code{"state"}.
#' @param output_to_csv A string specifying the file path to save the counts as a CSV file.
#'   If \code{NULL}, no file will be saved. Default is \code{NULL}.
#'
#' @return A dataframe with counts of physicians grouped by state or subdivision.
#'
#' @examples
#' # Example 1: Count physicians by state
#' state_counts <- count_physicians_by_group(
#'   data = physicians_data,
#'   state_name_column = "state_code",
#'   phone_column = "phone",
#'   first_name_column = "first_name",
#'   last_name_column = "last_name",
#'   group_by = "state"
#' )
#'
#' # Example 2: Count physicians by U.S. Census subdivision
#' subdivision_counts <- count_physicians_by_group(
#'   data = physicians_data,
#'   state_name_column = "state_code",
#'   phone_column = "phone",
#'   first_name_column = "first_name",
#'   last_name_column = "last_name",
#'   group_by = "subdivision"
#' )
#'
#' # Example 3: Save counts to a CSV file
#' count_physicians_by_group(
#'   data = physicians_data,
#'   group_by = "state",
#'   output_to_csv = "state_counts.csv"
#' )
#'
#' @importFrom dplyr filter group_by summarize left_join n
#' @importFrom readr write_csv
#' @importFrom glue glue
#' @export
count_physicians_by_group <- function(data,
                                      state_name_column = "state_code",
                                      phone_column = "phone_number",
                                      first_name_column = "first",
                                      last_name_column = "last",
                                      group_by = "state",
                                      output_to_csv = NULL) {

  # Log the inputs
  logger::log_info(glue::glue("Data dimensions: {nrow(data)} rows and {ncol(data)} columns"))
  logger::log_info(glue::glue("State name column: {state_name_column}, Phone column: {phone_column}, First name column: {first_name_column}, Last name column: {last_name_column}, Group by: {group_by}"))

  # Ensure `us_census_bureau_regions_df` is available
  if (!exists("us_census_bureau_regions_df")) {
    stop("The dataset `us_census_bureau_regions_df` is required but not found.")
  }

  # Step 1: Standardize state names or codes and join with region/subdivision data
  data <- data %>%
    dplyr::rename(State = !!rlang::sym(state_name_column)) %>%
    dplyr::mutate(State = tolower(State)) %>%
    dplyr::left_join(us_census_bureau_regions_df %>%
                       dplyr::mutate(State = tolower(State)),
                     by = "State")

  if (group_by == "state") {
    # Group by state and count physicians
    counts <- data %>%
      dplyr::group_by(State) %>%
      dplyr::summarize(total_available = dplyr::n(), .groups = "drop")
  } else if (group_by == "subdivision") {
    # Group by subdivision and count physicians
    counts <- data %>%
      dplyr::group_by(Division) %>%
      dplyr::summarize(total_available = dplyr::n(), .groups = "drop")
  } else {
    stop("Invalid value for `group_by`. Use 'state' or 'subdivision'.")
  }

  # Step 3: Save counts to a CSV if required
  if (!is.null(output_to_csv)) {
    readr::write_csv(counts, output_to_csv)
    logger::log_info(glue::glue("Counts saved to: {output_to_csv}"))
  }

  # Log and return results
  logger::log_info(glue::glue("Returning counts grouped by {group_by}."))
  return(counts)
}
