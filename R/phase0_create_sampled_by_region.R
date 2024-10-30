#' Phase 0 Sample Creation by Region for Ortho Spine
#'
#' This function processes taxonomy and AAOS data, merging it with Census Bureau data
#' to create a sampled dataset grouped by region for ortho spine. Outputs are saved as a CSV.
#'
#' @param taxonomy_and_aaos_tbl A tibble containing taxonomy and AAOS data.
#' @param census_bureau_tbl A tibble containing Census Bureau data with regional information.
#' @param output_csv_path The file path to save the sampled dataset. Defaults to `"ortho_spine/phase_0/sampled_by_region_ortho_spine.csv"`.
#' @return A tibble of sampled ortho spine records by region.
#' @examples
#' # Example 1: Basic usage with default output path
#' sampled_data <- phase0_create_sampled_by_region(taxonomy_and_aaos_tbl, census_bureau_tbl)
#'
#' # Example 2: Custom output path
#' sampled_data <- phase0_create_sampled_by_region(taxonomy_and_aaos_tbl, census_bureau_tbl,
#'                                                 output_csv_path = "custom_path/sample.csv")
#'
#' # Example 3: Using a subset of census_bureau_tbl for faster testing
#' sampled_data <- phase0_create_sampled_by_region(taxonomy_and_aaos_tbl,
#'                                                 dplyr::filter(census_bureau_tbl, Region == "West"),
#'                                                 output_csv_path = "test/sample.csv")
#' @export
#' @importFrom dplyr mutate filter group_by left_join select arrange row_number bind_rows
#' @importFrom exploratory statecode sample_rows
#' @importFrom readr write_csv
#' @importFrom logger log_info
phase0_create_sampled_by_region <- function(taxonomy_and_aaos_tbl, census_bureau_tbl, output_csv_path = "ortho_spine/phase_0/sampled_by_region_ortho_spine.csv") {
  logger::log_info("Starting phase0_create_sampled_by_region.")
  logger::log_info("Inputs provided: taxonomy_and_aaos_tbl with {nrow(taxonomy_and_aaos_tbl)} rows and {ncol(taxonomy_and_aaos_tbl)} columns.")
  logger::log_info("Inputs provided: census_bureau_tbl with {nrow(census_bureau_tbl)} rows and {ncol(census_bureau_tbl)} columns.")

  sampled_region_data <- taxonomy_and_aaos_tbl %>%
    add_full_state_names() %>%
    filter_valid_entries() %>%
    lowercase_state_codes() %>%
    join_census_regions(census_bureau_tbl) %>%
    sample_by_division() %>%
    assign_alternating_insurance() %>%
    arrange_final_columns()

  logger::log_info("Saving sampled data to {output_csv_path}.")
  readr::write_csv(sampled_region_data, output_csv_path)

  logger::log_info("Completed phase0_create_sampled_by_region. Output dataset has {nrow(sampled_region_data)} rows and {ncol(sampled_region_data)} columns.")

  return(sampled_region_data)
}

# Helper Functions --------------------------------------------------------

#' Add full state names using exploratory::statecode
#' @param tbl A tibble with a `state_code` column.
#' @return A tibble with `state_code` converted to full state names.
#' @examples
#' # Example 1: Convert state codes in a sample tibble
#' sample_data <- tibble::tibble(state_code = c("CO", "CA", "NY"))
#' add_full_state_names(sample_data)
#'
#' # Example 2: Using with dplyr chain for larger datasets
#' sample_data %>%
#'   add_full_state_names() %>%
#'   dplyr::filter(state_code != "California")
#'
#' # Example 3: Handling mixed valid and invalid state codes
#' sample_data <- tibble::tibble(state_code = c("CO", "XX", "CA"))
#' add_full_state_names(sample_data)
#' @noRd
#' @importFrom exploratory statecode
#' @importFrom dplyr mutate
add_full_state_names <- function(tbl) {
  logger::log_info("Converting state_code to full state names.")
  tbl %>%
    dplyr::mutate(state_code = exploratory::statecode(state_code, output_type = "name"))
}

#' Filter out entries with missing phone, first name, or last name and exclude Guam
#' @param tbl A tibble with `phone_number`, `first`, `last`, and `state_code` columns.
#' @return A tibble filtered for valid entries.
#' @examples
#' # Example 1: Filter out missing values in sample tibble
#' sample_data <- tibble::tibble(phone_number = c("123", NA), first = c("John", "Jane"), last = c("Doe", NA), state_code = c("CO", "Guam"))
#' filter_valid_entries(sample_data)
#'
#' # Example 2: Filtering larger dataset with dplyr chaining
#' sample_data %>%
#'   filter_valid_entries() %>%
#'   dplyr::arrange(state_code)
#'
#' # Example 3: Using within custom data wrangling functions
#' filtered_data <- filter_valid_entries(taxonomy_and_aaos_tbl)
#' @noRd
#' @importFrom dplyr filter
filter_valid_entries <- function(tbl) {
  logger::log_info("Filtering records with missing phone numbers, first or last names, and excluding Guam.")
  tbl %>%
    dplyr::filter(!is.na(phone_number), !is.na(first), !is.na(last), state_code != "Guam")
}

#' Convert state codes to lowercase for joining
#' @param tbl A tibble with `state_code` column.
#' @return A tibble with `state_code` in lowercase.
#' @examples
#' # Example 1: Converting state codes in sample tibble
#' sample_data <- tibble::tibble(state_code = c("CO", "CA", "NY"))
#' lowercase_state_codes(sample_data)
#'
#' # Example 2: Use within a pipeline
#' sample_data %>%
#'   lowercase_state_codes() %>%
#'   dplyr::filter(state_code == "ca")
#'
#' # Example 3: Using as a standalone transformation
#' lowercased_data <- lowercase_state_codes(sample_data)
#' @noRd
#' @importFrom dplyr mutate
lowercase_state_codes <- function(tbl) {
  logger::log_info("Converting state_code to lowercase.")
  tbl %>%
    dplyr::mutate(state_code = tolower(state_code))
}

#' Join with Census Bureau data to add region and division information
#' @param tbl A tibble with `state_code` column in lowercase.
#' @param census_bureau_tbl A tibble with `State` column for joining.
#' @return A tibble with Census region and division information.
#' @examples
#' # Example 1: Join with Census Bureau data in a pipeline
#' sample_data <- tibble::tibble(state_code = c("colorado", "california"))
#' join_census_regions(sample_data, census_bureau_tbl)
#'
#' # Example 2: Using with dplyr pipeline
#' sample_data %>%
#'   lowercase_state_codes() %>%
#'   join_census_regions(census_bureau_tbl)
#'
#' # Example 3: Test with partial census data for specific regions
#' join_census_regions(sample_data, dplyr::filter(census_bureau_tbl, Region == "West"))
#' @noRd
#' @importFrom dplyr left_join
join_census_regions <- function(tbl, census_bureau_tbl) {
  logger::log_info("Joining with Census Bureau data for region and division information.")
  tbl %>%
    dplyr::left_join(census_bureau_tbl, by = c("state_code" = "State"))
}

#' Sample 14 records per division with a seed for reproducibility
#' @param tbl A tibble grouped by `Division`.
#' @return A tibble with 14 sampled records per division.
#' @examples
#' # Example 1: Sample by division on grouped dataset
#' sampled_data <- sample_by_division(grouped_tbl)
#'
#' # Example 2: Use within pipeline on original dataset
#' taxonomy_and_aaos_tbl %>%
#'   join_census_regions(census_bureau_tbl) %>%
#'   sample_by_division()
#'
#' # Example 3: Test sampling with varied seed
#' sampled_data <- sample_by_division(taxonomy_and_aaos_tbl, seed = 2023)
#' @noRd
#' @importFrom exploratory sample_rows
#' @importFrom dplyr group_by mutate row_number bind_rows arrange
sample_by_division <- function(tbl) {
  logger::log_info("Sampling 14 records per Division for reproducibility.")
  tbl %>%
    dplyr::group_by(Division) %>%
    exploratory::sample_rows(14, seed = 1978) %>%
    dplyr::mutate(temp_id = dplyr::row_number()) %>%
    dplyr::bind_rows(.) %>%
    dplyr::arrange(npi)
}

#' Assign alternating insurance types to sampled rows
#' @param tbl A tibble with sampled rows.
#' @return A tibble with alternating insurance types.
#' @examples
#' # Example 1: Assign insurance types on duplicated sample tibble
#' assign_alternating_insurance(sampled_tbl)
#'
#' # Example 2: Applying on tibble in pipeline
#' sample_by_division(grouped_tbl) %>%
#'   assign_alternating_insurance()
#'
#' # Example 3: Using directly after row duplication
#' dplyr::bind_rows(sampled_tbl, sampled_tbl) %>%
#'   assign_alternating_insurance()
#' @noRd
#' @importFrom dplyr mutate
assign_alternating_insurance <- function(tbl) {
  logger::log_info("Assigning alternating insurance values to each row.")
  tbl %>%
    dplyr::mutate(Insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = n()))
}

#' Arrange final columns and remove temporary ID
#' @param tbl A tibble with columns to arrange by `state_code` and `npi`.
#' @return A tibble arranged with temporary ID column removed.
#' @examples
#' # Example 1: Arranging after insurance assignment
#' arrange_final_columns(tibble_with_insurance)
#'
#' # Example 2: Using after all transformations
#' assign_alternating_insurance(sampled_tbl) %>%
#'   arrange_final_columns()
#'
#' # Example 3: Final cleanup in a pipeline
#' tibble_with_temp_id %>%
#'   arrange_final_columns()
#' @noRd
#' @importFrom dplyr arrange select
arrange_final_columns <- function(tbl) {
  logger::log_info("Arranging by state_code and NPI, and removing temporary ID column.")
  tbl %>%
    dplyr::arrange(state_code, npi) %>%
    dplyr::select(-temp_id)
}
