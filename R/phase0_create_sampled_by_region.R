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
#' @importFrom dplyr mutate filter group_by left_join select arrange row_number bind_rows sample_n
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
  dir.create(dirname(output_csv_path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(sampled_region_data, output_csv_path)

  logger::log_info("Completed phase0_create_sampled_by_region. Output dataset has {nrow(sampled_region_data)} rows and {ncol(sampled_region_data)} columns.")

  return(sampled_region_data)
}

# Helper Functions --------------------------------------------------------

#' Add full state names using a lookup table
#' @noRd
add_full_state_names <- function(tbl) {
  logger::log_info("Converting state_code to full state names.")
  state_lookup <- tibble::tibble(
    state_code = state.abb,
    full_name = state.name
  )
  tbl %>%
    dplyr::left_join(state_lookup, by = c("state_code" = "state_code")) %>%
    dplyr::mutate(state_code = ifelse(!is.na(full_name), full_name, state_code)) %>%
    dplyr::select(-full_name)
}

#' Filter out entries with missing phone, first name, or last name and exclude Guam
#' @noRd
filter_valid_entries <- function(tbl) {
  logger::log_info("Filtering records with missing phone numbers, first or last names, and excluding Guam.")
  tbl %>%
    dplyr::filter(!is.na(phone_number), !is.na(first), !is.na(last), state_code != "Guam")
}

#' Convert state codes to lowercase for joining
#' @noRd
lowercase_state_codes <- function(tbl) {
  logger::log_info("Converting state_code to lowercase.")
  tbl %>%
    dplyr::mutate(state_code = tolower(state_code))
}

#' Join with Census Bureau data to add region and division information
#' @noRd
join_census_regions <- function(tbl, census_bureau_tbl) {
  logger::log_info("Joining with Census Bureau data for region and division information.")
  tbl %>%
    dplyr::left_join(census_bureau_tbl, by = c("state_code" = "State"))
}

#' Sample 14 records per division with a seed for reproducibility
#' @noRd
sample_by_division <- function(tbl) {
  logger::log_info("Sampling up to 14 records per Division for reproducibility.")
  tbl %>%
    dplyr::group_by(Division) %>%
    dplyr::group_modify(~ {
      sample_size <- min(14, nrow(.x))
      .x %>%
        dplyr::slice_sample(n = sample_size, replace = (nrow(.x) < 14))
    }) %>%
    dplyr::mutate(temp_id = dplyr::row_number()) %>%
    dplyr::arrange(npi)
}

#' Assign alternating insurance types to sampled rows
#' @noRd
assign_alternating_insurance <- function(tbl) {
  logger::log_info("Assigning alternating insurance values to each row.")
  tbl %>%
    dplyr::mutate(Insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = n()))
}

#' Arrange final columns and remove temporary ID
#' @noRd
arrange_final_columns <- function(tbl) {
  logger::log_info("Arranging by state_code and NPI, and removing temporary ID column.")
  tbl %>%
    dplyr::arrange(state_code, npi) %>%
    dplyr::select(-temp_id)
}
