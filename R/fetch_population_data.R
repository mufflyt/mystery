#' @title Fetch Population Data
#' @description Fetches population data for specified states, years, and race/ethnicity variables from the ACS.
#' The function validates inputs, logs progress at various stages, and ensures results are consistent and reliable.
#' @param state A character vector of state abbreviations (e.g., `c("CO", "CA")`) or a single state abbreviation.
#'   Default is `c("CO")`.
#' @param years A numeric vector specifying the years to fetch data for (e.g., `2015:2022`). Default is `2022`.
#' @param race_vars A named character vector mapping race/ethnicity names to variable codes (e.g.,
#'   `c("White" = "B02001_002E")`). Default includes common race categories from ACS.
#' @return A tibble with population data including columns:
#'   \describe{
#'     \item{id}{Unique identifier for each geographic area (GEOID).}
#'     \item{population}{Estimated population for the given year, state, and race/ethnicity.}
#'     \item{race_ethnicity}{Race/ethnicity category for the population estimate.}
#'     \item{geometry}{Geographic boundaries of the area (sf object).}
#'     \item{year}{Year for the population estimate.}
#'   }
#' @importFrom tidycensus get_acs
#' @importFrom dplyr mutate select recode
#' @importFrom purrr map_dfr
#' @importFrom logger log_info log_debug log_error log_warn
#' @importFrom sf st_geometry
#' @export
#' @examples
#' # Example 1: Default behavior (Colorado, year 2022, default race variables)
#' population_tibble <- fetch_population_data()
#' print(population_tibble)
#'
#' # Example 2: Fetch data for multiple states and years
#' race_vars <- c("White" = "B02001_002E", "Black" = "B02001_003E")
#' multi_state_population <- fetch_population_data(
#'   state = c("CO", "CA"), years = 2015:2020,
#'   race_vars = race_vars
#' )
#' print(multi_state_population)
#'
#' # Example 3: Use a custom set of race variables
#' custom_race_vars <- c("Asian" = "B02001_005E", "Hispanic" = "B03003_003E")
#' custom_population <- fetch_population_data(state = "TX", years = 2018, race_vars = custom_race_vars)
#' print(custom_population)
fetch_population_data <- function(state = c("CO"),
                                  years = 2022,
                                  race_vars = c(
                                    "White" = "B02001_002E",
                                    "Black" = "B02001_003E",
                                    "Asian" = "B02001_005E",
                                    "Hispanic" = "B03003_003E"
                                  )) {
  logger::log_info("Starting `fetch_population_data` function.")

  # Log inputs
  log_inputs(state, years, race_vars)

  # Validate inputs using assertthat
  assertthat::assert_that(
    is.character(state),
    msg = "`state` must be a character vector of state abbreviations."
  )
  assertthat::assert_that(
    assertthat::noNA(state),
    msg = "`state` contains NA values, which are not allowed."
  )
  assertthat::assert_that(
    is.numeric(years),
    msg = "`years` must be a numeric vector."
  )
  assertthat::assert_that(
    assertthat::noNA(years),
    msg = "`years` contains NA values, which are not allowed."
  )
  assertthat::assert_that(
    is.character(race_vars) && length(names(race_vars)) > 0,
    msg = "`race_vars` must be a named character vector."
  )
  assertthat::assert_that(
    assertthat::noNA(names(race_vars)),
    msg = "`race_vars` contains unnamed elements, which are not allowed."
  )

  # Fetch population data for all years
  logger::log_info("Fetching population data from ACS...")
  population_by_race_and_year <- purrr::map_dfr(years, function(year) {
    fetch_data_for_year(state, year, race_vars)
  })

  # Log outputs
  log_outputs(population_by_race_and_year)

  return(population_by_race_and_year)
}

# Helper Functions ------------------------------------------------------

#' @noRd
log_inputs <- function(state, years, race_vars) {
  logger::log_info("Inputs to the function:")
  logger::log_info("States: {paste(state, collapse = ', ')}")
  logger::log_info("Years: {paste(years, collapse = ', ')}")
  logger::log_info("Race/Ethnicity Variables: {paste(names(race_vars), collapse = ', ')}")
}

#' @noRd
fetch_data_for_year <- function(state, year, race_vars) {
  logger::log_info("Fetching data for year: {year}")
  tidycensus::get_acs(
    geography = "county",
    variables = race_vars,
    state = state,
    year = year,
    geometry = TRUE
  ) %>%
    dplyr::mutate(
      race_ethnicity = dplyr::recode(variable, !!!race_vars), # Map variable codes to race/ethnicity
      id = GEOID, # Use GEOID as a unique identifier
      population = estimate, # Use the estimate column for population
      year = year # Add the year column
    ) %>%
    dplyr::select(id, population, race_ethnicity, geometry, year) %>%
    {
      logger::log_debug("Fetched {nrow(.)} rows of data for year {year}.")
      .
    }
}

#' @noRd
log_outputs <- function(population_data) {
  logger::log_info("Successfully fetched population data.")
  logger::log_info("Output contains {nrow(population_data)} rows and {ncol(population_data)} columns.")
  logger::log_debug("Output columns: {paste(colnames(population_data), collapse = ', ')}")
}
