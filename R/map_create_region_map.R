#' Create a Map of U.S. States by Region, Division, or Custom Districts
#'
#' This function generates a map of U.S. states, coloring them by regions, divisions, ACOG districts, or
#' ENT Board of Governors Regions. Users can customize which grouping to use (ACOG Districts, ENT_Board_of_Governors_Regions, or US Census Subdivisions).
#'
#' @param remove_ak_hi Logical. Whether to exclude Alaska and Hawaii from the map. Default is TRUE.
#' @param districts_per_group Character. Which grouping to use for the regions. Options are:
#'   - "acog_districts"
#'   - "ENT_Board_of_Governors_Regions"
#'   - "US_Census_Subdivisions"
#' @param save_path Character. An optional file path to save the map image. If NULL, the map will not be saved. Default is NULL.
#' @param alpha_level Numeric. The transparency level for the map's fill color, with 0 being fully transparent and 1 fully opaque. Default is 0.4.
#' @param title Character. The title for the map. Default is "U.S. States by Region/Division or Custom Districts".
#' @param subtitle Character. An optional subtitle for the map. Default is NULL.
#'
#' @return A ggplot object representing the U.S. states colored by region/division, ACOG districts, or ENT_Board_of_Governors_Regions.
#'
#' @examples
#' # Example 1: Create a map of U.S. states by ACOG districts
#' \dontrun{
#' map_acog <- create_region_map(
#'   remove_ak_hi = TRUE,
#'   districts_per_group = "acog_districts",
#'   save_path = "acog_district_map.png",
#'   alpha_level = 0.4,
#'   title = "Map of U.S. States by ACOG Districts",
#'   subtitle = "Excluding Alaska and Hawaii"
#' )
#' print(map_acog)
#' }
#'
#' # Example 2: Create a map of U.S. states by ENT Board of Governors Regions
#' \dontrun{
#' map_ent <- create_region_map(
#'   remove_ak_hi = FALSE,
#'   districts_per_group = "ENT_Board_of_Governors_Regions",
#'   alpha_level = 0.5,
#'   title = "Map of ENT Board of Governors Regions",
#'   subtitle = "Including Alaska and Hawaii"
#' )
#' print(map_ent)
#' }
#'
#' # Example 3: Create a map of U.S. states by Census Subdivisions without saving
#' \dontrun{
#' map_census <- create_region_map(
#'   remove_ak_hi = TRUE,
#'   districts_per_group = "US_Census_Subdivisions",
#'   save_path = NULL,
#'   alpha_level = 0.3,
#'   title = "Map of U.S. Census Subdivisions",
#'   subtitle = NULL
#' )
#' print(map_census)
#' }
#'
#' @importFrom sf st_sf st_read st_transform st_union
#' @importFrom ggplot2 ggplot geom_sf aes_string theme_minimal labs scale_fill_manual ggsave
#' @importFrom dplyr left_join filter pull mutate
#' @importFrom viridis viridis
#' @importFrom logger log_info log_success log_error
#' @importFrom assertthat assert_that is.string
#' @export
create_region_map <- function(remove_ak_hi = TRUE,
                              districts_per_group = "acog_districts",
                              save_path = NULL,
                              alpha_level = 0.4,
                              title = "U.S. States by Region/Division or Custom Districts",
                              subtitle = NULL) {
  # Validate Inputs
  assertthat::assert_that(is.logical(remove_ak_hi), msg = "`remove_ak_hi` must be a logical value (TRUE or FALSE).")
  assertthat::assert_that(
    districts_per_group %in% c("acog_districts", "ENT_Board_of_Governors_Regions", "US_Census_Subdivisions"),
    msg = "`districts_per_group` must be one of 'acog_districts', 'ENT_Board_of_Governors_Regions', or 'US_Census_Subdivisions'."
  )
  assertthat::assert_that(is.numeric(alpha_level) && alpha_level >= 0 && alpha_level <= 1,
    msg = "`alpha_level` must be a numeric value between 0 and 1."
  )
  if (!is.null(save_path)) {
    assertthat::assert_that(assertthat::is.string(save_path), msg = "`save_path` must be a character string.")
  }

  logger::log_info("Starting create_region_map with inputs:")
  logger::log_info(paste("  remove_ak_hi:", remove_ak_hi))
  logger::log_info(paste("  districts_per_group:", districts_per_group))
  logger::log_info(paste("  alpha_level:", alpha_level))
  logger::log_info(paste("  save_path:", save_path))

  # Step 1: Load shapefile data
  states_sf <- load_states_shapefile()

  # Step 2: Join with regions/divisions or ACOG districts
  join_result <- join_with_acog_or_region(states_sf, districts_per_group)
  states_data <- join_result$joined_states
  fill_column <- join_result$fill_column

  # Step 3: Remove Alaska and Hawaii if necessary
  states_data <- remove_alaska_hawaii(states_data, remove_ak_hi)

  # Step 4: Generate color palette
  color_palette <- generate_color_palette(states_data, fill_column)

  # Step 5: Create the map
  map_plot <- create_map_plot(
    states_data,
    fill_column,
    color_palette,
    alpha_level,
    title,
    subtitle,
    remove_ak_hi
  )

  # Step 6: Save the map if a file path is provided
  save_map_to_file(map_plot, save_path)

  logger::log_success("Map creation complete.")
  return(map_plot)
}
