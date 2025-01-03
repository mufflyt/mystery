#' Generate Hexagon Maps by ACOG District with Logging and Error Handling
#'
#' This function generates hexagon maps for ACOG districts, using physician location data.
#' It provides extensive logging, error handling, and defaults to sample data if no inputs are provided.
#'
#' @param physician_locations_sf A Simple Features (sf) object containing physician data with coordinates.
#'   If NULL, default sample data is used.
#' @param acog_districts_sf A Simple Features (sf) object containing the grouped ACOG districts.
#'   If NULL, default ACOG districts are used.
#' @param trait_map A string specifying the trait map (default is "all").
#' @param honeycomb_map A string specifying the honey map (default is "all").
#' @param hex_grid_size A numeric vector of length 2 specifying the grid size for the hexagon map
#'   (default is c(0.3, 0.3)).
#' @param target_district A string or NULL to specify a specific district for generating the map
#'   (default is NULL, which processes all districts).
#'
#' @return A list of ggplot objects of the generated maps for the specified or all districts.
#'   Each ggplot object is a hexagon map showing physician distribution.
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr filter mutate group_by summarize row_number
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_c theme_minimal
#' @importFrom sf st_transform st_make_grid st_intersection st_sf st_join
#' @importFrom logger log_info log_debug log_error
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' # Generate hexagon maps with default inputs
#' maps <- honeycomb_generate_maps()
#' print(maps[[1]])
#'
#' # Generate hexagon maps for a specific ACOG district
#' maps <- honeycomb_generate_maps(target_district = "District I")
#' print(maps[[1]])
#'
#' # Customize hex grid size and process all districts
#' maps <- honeycomb_generate_maps(hex_grid_size = c(0.5, 0.5))
#' print(maps[[1]])
#' }
#'
#' @export
honeycomb_generate_maps <- function(
    physician_locations_sf = NULL,
    acog_districts_sf = NULL,
    trait_map = "all",
    honeycomb_map = "all",
    hex_grid_size = c(0.3, 0.3),
    target_district = NULL) {
  # Validate inputs
  assertthat::assert_that(
    is.null(physician_locations_sf) || inherits(physician_locations_sf, "sf"),
    msg = "physician_locations_sf must be an sf object or NULL."
  )
  assertthat::assert_that(
    is.null(acog_districts_sf) || inherits(acog_districts_sf, "sf"),
    msg = "acog_districts_sf must be an sf object or NULL."
  )
  assertthat::assert_that(
    assertthat::is.string(trait_map),
    msg = "trait_map must be a character string."
  )
  assertthat::assert_that(
    assertthat::is.string(honeycomb_map),
    msg = "honeycomb_map must be a character string."
  )
  assertthat::assert_that(
    is.numeric(hex_grid_size) && length(hex_grid_size) == 2,
    msg = "hex_grid_size must be a numeric vector of length 2."
  )
  assertthat::assert_that(
    is.null(target_district) || assertthat::is.string(target_district),
    msg = "target_district must be a character string or NULL."
  )

  # Log the inputs
  logger::log_info("Function inputs logged.")
  logger::log_debug("Trait map: {trait_map}")
  logger::log_debug("Honeycomb map: {honeycomb_map}")
  logger::log_debug("Hex grid size: {paste(hex_grid_size, collapse = ', ')}")
  logger::log_debug("Target district: {target_district}")

  # Default to dummy physician and district data if none is provided
  if (is.null(physician_locations_sf)) {
    logger::log_info("No physician data provided, using default sample data.")
    physician_locations_sf <- tyler::physicians() # Assuming sample function available
  }

  if (is.null(acog_districts_sf)) {
    logger::log_info("No ACOG districts data provided, using default ACOG districts from package.")
    acog_districts_sf <- tyler::ACOG_Districts_sf() # Assuming default districts available
  }

  # Determine which districts to process
  districts_to_process <- if (!is.null(target_district)) {
    logger::log_info("Processing specific district: {target_district}")
    target_district
  } else {
    unique(acog_districts_sf$ACOG_District)
  }

  logger::log_info("Processing the following districts: {paste(districts_to_process, collapse = ', ')}")

  # Generate hexagon maps
  maps_list <- purrr::map(districts_to_process, function(single_district) {
    logger::log_info("Processing district: {single_district}")

    # Filter the ACOG district
    district_sf <- dplyr::filter(acog_districts_sf, ACOG_District == single_district)

    # Create honeycomb grid and intersect with physician data
    honeycomb_grid <- sf::st_make_grid(district_sf, cellsize = hex_grid_size, what = "polygons", square = FALSE) %>%
      sf::st_sf() %>%
      dplyr::mutate(grid_id = dplyr::row_number()) %>%
      sf::st_intersection(district_sf)

    intersections <- sf::st_intersection(honeycomb_grid, physician_locations_sf)
    physicians_in_grid <- intersections %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarize(physician_count = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(physician_count > 0)

    # Join honeycomb grid with physician count
    grid_with_physician_counts <- sf::st_join(honeycomb_grid, physicians_in_grid) %>%
      dplyr::filter(!is.na(physician_count))

    # Create the map
    hex_map_plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = district_sf, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
      ggplot2::geom_sf(data = grid_with_physician_counts, ggplot2::aes(fill = physician_count), color = NA) +
      ggplot2::scale_fill_viridis_c(name = "Physician Count") +
      ggplot2::theme_minimal(base_size = 10)

    return(hex_map_plot)
  })

  # Log output
  logger::log_info("Hexagon maps generation complete. Returning a list of ggplot objects.")
  return(maps_list)
}
