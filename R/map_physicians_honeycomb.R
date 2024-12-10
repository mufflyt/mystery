#' Generate Hexagon Maps by ACOG District with Logging and Error Handling
#'
#' This function generates hexagon maps for ACOG districts, using physician location data.
#' It provides extensive logging, error handling, and defaults to sample data if no inputs are provided.
#'
#' @param physician_locations_sf A Simple Features (sf) object containing physician data with coordinates.
#' If NULL, default sample data is used.
#' @param acog_districts_sf A Simple Features (sf) object containing the grouped ACOG districts.
#' If NULL, default ACOG districts are used.
#' @param trait_map A string specifying the trait map (default is "all").
#' @param honeycomb_map A string specifying the honey map (default is "all").
#' @param hex_grid_size A numeric vector of length 2 specifying the grid size for the hexagon map
#' (default is c(0.3, 0.3)).
#' @param target_district A string or NULL to specify a specific district for generating the map
#' (default is NULL, which processes all districts).
#'
#' @return A list of ggplot objects of the generated maps for the specified or all districts.
#' Each ggplot object is a hexagon map showing physician distribution.
#'
#' @importFrom dplyr filter mutate group_by summarize n row_number
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_c theme_minimal theme labs ggsave
#' @importFrom logger log_info log_debug log_error
#' @importFrom sf st_transform st_make_grid st_intersection st_sf st_union st_join
#' @import tigris
#' @export
honeycomb_generate_maps <- function(
    physician_locations_sf = NULL,
    acog_districts_sf = NULL,
    trait_map = "all",
    honeycomb_map = "all",
    hex_grid_size = c(0.3, 0.3),
    target_district = NULL
) {
  # Check for ggspatial
  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    stop("The ggspatial package is required to generate maps but is not installed. Please install it using install.packages('ggspatial').")
  }

  # Check if rnaturalearth is available
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("The rnaturalearth package is required for this function. Please install it using install.packages('rnaturalearth').")
  }

  # Default to dummy physician and district data if none is provided
  if (is.null(physician_locations_sf)) {
    logger::log_info("No physician data provided, using default sample data.")
    physician_locations_sf <- tyler::physicians()  # Assuming sample function available
  }

  if (is.null(acog_districts_sf)) {
    logger::log_info("No ACOG districts data provided, using default ACOG districts from package.")
    acog_districts_sf <- tyler::ACOG_Districts_sf()  # Assuming default districts available
  }

  # Log inputs
  logger::log_info("Function inputs logged.")
  logger::log_debug("Number of physician locations: {nrow(physician_locations_sf)}")
  logger::log_debug("Number of ACOG districts: {nrow(acog_districts_sf)}")
  logger::log_debug("Trait map: {trait_map}")
  logger::log_debug("Honeycomb map: {honeycomb_map}")
  logger::log_debug("Hex grid size: {hex_grid_size}")
  logger::log_debug("Target district: {target_district}")

  # Error handling for input validation
  tryCatch({
    if (!inherits(physician_locations_sf, "sf")) {
      stop("physician_locations_sf must be an sf object.")
    }
    if (!inherits(acog_districts_sf, "sf")) {
      stop("acog_districts_sf must be an sf object.")
    }
    if (!is.character(trait_map) || !is.character(honeycomb_map)) {
      stop("trait_map and honeycomb_map must be strings.")
    }
    if (!is.numeric(hex_grid_size) || length(hex_grid_size) != 2) {
      stop("hex_grid_size must be a numeric vector of length 2.")
    }
  }, error = function(e) {
    logger::log_error("Error in input validation: {e$message}")
    stop(e)
  })

  # Set options
  options(tigris_use_cache = TRUE)
  sf::sf_use_s2(FALSE)

  # Load USA shapefile
  logger::log_info("Loading USA shapefile...")
  usa_map <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
  usa_map <- sf::st_transform(usa_map, 4326)
  logger::log_info("USA shapefile loaded and transformed.")

  # Determine which districts to process
  districts_to_process <- if (!is.null(target_district)) {
    logger::log_info("Processing specific district: {target_district}")
    target_district
  } else {
    unique(acog_districts_sf$ACOG_District)
  }
  logger::log_info("Processing the following districts: {paste(districts_to_process, collapse = ', ')}")

  # Loop through each ACOG district to generate hex maps
  logger::log_info("Generating hexagon maps...")
  maps_list <- purrr::map(districts_to_process, function(single_district) {
    logger::log_info("Processing district: {single_district}")

    # Filter the ACOG district
    district_sf <- dplyr::filter(acog_districts_sf, ACOG_District == single_district)
    logger::log_debug("Filtered district: {nrow(district_sf)} rows")

    # Create honeycomb grid and intersect with physician data
    honeycomb_grid <- sf::st_make_grid(district_sf, hex_grid_size, what = "polygons", square = FALSE) %>%
      sf::st_sf() %>%
      dplyr::mutate(grid_id = dplyr::row_number()) %>%
      sf::st_intersection(sf::st_transform(district_sf, 4326))
    logger::log_debug("Honeycomb grid created with {nrow(honeycomb_grid)} cells")

    intersections <- sf::st_intersection(honeycomb_grid, sf::st_transform(physician_locations_sf, 4326))
    physicians_in_grid <- intersections %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarize(physician_count = dplyr::n(), .groups = 'drop') %>%
      dplyr::filter(physician_count > 1)
    logger::log_debug("Physician counts per honeycomb grid calculated")

    # Join honeycomb grid with physician count
    grid_with_physician_counts <- sf::st_join(honeycomb_grid, physicians_in_grid) %>%
      dplyr::filter(!is.na(physician_count))
    logger::log_debug("Honeycomb grid joined with physician counts: {nrow(grid_with_physician_counts)} rows")

    # Create the map
    hex_map_plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = district_sf, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
      ggplot2::geom_sf(data = grid_with_physician_counts, ggplot2::aes(fill = physician_count), color = NA) +
      ggplot2::scale_fill_viridis_c(name = "Obstetrics and Gynecology Faculty Subspecialists") +
      ggplot2::theme_minimal(base_size = 10) +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
      ggspatial::annotation_north_arrow(location = "tr", which_north = "true", style = ggspatial::north_arrow_fancy_orienteering())

    # Save the map
    file_path <- paste0("figures/hexmap/hexmap_figures/", trait_map, "_", honeycomb_map, "_district_", single_district, "_honey.tiff")
    ggplot2::ggsave(filename = file_path, plot = hex_map_plot, width = 10, height = 6, dpi = 800)
    logger::log_info("Map saved: {file_path}")

    return(hex_map_plot)
  })

  # Log output
  logger::log_info("Hexagon maps generation complete. Returning a list of ggplot objects.")

  return(maps_list)
}
