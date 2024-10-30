#' Validate the inputs for create_region_map function
#' @noRd
validate_inputs <- function(remove_ak_hi, districts_per_group, alpha_level, save_path) {
  if (!is.logical(remove_ak_hi)) {
    logger::log_error("Invalid 'remove_ak_hi': Must be a logical value.")
    stop("'remove_ak_hi' must be a logical value (TRUE or FALSE).")
  }

  if (!districts_per_group %in% c("acog_districts", "ENT_Board_of_Governors_Regions", "US_Census_Subdivisions")) {
    logger::log_error("Invalid 'districts_per_group': Must be one of 'acog_districts', 'ENT_Board_of_Governors_Regions', or 'US_Census_Subdivisions'.")
    stop("Invalid 'districts_per_group': Must be one of 'acog_districts', 'ENT_Board_of_Governors_Regions', or 'US_Census_Subdivisions'.")
  }

  if (!is.numeric(alpha_level) || alpha_level < 0 || alpha_level > 1) {
    logger::log_error("Invalid 'alpha_level': Must be a numeric value between 0 and 1.")
    stop("'alpha_level' must be a numeric value between 0 and 1.")
  }

  if (!is.null(save_path) && !is.character(save_path)) {
    logger::log_error("Invalid 'save_path': Must be a character string.")
    stop("'save_path' must be a character string.")
  }

  logger::log_success("Input validation passed.")
}

#' Load U.S. states shapefile data
#' @noRd
load_states_shapefile <- function() {
  logger::log_info("Loading U.S. states shapefile data from rnaturalearth...")
  states_sf <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  logger::log_success("U.S. states data loaded successfully.")
  return(states_sf)
}

#' Join shapefile with regions/divisions or ACOG districts
#' @noRd
join_with_acog_or_region <- function(states_sf, districts_per_group) {
  if (districts_per_group == "acog_districts") {
    logger::log_info("Joining ACOG District data with U.S. states shapefile...")
    joined_states <- dplyr::left_join(states_sf, acog_districts_df, by = c("name" = "State"))
    fill_column <- "ACOG_District"
    logger::log_success("Joined with ACOG Districts.")
  } else if (districts_per_group == "ENT_Board_of_Governors_Regions") {
    logger::log_info("Joining ENT Board of Governors Regions data with U.S. states shapefile...")
    joined_states <- dplyr::left_join(states_sf, ent_bog_regions, by = c("name" = "State"))
    fill_column <- "ENT_BOG_Region"
    logger::log_success("Joined with ENT Board of Governors Regions.")
  } else {
    logger::log_info("Joining US Census Subdivisions with U.S. states shapefile...")
    joined_states <- dplyr::left_join(states_sf, regions_df, by = c("name" = "State"))
    fill_column <- "Division"
    logger::log_success("Joined with US Census Subdivisions.")
  }

  return(list(joined_states = joined_states, fill_column = fill_column))
}

#' Remove Alaska and Hawaii if required
#' @noRd
remove_alaska_hawaii <- function(joined_states, remove_ak_hi) {
  if (remove_ak_hi) {
    logger::log_info("Removing Alaska and Hawaii from the dataset...")
    filtered_states <- dplyr::filter(joined_states, !postal %in% c("AK", "HI"))
    logger::log_success("Alaska and Hawaii removed.")
    return(filtered_states)
  }
  return(joined_states)
}

#' Generate the color palette for the map
#' @noRd
generate_color_palette <- function(states_data, fill_column) {
  unique_fill_values <- dplyr::pull(states_data, !!rlang::sym(fill_column)) %>%
    unique() %>%
    na.omit()
  num_fill_values <- length(unique_fill_values)

  logger::log_info("Generating a color palette for {num_fill_values} unique values...")
  color_palette <- viridis::viridis(num_fill_values, option = "D", direction = 1)
  logger::log_success("Color palette generated.")

  return(color_palette)
}

#' Create the map using ggplot2
#' @noRd
create_map_plot <- function(states_data, fill_column, color_palette, alpha_level, title, subtitle, remove_ak_hi) {
  logger::log_info("Creating the map using ggplot2...")
  map_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = states_data, ggplot2::aes_string(fill = fill_column), color = "black", alpha = alpha_level) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      subtitle = ifelse(is.null(subtitle), ifelse(remove_ak_hi, "(Excluding Alaska and Hawaii)", "(Including Alaska and Hawaii)"), subtitle),
      fill = fill_column
    ) +
    ggplot2::scale_fill_manual(values = setNames(color_palette, unique(states_data[[fill_column]])), na.value = "grey80")
  logger::log_success("Map created successfully.")

  return(map_plot)
}

#' Save the map to a file if a path is provided
#' @noRd
save_map_to_file <- function(map_plot, save_path) {
  if (!is.null(save_path)) {
    logger::log_info("Saving the map to {save_path}...")
    ggplot2::ggsave(filename = save_path, plot = map_plot, width = 10, height = 6, dpi = 300)
    logger::log_success("Map saved to {save_path}.")
  }
}

#' Main function to create a map of U.S. States by Region, Division, or Custom Districts
#'
#' This function generates a map of U.S. states, coloring them by regions, divisions, ACOG districts, or ENT Board of Governors Regions.
#' Users can customize which grouping to use (ACOG Districts, ENT_Board_of_Governors_Regions, or US Census Subdivisions).
#'
#' @param remove_ak_hi Logical, whether to exclude Alaska and Hawaii from the map. Default is TRUE.
#' @param districts_per_group Character, which grouping to use for the regions. Options are: "acog_districts", "ENT_Board_of_Governors_Regions", or "US_Census_Subdivisions".
#' @param save_path Character, an optional file path to save the map image. If NULL, the map will not be saved. Default is NULL.
#' @param alpha_level Numeric, the transparency level for the map's fill color, with 0 being fully transparent and 1 fully opaque. Default is 0.2.
#' @param title Character, the title for the map. Default is "U.S. States by Region/Division or Custom Districts".
#' @param subtitle Character, an optional subtitle for the map.
#'
#' @return A ggplot map object representing the U.S. states colored by region/division, ACOG districts, or ENT_Board_of_Governors_Regions.
#'
#' @examples
#' # Example 1: Create a map of U.S. states by region/division, excluding Alaska and Hawaii
#' create_region_map(remove_ak_hi = TRUE, districts_per_group = "US_Census_Subdivisions", alpha_level = 0.2)
#'
#' # Example 2: Create a map of U.S. states by ACOG districts, including Alaska and Hawaii
#' create_region_map(remove_ak_hi = FALSE, districts_per_group = "acog_districts", alpha_level = 0.5)
#'
#' # Example 3: Create a map of U.S. states by ENT Board of Governors Regions and save it to a file
#' create_region_map(remove_ak_hi = TRUE, districts_per_group = "ENT_Board_of_Governors_Regions", save_path = "ent_bog_regions_map.png", alpha_level = 0.3)
#'
#' @importFrom sf st_sf st_read st_transform st_union
#' @importFrom ggplot2 ggplot geom_sf aes_string theme_minimal labs scale_fill_manual ggsave
#' @importFrom dplyr left_join filter pull mutate
#' @importFrom rnaturalearth ne_states
#' @importFrom viridis viridis
#' @importFrom logger log_info log_success log_error
#' @export
create_region_map <- function(remove_ak_hi = TRUE, districts_per_group = "acog_districts", save_path = NULL, alpha_level = 0.4, title = "U.S. States by Region/Division or Custom Districts", subtitle = NULL) {
  # Step 1: Validate Inputs
  validate_inputs(remove_ak_hi, districts_per_group, alpha_level, save_path)

  # Step 2: Load shapefile data
  states_sf <- load_states_shapefile()

  # Step 3: Join with regions/divisions or ACOG districts
  join_result <- join_with_acog_or_region(states_sf, districts_per_group)
  states_data <- join_result$joined_states
  fill_column <- join_result$fill_column

  # Step 4: Remove Alaska and Hawaii if necessary
  states_data <- remove_alaska_hawaii(states_data, remove_ak_hi)

  # Step 5: Generate color palette
  color_palette <- generate_color_palette(states_data, fill_column)

  # Step 6: Create the map
  map_plot <- create_map_plot(states_data, fill_column, color_palette, alpha_level, title, subtitle, remove_ak_hi)

  # Step 7: Save the map if a file path is provided
  save_map_to_file(map_plot, save_path)

  # Step 8: Return the map
  logger::log_info("Returning the map object.")
  return(map_plot)
}

# Example of calling the function
# create_region_map(remove_ak_hi = TRUE, districts_per_group = "ENT_Board_of_Governors_Regions", save_path = NULL, alpha_level = 0.4, title = "U.S. States by Region/Division or Custom Districts", subtitle = NULL)
