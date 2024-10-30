#' Plot Regional Physician Counts by Specified Grouping
#'
#' This function creates a choropleth map of physician counts by region, with options for different regional groupings.
#' It merges state counts with regional data, calculates centroids for text labels, and generates a plot using ggplot2.
#' Extensive logging is provided to the console for each major step, including data transformations, plot generation, and file saving.
#'
#' @param state_counts A dataframe containing state-level physician counts. Must have columns `state_code` and `total_available`.
#' @param region_df A dataframe with columns `State` and a region grouping column. Each row maps a state to its region.
#' @param region_col A string specifying the name of the column in `region_df` used to group the states by region.
#' @param save_path An optional string specifying the file path to save the generated plot. If `NULL`, the plot displays onscreen only.
#'
#' @return A ggplot object representing the choropleth map.
#'
#' @examples
#' # Example 1: Plot region counts for ENT_BOG regions, saving output to a file
#' ent_bog_regions <- data.frame(
#'   State = c("Alabama", "Alaska", "Arizona"),
#'   ENT_BOG_Region = c("Region 4", "Region 9", "Region 9")
#' )
#' state_counts <- data.frame(
#'   state_code = c("alabama", "alaska", "arizona"),
#'   total_available = c(10, 15, 8)
#' )
#' plot_region_counts(
#'   state_counts = state_counts,
#'   region_df = ent_bog_regions,
#'   region_col = "ENT_BOG_Region",
#'   save_path = "ent_bog_region_map.png"
#' )
#'
#' # Example 2: Plot without saving, grouping by ACOG districts
#' acog_districts_df <- data.frame(
#'   State = c("California", "Nevada", "Utah"),
#'   ACOG_District = c("District IX", "District IX", "District VIII")
#' )
#' state_counts <- data.frame(
#'   state_code = c("california", "nevada", "utah"),
#'   total_available = c(20, 12, 5)
#' )
#' plot_region_counts(
#'   state_counts = state_counts,
#'   region_df = acog_districts_df,
#'   region_col = "ACOG_District"
#' )
#'
#' # Example 3: Grouping by custom region data, saving output to file
#' custom_regions <- data.frame(
#'   State = c("Texas", "New York", "Florida"),
#'   Custom_Region = c("South", "Northeast", "South")
#' )
#' state_counts <- data.frame(
#'   state_code = c("texas", "new york", "florida"),
#'   total_available = c(25, 17, 13)
#' )
#' plot_region_counts(
#'   state_counts = state_counts,
#'   region_df = custom_regions,
#'   region_col = "Custom_Region",
#'   save_path = "custom_region_map.png"
#' )
#'
#' @importFrom dplyr mutate left_join group_by summarise arrange
#' @importFrom ggplot2 aes geom_polygon theme_void labs geom_text scale_fill_viridis_d scale_color_identity
#' @importFrom viridis scale_fill_viridis
#' @import maps
#' @importFrom logger log_info
plot_region_counts <- function(state_counts, region_df, region_col, save_path = NULL) {

  conflicted::conflicts_prefer(base::setdiff)

  # Log the inputs to the function
  logger::log_info("Function `plot_region_counts` called with inputs:")
  logger::log_info("state_counts dataframe with columns: {paste(colnames(state_counts), collapse = ', ')}")
  logger::log_info("region_df dataframe with columns: {paste(colnames(region_df), collapse = ', ')}")
  logger::log_info("Region column for grouping: {region_col}")
  if (!is.null(save_path)) {
    logger::log_info("Save path specified: {save_path}")
  } else {
    logger::log_info("No save path specified; plot will only display on screen.")
  }

  # Ensure the state codes are in lowercase to match the region data
  logger::log_info("Transforming state codes in `state_counts` and `region_df` to lowercase.")
  state_counts <- state_counts %>%
    dplyr::mutate(state_code = tolower(state_code))

  region_df <- region_df %>%
    dplyr::mutate(State = tolower(State))

  # Merge state counts with regions data based on specified region column
  logger::log_info("Merging `state_counts` with `region_df` based on the State and `{region_col}` columns.")
  state_counts_with_regions <- state_counts %>%
    dplyr::left_join(region_df, by = c("state_code" = "State")) %>%
    dplyr::group_by(.data[[region_col]]) %>%
    dplyr::summarise(total_available = sum(total_available, na.rm = TRUE))
  logger::log_info("Completed merge and aggregation. Resulting `state_counts_with_regions` has {nrow(state_counts_with_regions)} rows.")

  # Get map data for US states and convert state names to lowercase
  logger::log_info("Retrieving US state map data and converting region names to lowercase.")
  us_states <- ggplot2::map_data("state") %>%
    dplyr::mutate(region = tolower(region))

  # Merge the state map data with regions
  logger::log_info("Merging state map data with `region_df` and `state_counts_with_regions`.")
  map_data_with_regions <- us_states %>%
    dplyr::left_join(region_df, by = c("region" = "State")) %>%
    dplyr::left_join(state_counts_with_regions, by = region_col)
  logger::log_info("Map data merged. `map_data_with_regions` now has {nrow(map_data_with_regions)} rows.")

  # Calculate the centroids for each region
  logger::log_info("Calculating centroids for each region to position labels.")
  region_centroids <- map_data_with_regions %>%
    dplyr::group_by(.data[[region_col]]) %>%
    dplyr::summarise(
      long = mean(long, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      total_available = mean(total_available, na.rm = TRUE)
    )
  logger::log_info("Centroids calculated for each region. `region_centroids` has {nrow(region_centroids)} rows.")

  # Plot the choropleth map with region_col as fill
  logger::log_info("Creating the choropleth map with regions as fill and centroid labels.")
  region_plot <- ggplot2::ggplot(map_data_with_regions, ggplot2::aes(long, lat, group = group, fill = .data[[region_col]])) +
    ggplot2::geom_polygon(color = "gray30", size = 0.2) +
    viridis::scale_fill_viridis_d(option = "C", name = region_col) +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = paste("Choropleth Map of Physicians Available by", region_col),
      subtitle = "Aggregated Data for Spine Surgeons as of 2024"
    ) +
    ggplot2::geom_text(data = region_centroids, ggplot2::aes(x = long, y = lat, label = round(total_available, 0),
                                                             color = ifelse(total_available > 50, "white", "black")),
                       size = 4, fontface = "bold", inherit.aes = FALSE) +
    ggplot2::scale_color_identity()  # Ensures the color mapping works without legends for text colors
  logger::log_info("Plot creation complete.")

  # Save plot if a path is provided
  if (!is.null(save_path)) {
    logger::log_info("Saving the plot to specified path: {save_path}")
    ggplot2::ggsave(save_path, region_plot, width = 10, height = 7)
    logger::log_info("Plot successfully saved at {save_path}")
  } else {
    logger::log_info("No save path provided, displaying plot on screen.")
  }

  # Display the plot
  print(region_plot)

  # Log output of function
  logger::log_info("Function `plot_region_counts` completed execution.")
  logger::log_info("Outputs: Displayed or saved choropleth map with regional physician counts.")
}

# # Assuming state_counts looks like this:
# state_counts <- data.frame(
#   state_code = c("connecticut", "maine", "massachusetts", "new york", "california", "texas", "florida", "idaho"),
#   total_available = c(20, 15, 10, 30, 40, 35, 25, 5)
# )
# # Example 1: Plot with ENT Board of Governors (ENT_BOG) Regions
# # ent_bog_regions contains columns "State" and "ENT_BOG_Region"
# plot_region_counts(
#   state_counts = state_counts,
#   region_df = ent_bog_regions,
#   region_col = "ENT_BOG_Region",
#   state_col = "state_code",
#   save_path = "ent_bog_region_map.png",
#   color_scale = "C"
# )
#
# # Example 2: Plot with ACOG Districts
# # acog_districts_df contains columns "State" and "ACOG_District"
# plot_region_counts(
#   state_counts = state_counts,
#   region_df = acog_districts_df,
#   region_col = "ACOG_District",
#   state_col = "state_code",
#   save_path = "acog_district_map.png",
#   color_scale = "D"
# )
#
# # Example 3: Plot with General U.S. Census Regions
# # regions_df contains columns "State" and "Region"
# plot_region_counts(
#   state_counts = state_counts,
#   region_df = regions_df,
#   region_col = "Region",
#   state_col = "state_code",
#   save_path = "us_census_region_map.png",
#   color_scale = "B"
# )
