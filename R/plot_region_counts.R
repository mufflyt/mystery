#' Plot Regional Physician Counts by Specified Grouping
#'
#' This function creates a choropleth map visualizing physician counts by region, with options for grouping states into custom regions. It handles merging state counts with regional data, calculating centroids for text labels, and generating plots using ggplot2. Logging is included to provide insights into the execution flow and data transformations.
#'
#' @param state_counts A dataframe containing state-level physician counts. Must have columns `state_code` (lowercase state names) and `total_available` (numerical counts).
#' @param region_df A dataframe mapping states to regions. Must include `State` (state names in lowercase) and a column for the region grouping.
#' @param region_col A string specifying the column in `region_df` to use for grouping states into regions (e.g., "Region").
#' @param save_path Optional. A string specifying the file path to save the plot as an image. If `NULL` (default), the plot is displayed onscreen but not saved.
#'
#' @return A ggplot object representing the choropleth map.
#'
#' @details
#' The function merges `state_counts` and `region_df` by state name, aggregates physician counts by region, and maps these counts onto a choropleth. Labels representing the counts are added at region centroids.
#'
#' @examples
#' # Example 1: Plot and save a map grouped by ENT_BOG regions
#' ent_bog_regions <- data.frame(
#'   State = c("alabama", "alaska", "arizona"),
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
#' # Example 2: Plot ACOG Districts without saving
#' ACOG_Districts_sf <- data.frame(
#'   State = c("california", "nevada", "utah"),
#'   ACOG_District = c("District IX", "District IX", "District VIII")
#' )
#' state_counts <- data.frame(
#'   state_code = c("california", "nevada", "utah"),
#'   total_available = c(20, 12, 5)
#' )
#' plot_region_counts(
#'   state_counts = state_counts,
#'   region_df = ACOG_Districts_sf,
#'   region_col = "ACOG_District"
#' )
#'
#' # Example 3: Plot custom regions and save output
#' custom_regions <- data.frame(
#'   State = c("texas", "new york", "florida"),
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
#' @importFrom dplyr mutate left_join group_by summarise
#' @importFrom ggplot2 aes geom_polygon theme_void labs geom_text scale_fill_manual scale_color_identity
#' @importFrom viridis scale_fill_viridis
#' @importFrom logger log_info
#' @export
plot_region_counts <- function(state_counts, region_df, region_col, save_path = NULL) {

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

  # Ensure the region_col is a factor
  map_data_with_regions <- map_data_with_regions %>%
    dplyr::mutate({{ region_col }} := as.factor(.data[[region_col]]))

  # Create the choropleth map
  region_plot <- ggplot2::ggplot(map_data_with_regions, ggplot2::aes(long, lat, group = group, fill = .data[[region_col]])) +
    ggplot2::geom_polygon(color = "gray30", size = 0.2) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "C", name = region_col) +  # Corrected viridis usage
    ggplot2::theme_void() +
    ggplot2::labs(
      title = paste("Choropleth Map of Physicians Available by", region_col),
      subtitle = "Aggregated Data for Spine Surgeons as of 2024"
    ) +
    ggplot2::geom_text(
      data = region_centroids,
      ggplot2::aes(
        x = long, y = lat, label = round(total_available, 0),
        color = ifelse(total_available > 50, "white", "black")
      ),
      size = 4, fontface = "bold", inherit.aes = FALSE
    ) +
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

  logger::log_info("Function `plot_region_counts` completed execution.")
  logger::log_info("Outputs: Displayed or saved choropleth map with regional physician counts.")
}
