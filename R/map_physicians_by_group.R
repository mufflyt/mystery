#' Map Physicians by State or Subdivision
#'
#' This function generates a choropleth map based on the physician counts and saves the map as a PNG file.
#'
#' @param state_counts A tibble containing the counts of physicians per state or subdivision.
#' @param output_file_prefix A prefix for the output PNG file of the choropleth map (default is "choropleth_map").
#' @param group_by A string indicating whether the counts are grouped by "state" or "subdivision" (default is "state").
#'
#' @return A ggplot2 object representing the choropleth map.
#'
#' @importFrom dplyr left_join mutate group_by summarize arrange
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_gradient theme_void labs geom_text scale_color_identity ggsave
#' @examples
#' # Example 1: Map physicians by state
#' state_counts <- count_physicians_by_group(taxonomy_and_aaos_data)
#' map_physicians_by_group(state_counts)
#'
#' # Example 2: Map physicians by U.S. Census Bureau subdivision
#' subdivision_counts <- count_physicians_by_group(taxonomy_and_aaos_data, group_by = "subdivision")
#' map_physicians_by_group(subdivision_counts, group_by = "subdivision")
#'
#' @export
map_physicians_by_group <- function(state_counts,
                                    output_file_prefix = "choropleth_map",
                                    group_by = "state") {

  # Log the inputs to the function
  logger::log_info(glue::glue("Mapping physicians grouped by {group_by}"))

  # Step 3: Merge counts with map data
  map_data_with_counts <- if (group_by == "state") {
    merge_state_counts_with_map(state_counts)
  } else {
    merge_subdivision_counts_with_map(state_counts)
  }

  # Step 4: Calculate centroids for labeling
  state_centroids <- calculate_state_centroids(map_data_with_counts)

  # Step 5: Generate and save the map
  plot <- generate_and_save_map(map_data_with_counts, state_centroids, output_file_prefix)

  # Return the plot object
  return(plot)
}
