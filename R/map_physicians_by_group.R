#' Map Physicians by State or Subdivision
#'
#' This function generates a choropleth map based on physician counts and saves the map as a PNG file.
#'
#' @param state_counts A tibble containing the counts of physicians per state or subdivision. Must include columns `region` and `count`.
#' @param output_file_prefix A character string specifying the prefix for the output PNG file of the choropleth map. Default is "choropleth_map".
#' @param group_by A character string indicating whether the counts are grouped by "state" or "subdivision". Default is "state".
#'
#' @return A ggplot2 object representing the choropleth map.
#'
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom dplyr left_join mutate group_by summarize arrange
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_gradient theme_void labs geom_text scale_color_identity ggsave
#' @importFrom glue glue
#' @importFrom logger log_info
#' @examples
#' # Example 1: Map physicians by state
#' \dontrun{
#' state_counts <- count_physicians_by_group(taxonomy_and_aaos_data)
#' map_physicians_by_group(state_counts)
#' }
#'
#' # Example 2: Map physicians by U.S. Census Bureau subdivision
#' \dontrun{
#' subdivision_counts <- count_physicians_by_group(taxonomy_and_aaos_data, group_by = "subdivision")
#' map_physicians_by_group(subdivision_counts, group_by = "subdivision")
#' }
#'
#' # Example 3: Save the map with a custom output file prefix
#' \dontrun{
#' state_counts <- count_physicians_by_group(taxonomy_and_aaos_data)
#' map_physicians_by_group(state_counts, output_file_prefix = "custom_map_prefix")
#' }
#'
#' @export
map_physicians_by_group <- function(state_counts,
                                    output_file_prefix = "choropleth_map",
                                    group_by = "state") {
  # Validate inputs
  assertthat::assert_that(is.data.frame(state_counts),
    msg = "`state_counts` must be a tibble or dataframe."
  )
  assertthat::assert_that("region" %in% names(state_counts) && "count" %in% names(state_counts),
    msg = "`state_counts` must include columns `region` and `count`."
  )
  assertthat::assert_that(assertthat::is.string(output_file_prefix),
    msg = "`output_file_prefix` must be a character string."
  )
  assertthat::assert_that(group_by %in% c("state", "subdivision"),
    msg = "`group_by` must be either 'state' or 'subdivision'."
  )

  # Log the inputs to the function
  logger::log_info(glue::glue("Mapping physicians grouped by {group_by}"))

  # Step 1: Merge counts with map data
  map_data_with_counts <- if (group_by == "state") {
    merge_state_counts_with_map(state_counts)
  } else {
    merge_subdivision_counts_with_map(state_counts)
  }

  # Step 2: Calculate centroids for labeling
  state_centroids <- calculate_state_centroids(map_data_with_counts)

  # Step 3: Generate and save the map
  plot <- generate_and_save_map(map_data_with_counts, state_centroids, output_file_prefix)

  # Return the plot object
  return(plot)
}
