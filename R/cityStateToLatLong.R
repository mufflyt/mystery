#' City and State to Latitude and Longitude
#'
#' This dataset maps U.S. cities and states to their geographic coordinates (latitude and longitude).
#' It provides detailed location information for approximately 31,909 cities across the United States,
#' making it a valuable resource for geographic analyses, mapping, and location-based studies.
#'
#' @format A tibble with 31,909 rows and 4 variables:
#' \describe{
#'   \item{state}{The name of the U.S. state (e.g., "Alabama", "California").}
#'   \item{city}{The name of the city within the state (e.g., "Los Angeles", "Denver").}
#'   \item{latitude}{The latitude of the city in decimal degrees (e.g., 34.0522).}
#'   \item{longitude}{The longitude of the city in decimal degrees (e.g., -118.2437).}
#' }
#'
#' @details
#' - Geographic data is sourced from authoritative mapping datasets, ensuring accurate coordinates.
#' - This dataset can be used for:
#'   - Mapping and visualization of city-level data.
#'   - Location-based research and geographic clustering.
#'   - Integration with geospatial tools for further analysis.
#' - Note: Latitude and longitude are provided in the WGS 84 coordinate system, a standard for global mapping.
#'
#' @source
#' - Derived from U.S. Census Bureau's Gazetteer Files and OpenStreetMap geocoding data.
#' - [U.S. Census Bureau](https://www.census.gov/geographies/mapping-files.html)
#' - [OpenStreetMap](https://www.openstreetmap.org/)
#'
#' @examples
#' # Load the dataset
#' data(cityStateToLatLong)
#'
#' # View the first few rows
#' head(cityStateToLatLong)
#'
#' # Filter for cities in Alabama
#' subset(cityStateToLatLong, state == "Alabama")
#'
#' # Plot cities in a specific state
#' library(ggplot2)
#' ggplot(
#'   subset(cityStateToLatLong, state == "California"),
#'   aes(x = longitude, y = latitude)
#' ) +
#'   geom_point() +
#'   labs(title = "Cities in California", x = "Longitude", y = "Latitude") +
#'   theme_minimal()
#'
#' # Find the coordinates of a specific city
#' subset(cityStateToLatLong, city == "Denver" & state == "Colorado")
"cityStateToLatLong"
