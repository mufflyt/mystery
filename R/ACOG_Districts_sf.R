#' ACOG Districts Spatial Data
#'
#' This dataset provides the spatial geometries for the American College of Obstetricians and Gynecologists (ACOG) districts in the United States, including their corresponding state names and district designations.
#'
#' The dataset is structured as a `sf` object and includes merged geometries for each ACOG district, allowing for visualization and spatial analysis of district boundaries.
#'
#' @format An `sf` object with the following columns:
#' \describe{
#'   \item{ACOG_District}{The name of the ACOG district (e.g., "District I", "District VIII").}
#'   \item{geometry}{The spatial geometry (polygons) of the district boundaries.}
#' }
#'
#' @details
#' The `ACOG_Districts_sf` object is a spatial dataset that can be used to map ACOG districts or perform geospatial operations. The district boundaries were created by merging state geometries according to the ACOG district mapping.
#'
#' @source
#' State geometries were obtained from the TIGER/Line shapefiles via the `tigris` R package. ACOG district information was sourced from the official ACOG website: <https://www.acog.org/community/districts-and-sections>.
#'
#' @examples
#' # Load the ACOG Districts Spatial Data
#' data(ACOG_Districts_sf)
#'
#' # View the structure of the dataset
#' str(ACOG_Districts_sf)
#'
#' # Plot the district boundaries using ggplot2
#' library(ggplot2)
#' ggplot(ACOG_Districts_sf) +
#'   geom_sf() +
#'   ggtitle("ACOG Districts in the United States") +
#'   theme_minimal()
#'
#' @keywords dataset spatial
"ACOG_Districts_sf"
