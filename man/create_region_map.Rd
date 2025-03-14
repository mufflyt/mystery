% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_create_region_map.R
\name{create_region_map}
\alias{create_region_map}
\title{Create a Map of U.S. States by Region, Division, or Custom Districts}
\usage{
create_region_map(
  remove_ak_hi = TRUE,
  districts_per_group = "acog_districts",
  save_path = NULL,
  alpha_level = 0.4,
  title = "U.S. States by Region/Division or Custom Districts",
  subtitle = NULL
)
}
\arguments{
\item{remove_ak_hi}{Logical. Whether to exclude Alaska and Hawaii from the map. Default is TRUE.}

\item{districts_per_group}{Character. Which grouping to use for the regions. Options are:
\itemize{
\item "acog_districts"
\item "ENT_Board_of_Governors_Regions"
\item "US_Census_Subdivisions"
}}

\item{save_path}{Character. An optional file path to save the map image. If NULL, the map will not be saved. Default is NULL.}

\item{alpha_level}{Numeric. The transparency level for the map's fill color, with 0 being fully transparent and 1 fully opaque. Default is 0.4.}

\item{title}{Character. The title for the map. Default is "U.S. States by Region/Division or Custom Districts".}

\item{subtitle}{Character. An optional subtitle for the map. Default is NULL.}
}
\value{
A ggplot object representing the U.S. states colored by region/division, ACOG districts, or ENT_Board_of_Governors_Regions.
}
\description{
This function generates a map of U.S. states, coloring them by regions, divisions, ACOG districts, or
ENT Board of Governors Regions. Users can customize which grouping to use (ACOG Districts, ENT_Board_of_Governors_Regions, or US Census Subdivisions).
}
\examples{
# Example 1: Create a map of U.S. states by ACOG districts
\dontrun{
map_acog <- create_region_map(
  remove_ak_hi = TRUE,
  districts_per_group = "acog_districts",
  save_path = "acog_district_map.png",
  alpha_level = 0.4,
  title = "Map of U.S. States by ACOG Districts",
  subtitle = "Excluding Alaska and Hawaii"
)
print(map_acog)
}

# Example 2: Create a map of U.S. states by ENT Board of Governors Regions
\dontrun{
map_ent <- create_region_map(
  remove_ak_hi = FALSE,
  districts_per_group = "ENT_Board_of_Governors_Regions",
  alpha_level = 0.5,
  title = "Map of ENT Board of Governors Regions",
  subtitle = "Including Alaska and Hawaii"
)
print(map_ent)
}

# Example 3: Create a map of U.S. states by Census Subdivisions without saving
\dontrun{
map_census <- create_region_map(
  remove_ak_hi = TRUE,
  districts_per_group = "US_Census_Subdivisions",
  save_path = NULL,
  alpha_level = 0.3,
  title = "Map of U.S. Census Subdivisions",
  subtitle = NULL
)
print(map_census)
}

}
