#' ENT Board of Governors Regions
#'
#' This dataset maps U.S. states to their respective ENT Board of Governors (ENT BOG) Regions.
#' The regions are defined to align with the organizational structure of the ENT Board. This dataset
#' is commonly used for regional analyses, visualizations, and comparisons in studies involving ENT
#' professionals or geographic distributions.
#'
#' @format A data frame with 50 rows and 2 variables:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "California", "Texas").}
#'   \item{ENT_BOG_Region}{The corresponding ENT Board of Governors Region (e.g., "Region 1", "Region 2").}
#' }
#' @details
#' - Region 1 includes the New England states.
#' - Region 2 encompasses New York, New Jersey, and U.S. territories in the Caribbean.
#' - Regions are organized for administrative and organizational purposes and may not align
#'   with standard U.S. Census divisions.
#'
#' @source
#' - ENT Board of Governors organizational materials.
#' - U.S. Census Bureau shapefiles for geographic data: <https://www.census.gov/geographies/mapping-files.html>
#'
#' @references
#' ENT Board of Governors. "Regional Organization Overview." Retrieved from
#' <https://www.entnet.org/about-us/board-of-governors>.
#'
#' @examples
#' # Load the dataset
#' data(ent_bog_regions)
#'
#' # View the first few rows
#' head(ent_bog_regions)
#'
#' # Count the number of states in each ENT BOG Region
#' table(ent_bog_regions$ENT_BOG_Region)
"ent_bog_regions"

#' ACOG Districts
#'
#' This dataset maps U.S. states to their respective districts as defined by the American College of
#' Obstetricians and Gynecologists (ACOG). ACOG districts are used to organize and support members
#' based on geographic regions.
#'
#' @format A data frame with 52 rows and 2 variables:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "California", "Texas").}
#'   \item{ACOG_District}{The corresponding ACOG district (e.g., "District I", "District VII").}
#' }
#' @details
#' - Districts include states, U.S. territories, and specific regions like Washington, D.C.
#' - District XII includes Florida as a standalone district.
#' - Districts are commonly used for organizing conferences, membership activities, and policy efforts.
#'
#' @source
#' - ACOG district mappings from the official website: <https://www.acog.org/community/districts-and-sections>.
#' - State-level geographic data from the U.S. Census Bureau: <https://www.census.gov>.
#'
#' @references
#' American College of Obstetricians and Gynecologists (ACOG). "Districts and Sections."
#' Retrieved from <https://www.acog.org/community/districts-and-sections>.
#'
#' @examples
#' # Load the dataset
#' data(acog_districts_df)
#'
#' # Summarize the number of states in each ACOG district
#' table(acog_districts_df$ACOG_District)
#'
#' # Filter for states in District VIII
#' subset(acog_districts_df, ACOG_District == "District VIII")
"acog_districts_df"

#' U.S. Regions and Divisions
#'
#' This dataset provides comprehensive region and division information for U.S. states. It includes
#' regional groupings based on the U.S. Census Bureau's classifications and supports a wide range
#' of analyses involving geographic, economic, or demographic data.
#'
#' @format A data frame with 51 rows and 4 variables:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "California", "Texas").}
#'   \item{State Code}{The two-letter postal abbreviation for the state (e.g., "CA", "TX").}
#'   \item{Region}{The region of the state (e.g., "West", "South").}
#'   \item{Division}{The division of the state (e.g., "Pacific", "Mountain").}
#' }
#' @details
#' - Regions are classified as Northeast, South, Midwest, and West.
#' - Divisions are more granular groupings within regions, such as Pacific, Mountain, and East North Central.
#' - The dataset aligns with U.S. Census Bureau standards, making it suitable for demographic studies.
#'
#' @source
#' - U.S. Census Bureau. "Regions and Divisions of the United States."
#'   Available at <https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf>.
#'
#' @references
#' U.S. Census Bureau. "Geographic Terms and Concepts - Census Divisions and Regions."
#' Retrieved from <https://www.census.gov/programs-surveys/geography/guidance/geo-areas/reg-div.html>.
#'
#' @examples
#' # Load the dataset
#' data(regions_df)
#'
#' # View the unique regions
#' unique(regions_df$Region)
#'
#' # Count states per division
#' table(regions_df$Division)
#'
#' # Filter for states in the Pacific division
#' subset(regions_df, Division == "Pacific")
"us_census_bureau_regions_df"
