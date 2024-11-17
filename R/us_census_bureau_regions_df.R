#' U.S. Census Bureau Regions and Divisions
#'
#' This dataset provides region and division information for U.S. states.
#'
#' @format A data frame with 51 rows and 4 variables:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "California").}
#'   \item{State.Code}{The two-letter postal abbreviation for the state.}
#'   \item{Region}{The region of the state (e.g., "West").}
#'   \item{Division}{The division of the state (e.g., "Pacific").}
#' }
#' @source Derived from U.S. Census Bureau data.
#' @examples
#' data(us_census_bureau_regions_df)
#' head(us_census_bureau_regions_df)
"us_census_bureau_regions_df"
