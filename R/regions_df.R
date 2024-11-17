#' U.S. Census Bureau Regions and Divisions
#'
#' This dataset provides region and division information for U.S. states, based on the U.S. Census Bureau classifications.
#'
#' @format A data frame with 51 rows and 4 variables:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "California").}
#'   \item{State.Code}{The two-letter postal abbreviation for the state (e.g., "CA").}
#'   \item{Region}{The region of the state (e.g., "West").}
#'   \item{Division}{The division of the state (e.g., "Pacific").}
#' }
#'
#' @details
#' - This dataset can be used to group U.S. states by region or division for analysis.
#' - The `Region` column categorizes states into Northeast, Midwest, South, or West.
#' - The `Division` column provides finer granularity within each region.
#'
#' @source U.S. Census Bureau
#'
#' @examples
#' # Load the dataset
#' data(regions_df)
#'
#' # View the first few rows
#' head(regions_df)
#'
#' # Count states by region
#' table(regions_df$Region)
#'
#' # Subset states in the Pacific division
#' subset(regions_df, Division == "Pacific")
#'
#' @keywords dataset regions divisions
"regions_df"
