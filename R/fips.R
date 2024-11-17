#' FIPS Code Dataset
#'
#' This dataset provides Federal Information Processing Standards (FIPS) codes for U.S. states
#' and counties. FIPS codes are used in various governmental and statistical applications
#' to uniquely identify geographic regions.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "California", "Texas").}
#'   \item{County}{The name of the county (e.g., "Los Angeles County").}
#'   \item{FIPS}{The FIPS code for the county (e.g., "06037" for Los Angeles County).}
#' }
#' @details
#' - FIPS codes are hierarchical: state codes are the first two digits, and county codes follow.
#' - They are widely used in geographic data systems and federal reporting.
#'
#' @source
#' - [U.S. Census Bureau FIPS Codes](https://www.census.gov/library/reference/code-lists/ansi.html)
#'
#' @examples
#' # Load the dataset
#' data(fips)
#'
#' # Find the FIPS code for a specific county
#' subset(fips, County == "Los Angeles County")
#'
#' # Filter counties in California
#' subset(fips, State == "California")
"fips"
