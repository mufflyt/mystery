#' FIPS Codes for U.S. States
#'
#' This dataset provides Federal Information Processing Standards (FIPS) codes for U.S. states.
#'
#' @format A data frame with 52 rows and 3 variables:
#' \describe{
#'   \item{state}{The name of the U.S. state (e.g., "Alabama").}
#'   \item{state_code}{The two-digit FIPS code for the state (e.g., "01").}
#'   \item{state_name}{The full name of the state (e.g., "Alabama").}
#' }
#'
#' @details
#' This dataset maps state FIPS codes to their names and abbreviations.
#'
#' @source Derived from U.S. Census Bureau data.
#'
#' @examples
#' # Load the dataset
#' data(fips)
#'
#' # View the first few rows
#' head(fips)
#'
#' # Get FIPS code for a specific state
#' subset(fips, state == "California")
#'
#' @keywords dataset FIPS
"fips"
