#' ACOG Districts Data
#'
#' This dataset provides a mapping of U.S. states to their respective districts as defined
#' by the American College of Obstetricians and Gynecologists (ACOG). It also includes
#' state abbreviations and subregion details for additional geographic context.
#'
#' @format A tibble with 52 rows and 4 variables:
#' \describe{
#'   \item{State}{The name of the U.S. state (e.g., "Alabama", "California").}
#' \item{ACOG_District}{The district designation assigned by ACOG (e.g.,"District VII").}
#' \item{Subregion}{The subregion of the state within the district(e.g.,"District VII").}
#' \item{State_Abbreviations}{The two-letter postal abbreviation for the state(e.g., "AL", "CA").}
#' }
#'
#' @details
#' - This dataset is useful for mapping and regional analyses of ACOG districts.
#' - Districts are defined to align with the organization's geographic and administrative structure.
#' - Subregions often match the ACOG districts but can provide additional context when needed.
#'
#' @source Derived from official ACOG district mappings and documentation.
#'
#' @examples
#' # Load the dataset
#' data(ACOG_Districts)
#'
#' # View the first few rows
#' head(ACOG_Districts)
#'
#' # Summarize the number of states in each ACOG district
#' table(ACOG_Districts$ACOG_District)
#'
#' # Filter for states in District VIII
#' subset(ACOG_Districts, ACOG_District == "District VIII")
"ACOG_Districts"
