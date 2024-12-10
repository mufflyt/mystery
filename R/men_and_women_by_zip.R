#' Men and Women by ZIP Code
#'
#' A dataset containing the total number of men, women, and the total population
#' for each ZIP Code Tabulation Area (ZCTA) in the United States, based on the
#' American Community Survey (ACS) 5-Year Estimates.
#'
#' @format A data frame with three columns:
#' \describe{
#'   \item{zip_code}{The ZIP Code Tabulation Area (ZCTA).}
#'   \item{men}{The estimated total number of men in the ZCTA.}
#'   \item{women}{The estimated total number of women in the ZCTA.}
#'   \item{total_population}{The total estimated population in the ZCTA (men + women).}
#' }
#' @source U.S. Census Bureau, ACS 5-Year Estimates, 2021.
#' @examples
#' data(men_and_women_by_zip)
#' head(men_and_women_by_zip)
"men_and_women_by_zip"
