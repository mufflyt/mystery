#' Total Women by ZIP Code
#'
#' A dataset containing the total number of women for each ZIP Code Tabulation Area (ZCTA)
#' in the United States based on the American Community Survey (ACS) 5-Year Estimates.
#'
#' @format A data frame with two columns:
#' \describe{
#'   \item{zip_code}{The ZIP Code Tabulation Area (ZCTA).}
#'   \item{total_women}{The estimated total number of women in the ZCTA.}
#' }
#' @source U.S. Census Bureau, ACS 5-Year Estimates, 2021.
#' @examples
#' data(women_by_zip)
#' head(women_by_zip)
"women_by_zip"
