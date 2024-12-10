#' Uninsured Rate Summary by State and Gender
#'
#' This dataset provides the uninsured rates for both males and females across different states,
#' calculated using data from the American Community Survey (ACS).
#'
#' @format A tibble with 52 rows and 9 variables:
#' \describe{
#'   \item{State}{Name of the state}
#'   \item{uninsured_female}{Number of uninsured females}
#'   \item{uninsured_male}{Number of uninsured males}
#'   \item{total_male}{Total male population}
#'   \item{total_female}{Total female population}
#'   \item{uninsured_male_rate}{Proportion of uninsured males}
#'   \item{uninsured_female_rate}{Proportion of uninsured females}
#'   \item{uninsured_male_female}{Total number of uninsured individuals (male and female)}
#'   \item{total_male_female}{Total population (male and female)}
#' }
#' @source American Community Survey (ACS)
"uninsured_rate_summary"
