#' Rural-Urban Commuting Area (RUCA) Codes by ZIP Code
#'
#' This dataset provides Rural-Urban Commuting Area (RUCA) codes and related rural/urban
#' classifications for U.S. ZIP codes, including information about primary and secondary
#' RUCA designations and AHRQ rural vs. urban status.
#'
#' @format A tibble with 35,648 rows and 10 variables:
#' \describe{
#'   \item{zip}{ZIP code (character)}
#'   \item{USPS_ZIP_PREF_CITY}{USPS preferred city name for the ZIP code}
#'   \item{state_name}{Full state or territory name}
#'   \item{AHRQ_rural_vs_urban}{Rural/urban classification according to AHRQ standards
#'         (e.g., "Urban", "Large rural town", "Small rural town")}
#'   \item{primary_ruca_desc}{Description of the primary RUCA classification}
#'   \item{primary_ruca}{Numeric code for primary RUCA classification (1-10)}
#'   \item{secondary_ruca}{Numeric code for secondary RUCA classification (1-10)}
#'   \item{secondary_ruca_desc}{Description of the secondary RUCA classification}
#'   \item{tract_fips}{Census tract FIPS code}
#'   \item{res_ratio}{Residential ratio within the ZIP code area}
#' }
#'
#' @details
#' The Rural-Urban Commuting Area (RUCA) codes are a census tract-based classification
#' scheme that utilizes the Bureau of Census urbanized area and urban cluster definitions
#' with work commuting information to characterize all of the nation's census tracts
#' regarding their rural and urban status and relationships.
#'
#' This dataset is particularly useful for:
#' - Healthcare access and delivery studies
#' - Rural health research
#' - Geographic analysis of healthcare disparities
#' - Policy planning for rural and urban healthcare services
#'
#' @source USDA Economic Research Service and AHRQ rural-urban classifications
#' <https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/>
#'
#' @examples
#' # Load the RUCA dataset
#' data(zip_to_ruca)
#'
#' # View distribution of AHRQ rural/urban classifications
#' dplyr::count(zip_to_ruca, AHRQ_rural_vs_urban)
#'
#' # Find all ZIP codes classified as "Large rural town"
#' large_rural <- zip_to_ruca %>%
#'   dplyr::filter(AHRQ_rural_vs_urban == "Large rural town")
#'
#' # Calculate average residential ratio by primary RUCA code
#' zip_to_ruca %>%
#'   dplyr::group_by(primary_ruca, primary_ruca_desc) %>%
#'   dplyr::summarise(
#'     avg_res_ratio = mean(res_ratio, na.rm = TRUE),
#'     n = n()
#'   )
#'
#' @keywords dataset healthcare geography RUCA rural urban
"zip_to_ruca"
