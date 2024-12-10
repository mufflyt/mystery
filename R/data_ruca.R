#' Taxonomy Codes for Healthcare Providers
#'
#' This dataset contains the taxonomy codes for healthcare providers, including classifications
#' and specializations, as defined by the National Uniform Claim Committee (NUCC).
#' It is particularly useful for mapping provider types to their respective NUCC taxonomy codes.
#'
#' @format A data frame with 862 rows and 3 variables:
#' \describe{
#'   \item{Code}{The NUCC taxonomy code for healthcare providers (e.g., "101Y00000X").}
#'   \item{Classification}{The general classification of the provider (e.g., "Counselor").}
#'   \item{Specialization}{The specific specialization within the classification
#'         (e.g., "Addiction (Substance Use Disorder)"). May be \code{NA} if not applicable.}
#' }
#'
#' @details
#' - The dataset includes taxonomy codes across various provider types, not limited to Obstetricians
#'   and Gynecologists.
#' - The NUCC taxonomy is widely used in healthcare for credentialing, claims processing, and
#'   provider directories.
#' - This dataset can assist in linking provider specialties with their taxonomy codes for research
#'   and administrative purposes.
#'
#' @source NUCC taxonomy codes, version 23.0:
#' <https://www.nucc.org/images/stories/PDF/taxonomy_23_0.pdf>
#'
#' @examples
#' # Load the taxonomy dataset
#' data(taxonomy)
#'
#' # View the first few rows
#' head(taxonomy)
#'
#' # Filter for Obstetricians and Gynecologists
#' obgyn_taxonomy <- taxonomy %>%
#'   dplyr::filter(grepl("Obstetrics|Gynecology", Classification, ignore.case = TRUE))
#'
#' # Count the number of unique classifications
#' dplyr::count(taxonomy, Classification)
#'
#' @keywords dataset taxonomy healthcare NUCC
"zip_to_ruca"
