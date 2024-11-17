#' ACOG Presidents
#'
#' This dataset lists the past and current presidents of the American College of Obstetricians and Gynecologists (ACOG).
#' It provides historical insights into leadership over the years and can be used for reference or research on the organization's history.
#'
#' @format A data frame with two columns:
#' \describe{
#'   \item{Name}{The full name of the ACOG president (e.g., "Dr. Jane Smith").}
#'   \item{Year}{The year(s) they served as president (e.g., "2020-2022").}
#' }
#' @details
#' - ACOG presidents are elected by the organization's members to serve a term, providing strategic leadership.
#' - This dataset can support studies on leadership trends or ACOG's organizational history.
#'
#' @source
#' - [ACOG Official Website](https://www.acog.org)
#' - Historical archives and leadership directories from ACOG.
#'
#' @examples
#' # Load the dataset
#' data(acog_presidents)
#'
#' # View the first few rows
#' head(acog_presidents)
#'
#' # Find presidents who served in the 2000s
#' subset(acog_presidents, grepl("^200", Year))
"acog_presidents"
