#' Board-Certified OBGYN Subspecialists
#'
#' This dataset contains information about board-certified obstetrician-gynecologist (OBGYN) subspecialists in the United States, including their National Provider Identifier (NPI), names, subspecialties, and geographic coordinates.
#'
#' @format A tibble with 4,659 rows and 5 columns:
#' \describe{
#'   \item{NPI}{A numeric value representing the National Provider Identifier for each physician.}
#'   \item{name}{A character string containing the name of the physician.}
#'   \item{subspecialty}{A character string describing the physician's OBGYN subspecialty.}
#'   \item{lat}{The latitude of the physician's primary practice location.}
#'   \item{long}{The longitude of the physician's primary practice location.}
#' }
#'
#' @details
#' The dataset provides details about physicians specialized in OBGYN subspecialties such as Female Pelvic Medicine, Maternal-Fetal Medicine, Gynecologic Oncology, and more. The data includes geographic information for mapping and spatial analysis.
#'
#' @source
#' This data was manually compiled for demonstration purposes. National Provider Identifiers (NPI) and subspecialties are based on publicly available data sources.
#'
#' @examples
#' # Load the physicians dataset
#' data(physicians)
#'
#' # View the structure of the dataset
#' str(physicians)
#'
#' # Summary of subspecialties
#' summary(physicians$subspecialty)
#'
#' # Basic mapping using ggplot2
#' library(ggplot2)
#' ggplot(physicians, aes(x = long, y = lat, color = subspecialty)) +
#'   geom_point() +
#'   ggtitle("Locations of Board-Certified OBGYN Subspecialists") +
#'   theme_minimal()
#'
#' @keywords dataset
"physicians"
