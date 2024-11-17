#' ACGME OBGYN Residency Program Data
#'
#' This dataset provides comprehensive information about Obstetrics and Gynecology (OBGYN)
#' residency programs accredited by the Accreditation Council for Graduate Medical Education (ACGME).
#' It includes program details such as addresses, accreditation status, program leadership,
#' participating sites, and rotation details.
#'
#' @format A tibble with 318 rows and 142 variables:
#' \describe{
#'   \item{program_name}{The name of the OBGYN residency program.}
#'   \item{address}{The program's mailing address.}
#'   \item{zip}{The program's ZIP code.}
#'   \item{city}{The city where the program is located.}
#'   \item{state}{The state where the program is located.}
#'   \item{sponsoring_institution_code}{The code for the sponsoring institution.}
#'   \item{sponsoring_institution_name}{The name of the sponsoring institution.}
#'   \item{phone}{The main contact phone number for the program.}
#'   \item{original_accreditation_date}{The date when the program first received accreditation.}
#'   \item{accreditation_status}{The current accreditation status (e.g., "Continued Accreditation").}
#'   \item{director_name}{The name of the program director.}
#'   \item{director_date_appointed}{The date the program director was appointed.}
#'   \item{coordinator_name_1}{The name of the program coordinator.}
#'   \item{coordinator_phone_1}{The phone number of the program coordinator.}
#'   \item{coordinator_email_1}{The email address of the program coordinator.}
#'   \item{participation_site_code_*}{The code for each participating site (up to 18 sites).}
#'   \item{participation_site_name_*}{The name of each participating site (up to 18 sites).}
#'   \item{rotation_required_*}{Indicates if a rotation is required at each site ("Yes" or "No").}
#'   \item{rotation_months_y*_#}{The number of months allocated for rotations at each site
#'         by year (up to 4 years and 18 sites).}
#' }
#'
#' @details
#' - This dataset is particularly useful for understanding the structure and requirements
#'   of OBGYN residency programs across the United States.
#' - Rotations are detailed by site and year, allowing for comprehensive planning and analysis.
#' - Data includes program leadership details to facilitate communication and networking.
#'
#' @source Data was obtained from the ACGME website:
#' <https://apps.acgme.org/ads/Public/Programs/Search>
#'
#' @examples
#' # Load the ACGME OBGYN Residency Data
#' data(acgme)
#'
#' # View the first few rows of the dataset
#' head(acgme)
#'
#' # Summarize the dataset
#' summary(acgme)
#'
#' # Analyze the number of programs by state
#' table(acgme$state)
#'
#' # Filter programs in California
#' subset(acgme, state == "California")
#'
#' @keywords dataset residency OBGYN
"acgme"
