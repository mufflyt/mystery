#' Medicaid-to-Medicare Fee Index
#'
#' A dataset measuring each state's physician fees relative to Medicare fees in each state.
#' The Medicaid-to-Medicare fee index compares the Medicaid fee for each service in each state
#' to the Medicare fee for the same services, using surveys conducted by the Urban Institute.
#'
#' All numeric values are scaled as percentages (multiplied by 100).
#'
#' @format A data frame with 52 rows and 5 columns:
#' \describe{
#'   \item{Location}{State name or abbreviation.}
#'   \item{All Services}{Fee index for all services, relative to Medicare, as a percentage.}
#'   \item{Primary Care}{Fee index for primary care services eligible for the ACA "fee bump," as a percentage.}
#'   \item{Obstetric Care}{Fee index for obstetric care services, as a percentage.}
#'   \item{Other Services}{Fee index for other non-primary care services, as a percentage.}
#' }
#' @source Urban Institute: \url{https://www.kff.org/medicaid/state-indicator/medicaid-to-medicare-fee-index/}.
"medicaid_to_medicare_fee_index"
