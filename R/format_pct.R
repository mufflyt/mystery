#' Format a Numeric Value as a Percentage
#'
#' This function formats a numeric value or vector as a percentage with a specified number of decimal places.
#'
#' @param x A numeric value or vector to format as a percentage.
#' @param my_digits An integer specifying the number of decimal places to include in the formatted percentage.
#'   The default is 1.
#'
#' @return A character vector representing the formatted percentage(s) with the specified number of decimal places.
#'
#' @details The function converts numeric values to a percentage format with the desired number of decimal places.
#'   It is especially useful for ensuring consistent formatting in reports, tables, or visualizations.
#'
#' @examples
#' # Example 1: Format a single numeric value with default decimal places
#' single_value <- format_pct(0.12345)
#' print(single_value)  # Output: "12.3%"
#'
#' # Example 2: Format a vector of numeric values with 2 decimal places
#' values <- c(0.12345, 0.6789, 0.54321)
#' formatted_values <- format_pct(values, my_digits = 2)
#' print(formatted_values)  # Output: "12.35%", "67.89%", "54.32%"
#'
#' # Example 3: Format a single value with no decimal places
#' no_decimal <- format_pct(0.5, my_digits = 0)
#' print(no_decimal)  # Output: "50%"
#'
#' # Example 4: Format a vector of proportions with varying decimal places
#' proportions <- c(0.1, 0.25, 0.33333, 0.9)
#' formatted_proportions <- format_pct(proportions, my_digits = 3)
#' print(formatted_proportions)  # Output: "10.000%", "25.000%", "33.333%", "90.000%"
#'
#' @importFrom assertthat assert_that
#' @export
format_pct <- function(x, my_digits = 1) {
  # Validate inputs
  assertthat::assert_that(is.numeric(x), msg = "`x` must be a numeric value or vector.")
  assertthat::assert_that(is.numeric(my_digits) && my_digits >= 0,
                          msg = "`my_digits` must be a non-negative integer.")

  # Format values as percentages
  formatted <- sprintf(paste0("%.", my_digits, "f%%"), x * 100)
  return(formatted)
}
