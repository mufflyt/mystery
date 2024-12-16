#' Calculate the Maximum Value(s) and Corresponding Level(s) of a Factor Variable
#'
#' This function calculates the maximum frequency of a factor variable and returns the level(s)
#' corresponding to the maximum value(s). It can return either a single maximum or all levels with the maximum frequency.
#'
#' @param InVec Input vector, expected to be a factor variable or convertible to a factor.
#' @param mult Logical value indicating whether to return multiple maximum levels or just the first one. Default is FALSE.
#'
#' @return A character vector. If \code{mult} is FALSE, it returns the level with the highest frequency.
#'         If \code{mult} is TRUE, it returns all levels with the maximum frequency.
#'
#' @details This function handles both factor variables and those that can be converted to factors.
#'          It uses frequency counts to determine the level(s) with the maximum frequency.
#'
#' @importFrom assertthat assert_that is.flag
#' @examples
#' # Example 1: Single maximum level
#' vec <- factor(c("A", "B", "A", "C", "B", "B"))
#' MaxTable(vec) # Returns "B"
#'
#' # Example 2: Multiple maximum levels
#' vec <- factor(c("A", "B", "A", "C", "B", "B"))
#' MaxTable(vec, mult = TRUE) # Returns c("A", "B")
#'
#' # Example 3: Handling a non-factor input
#' vec <- c("A", "B", "A", "C", "B", "B")
#' MaxTable(vec) # Returns "B"
#'
#' # Example 4: Empty input vector
#' vec <- factor(character(0))
#' MaxTable(vec) # Returns character(0)
#'
#' @export
MaxTable <- function(InVec, mult = FALSE) {
  # Validate inputs
  assertthat::assert_that(length(InVec) > 0, msg = "Input vector must not be empty.")
  assertthat::assert_that(assertthat::is.flag(mult), msg = "`mult` must be a logical value.")

  # Convert to factor if necessary
  if (!is.factor(InVec)) {
    InVec <- factor(InVec)
  }

  # Compute frequency counts
  freq_counts <- tabulate(InVec)

  # Return maximum level(s) based on `mult`
  if (isTRUE(mult)) {
    levels(InVec)[freq_counts == max(freq_counts)]
  } else {
    levels(InVec)[which.max(freq_counts)]
  }
}
