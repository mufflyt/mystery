#' Calculate the Percentage of the Most Common Value in a Categorical Variable
#'
#' This function calculates the percentage of the most frequent value in a specified categorical variable within a data frame. It identifies the most common value, its count, and its proportion relative to the total number of observations.
#'
#' @param df A data frame containing the categorical variable to analyze.
#' @param variable A character string specifying the name of the categorical variable in `df`.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{\code{variable}}{The most common value of the specified variable.}
#'   \item{\code{n}}{The count of the most common value.}
#'   \item{\code{percentage}}{The percentage of the total count represented by the most common value.}
#' }
#'
#' @details
#' The function first converts the specified variable to a character type if it is a factor. It then counts the occurrences of each unique value in the variable, identifies the most frequent value, and calculates its percentage of the total. If there are ties for the most common value, only one is returned.
#'
#' @examples
#' # Example 1: Basic usage with a simple dataset
#' df <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
#' result <- calcpercentages(df, "category")
#' print(result)
#'
#' # Example 2: Dataset with ties for the most common value
#' df_tie <- data.frame(category = c("A", "B", "A", "B", "C", "C", "C", "A", "B"))
#' result <- calcpercentages(df_tie, "category")
#' print(result)
#'
#' # Example 3: Dataset with missing values
#' df_na <- data.frame(category = c("A", NA, "A", "C", "A", "B", "B", NA))
#' result <- calcpercentages(df_na, "category")
#' print(result)
#'
#' @importFrom dplyr count slice_max mutate
#' @export
calcpercentages <- function(df, variable) {
  variable <- as.character(variable)  # Convert variable name to character

  # Calculate counts and identify the most common value
  x <- df %>%
    dplyr::count({{ variable }}, name = "n") %>%
    dplyr::mutate(percentage = n / sum(n) * 100) %>%
    dplyr::slice_max(n, n = 1)

  return(x)
}
