#' Coalesce Columns by Mode Excluding NAs
#'
#' This function combines values from multiple columns into a new column. If
#' `column_coalesce_names` is provided, those columns are used. Otherwise,
#' columns are selected based on `column_name_pattern`. The resulting column
#' contains the mode (most frequent value) of the selected columns for each
#' row, excluding \code{NA} values. The user can optionally delete the columns
#' used for coalescing after the operation.
#'
#' @param input_dataframe A \code{data.frame} or \code{tibble} to process.
#' @param column_name_pattern A character string used to identify columns to
#'   coalesce. Ignored if \code{column_coalesce_names} is provided.
#' @param column_coalesce_names A character vector specifying the exact column
#'   names to coalesce. Overrides \code{column_name_pattern} if provided.
#' @param new_column_name A character string specifying the name of the new
#'   column to store coalesced values. Defaults to \code{"coalesced_column"}.
#' @param delete_columns A logical flag indicating whether the columns used
#'   for coalescing should be removed from the dataframe. Defaults to
#'   \code{FALSE}.
#' @param verbose A logical flag indicating whether detailed logs should be
#'   printed during execution. Defaults to \code{FALSE}.
#'
#' @returns A \code{data.frame} or \code{tibble} with the new column added. If
#'   \code{delete_columns} is \code{TRUE}, the columns used for coalescing are
#'   removed from the dataframe, except for \code{new_column_name}.
#'
#' @importFrom dplyr select mutate rowwise ungroup
#' @importFrom dplyr c_across contains all_of
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#'
#' @examples
#' # Example 1: Coalesce using a pattern and keep original columns
#' example_df <- tibble::tibble(
#'   address_state = c("NY", NA, "CA", NA),
#'   license_state = c(NA, "TX", "CA", NA),
#'   profile_state = c("NY", "TX", NA, NA)
#' )
#'
#' result_df <- phase0_coalesce(
#'   input_dataframe = example_df,
#'   column_name_pattern = "state",
#'   verbose = TRUE
#' )
#' print(result_df)
#'
#' # Example 2: Coalesce and delete original columns
#' delete_columns_df <- tibble::tibble(
#'   col1 = c("A", "B", NA, "C"),
#'   col2 = c("X", "B", NA, "C"),
#'   col3 = c(NA, "B", NA, NA)
#' )
#'
#' result_delete <- phase0_coalesce(
#'   input_dataframe = delete_columns_df,
#'   column_coalesce_names = c("col1", "col2"),
#'   new_column_name = "combined_col",
#'   delete_columns = TRUE,
#'   verbose = TRUE
#' )
#' print(result_delete)
#'
#' @export
phase0_coalesce <- function(
    input_dataframe,
    column_name_pattern = NULL,
    column_coalesce_names = NULL,
    new_column_name = "coalesced_column",
    delete_columns = FALSE,
    verbose = TRUE
) {
  # Function to calculate the mode excluding NA
  calculate_mode <- function(values) {
    values <- na.omit(values)  # Exclude NA values
    if (length(values) == 0) return(NA)  # Return NA if all values are NA
    unique_values <- unique(values)
    unique_values[which.max(tabulate(match(values, unique_values)))]  # Return the mode
  }

  # Validate inputs
  assert_that(is.data.frame(input_dataframe), msg = "The input 'input_dataframe' must be a dataframe.")
  assert_that(is.character(new_column_name), msg = "'new_column_name' must be a character string.")
  assert_that(length(new_column_name) == 1, msg = "'new_column_name' must be a single string.")

  # Determine columns to coalesce
  if (!is.null(column_coalesce_names)) {
    assert_that(all(column_coalesce_names %in% colnames(input_dataframe)),
                msg = "Some columns in 'column_coalesce_names' do not exist in the input dataframe.")
    columns_to_coalesce <- column_coalesce_names
  } else {
    assert_that(!is.null(column_name_pattern), msg = "Either 'column_name_pattern' or 'column_coalesce_names' must be provided.")
    assert_that(is.character(column_name_pattern), msg = "'column_name_pattern' must be a character string.")
    assert_that(length(column_name_pattern) == 1, msg = "'column_name_pattern' must be a single string.")
    columns_to_coalesce <- colnames(select(input_dataframe, contains(column_name_pattern)))
    assert_that(length(columns_to_coalesce) > 0, msg = glue("No columns found containing the pattern '{column_name_pattern}'."))
  }

  if (verbose) {
    cat("Starting phase0_coalesce...\n")
    cat(glue("Input dataframe has {nrow(input_dataframe)} rows and {ncol(input_dataframe)} columns.\n"))
    cat(glue("Coalescing columns: {paste(columns_to_coalesce, collapse = ', ')}...\n"))
  }

  # Perform row-wise coalescing with mode calculation
  updated_dataframe <- input_dataframe %>%
    rowwise() %>%
    mutate(!!new_column_name := calculate_mode(c_across(all_of(columns_to_coalesce)))) %>%
    ungroup()

  # Optionally delete the coalesced columns, excluding the new column
  if (delete_columns) {
    columns_to_remove <- setdiff(columns_to_coalesce, new_column_name)
    updated_dataframe <- updated_dataframe %>%
      select(-all_of(columns_to_remove))
    if (verbose) {
      cat(glue("Deleted columns: {paste(columns_to_remove, collapse = ', ')}\n"))
    }
  }

  if (verbose) {
    cat(glue("Added new column '{new_column_name}' to the dataframe.\n"))
    cat(glue("Updated dataframe has {nrow(updated_dataframe)} rows and {ncol(updated_dataframe)} columns.\n"))
  }

  return(updated_dataframe)
}
