#' Convert a List of Column Names to an Expanded Data Frame
#'
#' This helper function converts a named list of column names, grouped by table, into an expanded data frame
#' where each column name is placed into a separate column.
#'
#' @param column_list A named list where each element contains the column names of a table.
#' @return A data frame with each table name and its corresponding columns.
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom purrr map_dfc map_int
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @examples
#' # Example 1: Convert a list of column names to an expanded data frame
#' test_list <- list(table1 = c("col1", "col2"), table2 = c("col1", "col2", "col3"))
#' expanded_df <- convert_list_to_df_expanded(test_list)
#' print(expanded_df)
#'
#' # Example 2: Handling missing columns in some tables
#' test_list <- list(table1 = c("col1", "col2"), table2 = c("col1"))
#' expanded_df <- convert_list_to_df_expanded(test_list)
#' print(expanded_df)
#'
#' # Example 3: Convert an empty list
#' empty_list <- list()
#' expanded_df_empty <- convert_list_to_df_expanded(empty_list)
#' print(expanded_df_empty)  # Should return an empty data frame
convert_list_to_df_expanded <- function(column_list) {
  # Log function input
  message("Converting list to data frame...")

  # Filter for general tables (tables that match the "OP_DTL_GNRL_" pattern)
  filtered_list <- column_list[stringr::str_detect(names(column_list), "^OP_DTL_GNRL_")]

  # Log filtered tables
  message(glue::glue("Filtered tables: {paste(names(filtered_list), collapse = ', ')}"))

  # Get the maximum number of columns across all tables
  max_cols <- purrr::map_int(filtered_list, length) %>% max(na.rm = TRUE)

  # Create the base data frame with table names
  df <- tibble::tibble(Table = names(filtered_list))

  # Add each column name as a separate column using map_dfc to iterate over the columns
  column_df <- purrr::map_dfc(seq_len(max_cols), function(i) {
    purrr::map_chr(filtered_list, ~ .x[i] %||% NA_character_) %>%
      tibble::tibble(!!glue::glue("Column_{i}") := .)
  })

  # Combine the table names and the column names into one tibble
  final_df <- dplyr::bind_cols(df, column_df)

  # Log the resulting data frame dimensions
  message(glue::glue("Data frame created with {nrow(final_df)} rows and {ncol(final_df)} columns."))

  return(final_df)
}
