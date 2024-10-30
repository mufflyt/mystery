#' Query sample data from a DuckDB table
#'
#' This function queries a sample of data from a specified DuckDB table.
#'
#' @details After querying the sample data, the next step is to process the NPI table using `nppes_process_npi_table_chunk()`.
#'
#' @param con A DuckDB connection object.
#' @param table_name A character string specifying the name of the table to query. Default is "npi_2024".
#' @param limit An integer specifying the number of rows to retrieve. Default is 5.
#' @param save_column_in_each_nppes_year A logical value indicating whether to save the sample data to an Excel file. Default is FALSE.
#' @param excel_file_path A character string specifying the path to the Excel file if `save_column_in_each_nppes_year` is TRUE. Default is NULL.
#' @return A data frame containing the queried sample data.
#' @examples
#' con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
#' nppes_query_sample_data(con, "npi_2024", 5)
#' # Next step: nppes_process_npi_table_chunk()
#'
#' con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
#' nppes_query_sample_data(con, "npi_2024", 10, TRUE, "/path/to/excel_file.xlsx")
#' # Next step: nppes_process_npi_table_chunk()
nppes_query_sample_data <- function(con, table_name = "npi_2024", limit = 5, save_column_in_each_nppes_year = TRUE, excel_file_path = NULL) {
  base::cat("Querying sample data from table", table_name, "with limit", limit, "...\n")

  query <- base::sprintf("SELECT * FROM %s LIMIT %d", table_name, limit)
  result <- DBI::dbGetQuery(con, query)

  base::cat("Sample data query successful. Retrieved", nrow(result), "rows.\n")
  base::print(result)

  if (save_column_in_each_nppes_year && !is.null(excel_file_path)) {
    base::cat("Saving sample data to Excel file at", excel_file_path, "...\n")
    writexl::write_xlsx(result, excel_file_path)
    base::cat("Sample data saved to Excel file at", excel_file_path, "successfully.\n")
  }

  base::cat("Next step: nppes_process_npi_table_chunk()\n")
  return(result)
}
