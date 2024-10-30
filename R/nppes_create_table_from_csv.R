#' Create a table in DuckDB from a CSV file
#'
#' This function creates a table in DuckDB by reading data from a CSV file.
#'
#' @details After creating the table, the next step is to query sample data using `nppes_query_sample_data()`.
#'
#' @param con A DuckDB connection object.
#' @param file_path A character string specifying the path to the CSV file.
#' @param table_name A character string specifying the name of the table to be created. Default is "npi_2024".
#' @return None
#' @examples
#' con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
#' nppes_create_table_from_csv(con, "/path/to/your/csv_file.csv", "npi_2024")
#' # Next step: nppes_query_sample_data()
#'
#' con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
#' nppes_create_table_from_csv(con, "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/NPPES_Data.csv", "npi_2024")
#' # Next step: nppes_query_sample_data()
nppes_create_table_from_csv <- function(con, file_path, table_name = "npi_2024") {
  base::cat("Creating table", table_name, "from CSV file at", file_path, "...\n")

  sql_command <- base::sprintf(
    "CREATE TABLE %s AS SELECT * FROM read_csv_auto('%s', header=TRUE, quote='\"', escape='\"', null_padding=true, ignore_errors=true)",
    table_name, file_path
  )

  DBI::dbExecute(con, sql_command)
  base::cat("Table", table_name, "created successfully.\n")
  base::cat("Next step: nppes_query_sample_data()\n")
}
