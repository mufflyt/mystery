#' Connect to DuckDB
#'
#' This function establishes a connection to a DuckDB database.
#'
#' @param duckdb_file_path The file path to the DuckDB database file.
#' @return A DuckDB connection object.
#' @export
#'
#' @examples
#' con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
nppes_connect_to_duckdb <- function(duckdb_file_path) {
  base::cat("Connecting to DuckDB at", duckdb_file_path, "...\n")

  # Establish connection
  con <- DBI::dbConnect(duckdb::duckdb(), duckdb_file_path)

  base::cat("Connected to DuckDB.\n")
  base::cat("Next step: tyler::nppes_create_table_from_csv()\n")

  return(con)
}
