#' Disconnect from DuckDB
#'
#' This function disconnects from a DuckDB database connection after all operations are complete.
#'
#' @details Use this function after you have completed all operations involving the DuckDB connection.
#'
#' @param con A DuckDB connection object.
#' @return None
#' @export
#' @examples
#' con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
#' # After all operations, disconnect from the database
#' nppes_disconnect_from_duckdb(con)
#'
#' con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
#' # After all operations, disconnect from the database
#' nppes_disconnect_from_duckdb(con)

nppes_disconnect_from_duckdb <- function(con) {
  base::cat("Disconnecting from DuckDB...\n")
  DBI::dbDisconnect(con)
  base::cat("Disconnected from DuckDB.\n")
  base::cat("Next step: None, process complete.\n")
}
