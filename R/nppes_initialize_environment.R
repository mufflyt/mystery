#' Initialize the environment by loading necessary libraries and resolving conflicts
#'
#' This function sets up the environment by sourcing the setup script and resolving package conflicts.
#'
#' @details After initializing the environment, the next step is to connect to DuckDB using `connect_to_duckdb()`.
#'
#' @return None
#' @examples
#' nppes_initialize_environment()
#' # Next step: tyler::connect_to_duckdb()
nppes_initialize_environment <- function(setup_file) {
  base::cat("Initializing environment...\n")
  base::source(setup_file)

  # Resolve conflicts between packages
  conflicted::conflict_prefer("filter", "duckplyr")
  conflicted::conflict_prefer("year", "lubridate")
  conflicted::conflicts_prefer(exploratory::str_detect)
  conflicted::conflicts_prefer(exploratory::str_remove_all)
  conflicted::conflicts_prefer(dplyr::recode)

  base::cat("Environment initialized.\n")
  base::cat("Next step: tyler::connect_to_duckdb()\n")
}
