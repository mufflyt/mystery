#' Install Missing CRAN Packages
#'
#' This function checks if the specified CRAN packages are installed. If a package is not installed, it will be installed automatically. For packages that are already installed, a message will be printed indicating that they are installed along with their version.
#'
#' @param cran_pkgs A character vector of CRAN package names to check and install if missing.
#'
#' @examples
#' # Example 1: Install ggplot2 and dplyr if missing
#' install_missing_packages(c("ggplot2", "dplyr"))
#'
#' # Example 2: Install multiple packages including data.table and tidyr
#' install_missing_packages(c("data.table", "tidyr", "readr"))
#'
#' # Example 3: Install a large set of packages commonly used in data science
#' install_missing_packages(c("ggplot2", "dplyr", "tidyverse", "caret", "randomForest"))
#'
#' @export
install_missing_packages <- function(cran_pkgs) {
  pkgs_installed <- installed.packages()[, 'Version']

  for (pkg in cran_pkgs) {
    if (!pkg %in% names(pkgs_installed)) {
      cat(pkg, "missing, installing...\n")
      install.packages(pkg, dependencies = TRUE)
    } else {
      cat("Already installed:", pkg, pkgs_installed[pkg], '\n')
    }
  }
}
