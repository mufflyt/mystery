#' Install Missing CRAN Packages with Version Reporting
#'
#' This function checks for the presence of specified CRAN packages on the user's system.
#' If a package is not already installed, it automatically installs it with all dependencies.
#' For packages that are already installed, the function logs their version numbers to the console.
#'
#' @param cran_pkgs A character vector containing the names of CRAN packages to verify and install if missing.
#'                  Each element should be the name of a package available on CRAN.
#'
#' @details
#' This utility function is designed to ensure that necessary CRAN packages are installed for a project.
#' It uses \code{\link[utils]{installed.packages}} to check for existing packages and \code{\link[utils]{install.packages}}
#' to install any missing packages. For already-installed packages, the function retrieves and displays the version
#' number from the installed package metadata.
#'
#' The function automatically handles package dependencies during installation by passing \code{dependencies = TRUE}
#' to \code{install.packages}.
#'
#' @note
#' This function requires an active internet connection to download and install packages from CRAN.
#' Ensure that your R environment has write permissions to the library path.
#'
#' @examples
#' \dontrun{
#' # Example 1: Ensure ggplot2 and dplyr are installed
#' install_missing_packages(c("ggplot2", "dplyr"))
#'
#' # Example 2: Install a set of commonly used packages for data wrangling and visualization
#' install_missing_packages(c("tidyr", "readr", "ggplot2", "dplyr"))
#'
#' # Example 3: Check for and install machine learning packages
#' install_missing_packages(c("caret", "randomForest", "xgboost"))
#'
#' # Example 4: Use this function to ensure all required packages for a project are installed
#' required_packages <- c("ggplot2", "dplyr", "tidyr", "readr", "caret", "randomForest")
#' install_missing_packages(required_packages)
#' }
#'
#' @importFrom utils install.packages installed.packages
#' @export
install_missing_packages <- function(cran_pkgs) {
  # Retrieve installed packages and their versions
  pkgs_installed <- installed.packages()[, 'Version']

  for (pkg in cran_pkgs) {
    if (!pkg %in% names(pkgs_installed)) {
      # Package is missing, install it
      cat(pkg, "is missing. Installing...\n")
      utils::install.packages(pkg, dependencies = TRUE)
    } else {
      # Package already installed, log version
      cat("Already installed:", pkg, "- Version:", pkgs_installed[pkg], '\n')
    }
  }
}
