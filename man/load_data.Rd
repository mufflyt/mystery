% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{load_data}
\alias{load_data}
\title{Load and Process Data from an RDS File with Robust Logging}
\usage{
load_data(data_dir, file_name, verbose = TRUE)
}
\arguments{
\item{data_dir}{A string specifying the directory where the RDS file is located.}

\item{file_name}{A string specifying the name of the RDS file to load.}

\item{verbose}{A boolean indicating whether to print detailed logs. Default is TRUE.}
}
\value{
A data frame with the 'ID' column renamed to 'id_number'.
}
\description{
This function loads data from an RDS file, renames the 'ID' column to 'id_number', and logs every step of the process.
}
\examples{
# Example: Load data from a specified directory with logging
df <- load_data(data_dir = "data", file_name = "Phase_2.rds", verbose = TRUE)

}
