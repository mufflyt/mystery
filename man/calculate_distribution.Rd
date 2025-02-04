% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_distribution.R
\name{calculate_distribution}
\alias{calculate_distribution}
\title{Calculate Demographic Distribution with Robust Logging}
\usage{
calculate_distribution(df, column)
}
\arguments{
\item{df}{A data frame containing the data.}

\item{column}{A string representing the name of the column for which the distribution is calculated.}
}
\value{
A data frame with the count, total, and percentage for each level of the specified column.
}
\description{
This function calculates the distribution of a categorical variable within a data frame,
including counts and percentages. It also logs inputs, outputs, and all transformations
for transparency and debugging purposes.
}
\examples{
df <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female", NA))
result <- calculate_distribution(df, "gender")
print(result)
}
