% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_section_physician_age.R
\name{physician_age}
\alias{physician_age}
\title{Calculate and Summarize Physician Age}
\usage{
physician_age(df, age_column)
}
\arguments{
\item{df}{A data frame containing the age data.}

\item{age_column}{A character string representing the name of the column in \code{df} that contains the age data.}
}
\value{
A character string summarizing the median age and IQR of the specified age column in the dataset.
}
\description{
This function calculates the median age, as well as the 25th and 75th percentiles (Interquartile Range, IQR) of a specified age column in a data frame. It returns a sentence summarizing these statistics.
}
\details{
The function calculates the median, 25th percentile (Q1), and 75th percentile (Q3) of the age data, rounding the results to two decimal places for the median and one decimal place for the percentiles. It then constructs a summary sentence describing these statistics.
}
\examples{
# Example 1: Basic usage with a small dataset
df <- data.frame(age = c(30, 40, 50, 60, 35, 45, 55, 65))
summary_sentence <- physician_age(df, "age")
print(summary_sentence)

# Example 2: Handling missing data
df_with_na <- data.frame(age = c(30, 40, NA, 60, 35, NA, 55, 65))
summary_sentence <- physician_age(df_with_na, "age")
print(summary_sentence)

# Example 3: Different age distribution
df_large <- data.frame(age = c(rep(30, 70), rep(40, 30), rep(50, 20), rep(60, 10)))
summary_sentence <- physician_age(df_large, "age")
print(summary_sentence)

}
