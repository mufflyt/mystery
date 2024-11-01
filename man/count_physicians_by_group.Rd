% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_section_count_physicians_by_group.R
\name{count_physicians_by_group}
\alias{count_physicians_by_group}
\title{Count Physicians by State or Subdivision}
\usage{
count_physicians_by_group(
  data,
  state_name_column = "state_code",
  phone_column = "phone_number",
  first_name_column = "first",
  last_name_column = "last",
  group_by = "state",
  output_to_csv = NULL
)
}
\arguments{
\item{data}{A dataframe containing the physician data.}

\item{state_name_column}{The column name in the dataframe containing state names or abbreviations (default is "state_code").}

\item{phone_column}{The column name in the dataframe containing phone numbers (default is "phone_number").}

\item{first_name_column}{The column name in the dataframe containing first names (default is "first").}

\item{last_name_column}{The column name in the dataframe containing last names (default is "last").}

\item{group_by}{A string indicating whether to group by "state" or "subdivision" (default is "state").}

\item{output_to_csv}{(Optional) A file path to save the state/subdivision counts to a CSV file. If NULL, no file is saved.}
}
\value{
A tibble with the counts of physicians per state or subdivision.
}
\description{
This function counts the number of physicians available per state or US Census Bureau subdivision.
It can optionally save the counts to a CSV file.
}
\examples{
# Example 1: Count physicians by state
count_physicians_by_group(taxonomy_and_aaos_data)

# Example 2: Count physicians by U.S. Census Bureau subdivision
count_physicians_by_group(taxonomy_and_aaos_data, group_by = "subdivision")

}
