% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phase0_genderize_physicians.R
\name{genderize_physicians}
\alias{genderize_physicians}
\title{Genderize Physicians Data with Logging and Error Handling}
\usage{
genderize_physicians(input_csv, output_dir = getwd())
}
\arguments{
\item{input_csv}{A string representing the path to the input CSV file containing physician data.
The CSV should contain a column named 'first_name' for genderization.}

\item{output_dir}{A string representing the directory where the output CSV file will be saved.
The default is the current working directory.}
}
\value{
A tibble with genderized information joined to the original data.
}
\description{
This function reads a CSV file containing physician data, genderizes the first names,
and joins the gender information back to the original data. It logs the entire process,
including inputs, transformations, and outputs, and saves the result to a new CSV file with a timestamp.
}
\examples{
# Example 1: Basic usage with the default output directory
\dontrun{
result <- genderize_physicians("physicians_data.csv")
}

# Example 2: Custom output directory for saving the result
\dontrun{
result <- genderize_physicians("physicians_data.csv", output_dir = "output_directory/")
}

# Example 3: Handling missing first names gracefully
\dontrun{
result <- genderize_physicians("physicians_data_missing_names.csv")
}

# Example 4: Using a CSV without the required 'first_name' column
\dontrun{
result <- genderize_physicians("invalid_data.csv")
}

# Example 5: Saving the output in a different directory with a custom name
\dontrun{
result <- genderize_physicians("physicians_data.csv", output_dir = "custom_output/")
}
}
