% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_section_generate_overall_table.R
\name{tm_write2pdf}
\alias{tm_write2pdf}
\title{Generate Overall Table with Error Handling and Logging}
\usage{
tm_write2pdf(object, filename)
}
\arguments{
\item{input_file_path}{A string representing the path to the data file (RDS, CSV, or XLS format).}

\item{output_directory}{A string representing the directory where the output table file will be saved.}

\item{title}{A string specifying the title for the overall table summary (default is "Overall Table Summary").}

\item{selected_columns}{An optional vector of selected columns to include in the table. Default is NULL.}

\item{label_translations}{An optional named list for label translations to use in the table summary.}
}
\value{
Nothing is returned. The function saves the output table as a PDF file.
}
\description{
This function generates an overall table summarizing the demographics of the Table 1 data.
It logs all key steps, including inputs, outputs, data transformations, and file paths. The function
supports RDS, CSV, and XLS file formats and creates an overall summary table saved as a PDF file.
}
\examples{
\dontrun{
# Example 1: Generate the overall table with default settings
generate_overall_table("data/Table1.rds", "output_tables")

# Example 2: Generate the overall table with specific selected columns
generate_overall_table("data/Table1.csv", "output_tables", selected_columns = c("age", "gender"))

# Example 3: Generate the overall table with label translations and title customization
label_translations <- list(age = "Age (years)", gender = "Gender")
generate_overall_table("data/Table1.xlsx", "output_tables", title = "Demographic Summary",
                       label_translations = label_translations)

# Example 4: Generate the table from a CSV file with all columns used
generate_overall_table("data/Table1.csv", "output_tables")

# Example 5: Save the table to a custom directory with a specific title and selected columns
generate_overall_table("data/Table1.rds", "custom_output", title = "Custom Summary",
                       selected_columns = c("age", "income"))
}
}
