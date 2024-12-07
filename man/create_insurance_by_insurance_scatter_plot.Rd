% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/figure_create_insurance_by_insurance_scatter_plot.R
\name{create_insurance_by_insurance_scatter_plot}
\alias{create_insurance_by_insurance_scatter_plot}
\title{Create a Scatter Plot Comparing Waiting Times Between Two Insurance Types}
\usage{
create_insurance_by_insurance_scatter_plot(
  df,
  unique_variable,
  insurance1 = "medicaid",
  insurance2 = "blue cross/blue shield",
  output_directory = "ortho_sports_med/figures/",
  dpi = 100,
  height = 8,
  width = 11,
  x_label = "Time in days to appointment\\nBlue Cross Blue Shield (Log Scale)",
  y_label = "Time in days to appointment\\nMedicaid (Log Scale)",
  plot_title = "Comparison of Waiting Times: Medicaid vs Blue Cross Blue Shield",
  point_size = 3,
  point_alpha = 0.6,
  add_confidence_interval = TRUE
)
}
\arguments{
\item{df}{A data frame containing the data to be plotted.}

\item{unique_variable}{A string representing the column name that uniquely identifies each entity in the data (e.g., "phone" or "npi").}

\item{insurance1}{A string representing the first insurance type to be compared (default is "medicaid").}

\item{insurance2}{A string representing the second insurance type to be compared (default is "blue cross/blue shield").}

\item{output_directory}{A string specifying the directory where the plot files will be saved. The default is "ortho_sports_med/figures/".}

\item{dpi}{An integer specifying the resolution of the saved plot files in dots per inch (DPI). The default is 100.}

\item{height}{A numeric value specifying the height of the saved plot files in inches. The default is 8 inches.}

\item{width}{A numeric value specifying the width of the saved plot files in inches. The default is 11 inches.}

\item{x_label}{A string representing the label for the x-axis. The default is "Time in days to appointment Blue Cross Blue Shield (Log Scale)".}

\item{y_label}{A string representing the label for the y-axis. The default is "Time in days to appointment Medicaid (Log Scale)".}

\item{plot_title}{A string representing the title of the plot. The default is "Comparison of Waiting Times: Medicaid vs Blue Cross Blue Shield".}

\item{point_size}{A numeric value specifying the size of the points in the scatter plot. The default is 3.}

\item{point_alpha}{A numeric value between 0 and 1 specifying the transparency level of the points in the scatter plot. The default is 0.6.}

\item{add_confidence_interval}{A logical value indicating whether to add a confidence interval around the linear fit line. The default is TRUE.}
}
\value{
A ggplot2 scatter plot object.
}
\description{
This function creates a scatter plot comparing waiting times (in days) to an appointment between two different insurance types.
The plot is saved as both a TIFF and PNG file in the specified output directory. The function allows customization of plot aesthetics,
including axis labels, point size, and alpha transparency. It logs the process and ensures the output directory exists.
}
\examples{
\dontrun{
# Example 1: Basic usage with default values
scatterplot <- create_insurance_by_insurance_scatter_plot(
  df = df3,  # The data frame to be used
  unique_variable = "phone"  # The unique identifier variable
)

# Example 2: Custom DPI, height, and width for better resolution
scatterplot <- create_insurance_by_insurance_scatter_plot(
  df = df3,  # The data frame to be used
  unique_variable = "phone",  # The unique identifier variable
  dpi = 300,  # Higher DPI for better resolution
  height = 10,  # Custom height for the plot
  width = 15  # Custom width for the plot
)

# Example 3: Customized axis labels, plot title, and transparency settings
scatterplot <- create_insurance_by_insurance_scatter_plot(
  df = df3,  # The data frame to be used
  unique_variable = "npi",  # The unique identifier variable
  x_label = "Appointment Time (days) - Blue Cross Blue Shield (Log Scale)",  # Custom x-axis label
  y_label = "Appointment Time (days) - Medicaid (Log Scale)",  # Custom y-axis label
  plot_title = "Custom Plot Title: Waiting Times Comparison",  # Custom plot title
  point_alpha = 0.8  # Custom transparency for points
)
}

}
