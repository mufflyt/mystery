#' Create a Scatter Plot Comparing Waiting Times Between Two Insurance Types
#'
#' This function creates a scatter plot comparing waiting times (in days) to an appointment between two
#' different insurance types. The plot is saved as both a TIFF and PNG file in the specified output directory.
#' The function allows customization of plot aesthetics, including axis labels, point size, and alpha transparency.
#' It includes options for adding a linear fit with confidence intervals and logs the process to the console.
#'
#' @param df A data frame containing the data to be plotted. Must include `insurance`,
#'   `business_days_until_appointment`, and the variable specified in `unique_variable`.
#' @param unique_variable A string representing the column name that uniquely identifies each entity in the
#'   data (e.g., "phone" or "npi").
#' @param insurance1 A string representing the first insurance type to be compared. Default is "medicaid".
#' @param insurance2 A string representing the second insurance type to be compared.
#'   Default is "blue cross/blue shield".
#' @param output_directory A string specifying the directory where the plot files will be saved.
#'   Default is "output".
#' @param dpi An integer specifying the resolution of the saved plot files in dots per inch (DPI). Default is 100.
#' @param height A numeric value specifying the height of the saved plot files in inches. Default is 8 inches.
#' @param width A numeric value specifying the width of the saved plot files in inches. Default is 11 inches.
#' @param x_label A string representing the label for the x-axis. Default is
#'   "Time in days to appointment\nBlue Cross Blue Shield (Log Scale)".
#' @param y_label A string representing the label for the y-axis. Default is
#'   "Time in days to appointment\nMedicaid (Log Scale)".
#' @param plot_title A string representing the title of the plot. Default is
#'   "Comparison of Waiting Times: Medicaid vs Blue Cross Blue Shield".
#' @param point_size A numeric value specifying the size of the points in the scatter plot. Default is 3.
#' @param point_alpha A numeric value between 0 and 1 specifying the transparency level of the points in the
#'   scatter plot. Default is 0.6.
#' @param add_confidence_interval A logical value indicating whether to add a confidence interval around the
#'   linear fit line. Default is TRUE.
#'
#' @return A ggplot2 scatter plot object.
#'
#' @importFrom dplyr mutate select filter
#' @importFrom stringr str_trim
#' @importFrom tidyr spread
#' @importFrom ggplot2 ggplot aes geom_point geom_abline stat_smooth ylab xlab ggtitle scale_x_log10
#' @importFrom ggplot2 scale_y_log10 theme_minimal ggsave
#' @import assertthat
#' @export
#'
#' @examples
#' # Example 1: Default settings
#' scatterplot <- create_insurance_by_insurance_scatter_plot(
#'   df = df3,  # Input data frame
#'   unique_variable = "phone"  # Unique identifier variable
#' )
#' print(scatterplot)
#'
#' # Example 2: Customized axis labels and output directory
#' scatterplot <- create_insurance_by_insurance_scatter_plot(
#'   df = df3,  # Input data frame
#'   unique_variable = "npi",  # Unique identifier variable
#'   insurance1 = "medicaid",  # First insurance type
#'   insurance2 = "blue cross/blue shield",  # Second insurance type
#'   x_label = "Log Time to Appointment (BCBS)",  # Custom x-axis label
#'   y_label = "Log Time to Appointment (Medicaid)",  # Custom y-axis label
#'   plot_title = "Custom Waiting Times Comparison",  # Custom plot title
#'   output_directory = "custom_figures"  # Custom output directory
#' )
#' print(scatterplot)
#'
#' # Example 3: High-resolution plot with adjusted aesthetics
#' scatterplot <- create_insurance_by_insurance_scatter_plot(
#'   df = df3,  # Input data frame
#'   unique_variable = "phone",  # Unique identifier variable
#'   dpi = 300,  # High resolution
#'   point_size = 4,  # Larger point size
#'   point_alpha = 0.8,  # Less transparency
#'   height = 10,  # Custom height
#'   width = 15  # Custom width
#' )
#' print(scatterplot)
create_insurance_by_insurance_scatter_plot <- function(df,
                                                       unique_variable,
                                                       insurance1 = "medicaid",
                                                       insurance2 = "blue cross/blue shield",
                                                       output_directory = "output",
                                                       dpi = 100,
                                                       height = 8,
                                                       width = 11,
                                                       x_label = "Time in days to appointment\nBlue Cross Blue Shield (Log Scale)",
                                                       y_label = "Time in days to appointment\nMedicaid (Log Scale)",
                                                       plot_title = "Comparison of Waiting Times: Medicaid vs Blue Cross Blue Shield",
                                                       point_size = 3,
                                                       point_alpha = 0.6,
                                                       add_confidence_interval = TRUE) {
  # Validate inputs
  assertthat::assert_that(is.data.frame(df), msg = "`df` must be a data frame.")
  assertthat::assert_that(unique_variable %in% names(df), msg = "`unique_variable` must be a column in `df`.")
  assertthat::assert_that("insurance" %in% names(df), msg = "`df` must contain the column `insurance`.")
  assertthat::assert_that("business_days_until_appointment" %in% names(df),
                          msg = "`df` must contain the column `business_days_until_appointment`.")
  assertthat::assert_that(dir.exists(output_directory) || dir.create(output_directory, recursive = TRUE),
                          msg = "Failed to create the output directory.")

  # Clean and prepare the data
  temp <- df %>%
    dplyr::mutate(
      insurance = stringr::str_trim(tolower(insurance)),
      business_days_until_appointment = ifelse(business_days_until_appointment == 0, 0.01, business_days_until_appointment)
    ) %>%
    dplyr::select(!!rlang::sym(unique_variable), insurance, business_days_until_appointment) %>%
    tidyr::spread(key = insurance, value = business_days_until_appointment) %>%
    dplyr::filter(!is.na(!!rlang::sym(insurance1)), !is.na(!!rlang::sym(insurance2)))

  # Create the scatter plot
  scatterplot <- ggplot2::ggplot(temp, ggplot2::aes(x = !!rlang::sym(insurance2), y = !!rlang::sym(insurance1))) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
    ggplot2::stat_smooth(method = "lm", se = add_confidence_interval) +
    ggplot2::ylab(y_label) +
    ggplot2::xlab(x_label) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::theme_minimal()

  # Save the scatter plot
  tiff_file <- file.path(output_directory, "scatterplot.tiff")
  png_file <- file.path(output_directory, "scatterplot.png")
  ggplot2::ggsave(filename = tiff_file, plot = scatterplot, dpi = dpi, height = height, width = width)
  ggplot2::ggsave(filename = png_file, plot = scatterplot, dpi = dpi, height = height, width = width)

  return(scatterplot)
}
