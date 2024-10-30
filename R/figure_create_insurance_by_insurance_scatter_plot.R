#' Create a Scatter Plot Comparing Waiting Times Between Two Insurance Types
#'
#' This function creates a scatter plot comparing waiting times (in days) to an appointment between two different insurance types.
#' The plot is saved as both a TIFF and PNG file in the specified output directory. The function allows customization of plot aesthetics,
#' including axis labels, point size, and alpha transparency. It logs the process and ensures the output directory exists.
#'
#' @param df A data frame containing the data to be plotted.
#' @param unique_variable A string representing the column name that uniquely identifies each entity in the data (e.g., "phone" or "npi").
#' @param insurance1 A string representing the first insurance type to be compared (default is "medicaid").
#' @param insurance2 A string representing the second insurance type to be compared (default is "blue cross/blue shield").
#' @param output_directory A string specifying the directory where the plot files will be saved. The default is "ortho_sports_med/figures/".
#' @param dpi An integer specifying the resolution of the saved plot files in dots per inch (DPI). The default is 100.
#' @param height A numeric value specifying the height of the saved plot files in inches. The default is 8 inches.
#' @param width A numeric value specifying the width of the saved plot files in inches. The default is 11 inches.
#' @param x_label A string representing the label for the x-axis. The default is "Time in days to appointment Blue Cross Blue Shield (Log Scale)".
#' @param y_label A string representing the label for the y-axis. The default is "Time in days to appointment Medicaid (Log Scale)".
#' @param plot_title A string representing the title of the plot. The default is "Comparison of Waiting Times: Medicaid vs Blue Cross Blue Shield".
#' @param point_size A numeric value specifying the size of the points in the scatter plot. The default is 3.
#' @param point_alpha A numeric value between 0 and 1 specifying the transparency level of the points in the scatter plot. The default is 0.6.
#' @param add_confidence_interval A logical value indicating whether to add a confidence interval around the linear fit line. The default is TRUE.
#'
#' @return A ggplot2 scatter plot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline stat_smooth annotate ylab xlab ggtitle scale_x_log10 scale_y_log10 theme_minimal ggsave
#' @importFrom dplyr mutate select filter
#' @importFrom tidyr spread
#' @importFrom stringr str_trim
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with default values
#' scatterplot <- create_insurance_by_insurance_scatter_plot(
#'   df = df3,  # The data frame to be used
#'   unique_variable = "phone"  # The unique identifier variable
#' )
#'
#' # Example 2: Custom DPI, height, and width for better resolution
#' scatterplot <- create_insurance_by_insurance_scatter_plot(
#'   df = df3,  # The data frame to be used
#'   unique_variable = "phone",  # The unique identifier variable
#'   dpi = 300,  # Higher DPI for better resolution
#'   height = 10,  # Custom height for the plot
#'   width = 15  # Custom width for the plot
#' )
#'
#' # Example 3: Customized axis labels, plot title, and transparency settings
#' scatterplot <- create_insurance_by_insurance_scatter_plot(
#'   df = df3,  # The data frame to be used
#'   unique_variable = "npi",  # The unique identifier variable
#'   x_label = "Appointment Time (days) - Blue Cross Blue Shield (Log Scale)",  # Custom x-axis label
#'   y_label = "Appointment Time (days) - Medicaid (Log Scale)",  # Custom y-axis label
#'   plot_title = "Custom Plot Title: Waiting Times Comparison",  # Custom plot title
#'   point_alpha = 0.8  # Custom transparency for points
#' )
#' }
#'
#' @export
create_insurance_by_insurance_scatter_plot <- function(df,
                                                       unique_variable,
                                                       insurance1 = "medicaid",
                                                       insurance2 = "blue cross/blue shield",
                                                       output_directory = "ortho_sports_med/figures/",
                                                       dpi = 100,
                                                       height = 8,
                                                       width = 11,
                                                       x_label = "Time in days to appointment\nBlue Cross Blue Shield (Log Scale)",
                                                       y_label = "Time in days to appointment\nMedicaid (Log Scale)",
                                                       plot_title = "Comparison of Waiting Times: Medicaid vs Blue Cross Blue Shield",
                                                       point_size = 3,
                                                       point_alpha = 0.6,
                                                       add_confidence_interval = TRUE) {
  # Start of the function
  cat("Starting the function create_insurance_by_insurance_scatter_plot\n")

  # Step 1: Clean the insurance variable and spread the dataframe by the insurance types
  cat("Step 1: Cleaning the insurance variable and spreading the dataframe\n")

  temp <- df %>%
    dplyr::mutate(insurance = stringr::str_trim(insurance),  # Remove leading/trailing whitespace
                  insurance = tolower(insurance)) %>%  # Convert to lowercase for consistency
    dplyr::select(!!rlang::sym(unique_variable), insurance, business_days_until_appointment) %>%
    tidyr::spread(key = insurance, value = business_days_until_appointment) %>%
    dplyr::mutate(
      !!rlang::sym(insurance1) := ifelse(!!rlang::sym(insurance1) == 0, 0.01, !!rlang::sym(insurance1)),
      !!rlang::sym(insurance2) := ifelse(!!rlang::sym(insurance2) == 0, 0.01, !!rlang::sym(insurance2))
    ) %>%
    dplyr::filter(
      !is.na(!!rlang::sym(insurance1)),
      !is.na(!!rlang::sym(insurance2)),
      is.finite(!!rlang::sym(insurance1)),
      is.finite(!!rlang::sym(insurance2))
    )  # Ensure no NAs or non-finite values

  cat("Step 1: Data after cleaning and filtering:\n")
  print(head(temp))

  # Step 2: Create the scatterplot
  cat("Step 2: Creating the scatterplot\n")

  scatterplot <- ggplot2::ggplot(temp, ggplot2::aes(x = !!rlang::sym(insurance2), y = !!rlang::sym(insurance1))) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") + # 45-degree line
    ggplot2::stat_smooth(method = "lm", se = add_confidence_interval) +
    ggplot2::annotate(geom = "text", x = 130, y = 160, label = "Y = X", angle = 50, color = "gray50") +
    ggplot2::ylab(y_label) +
    ggplot2::xlab(x_label) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_x_log10(limits = c(1, NA)) +  # Set minimum limit for x-axis to 1
    ggplot2::scale_y_log10(limits = c(1, NA)) +  # Set minimum limit for y-axis to 1
    ggplot2::theme_minimal()

  # Step 3: Ensure the output directory exists
  cat("Step 3: Ensuring the output directory exists\n")

  if (!base::dir.exists(output_directory)) {
    base::dir.create(output_directory, recursive = TRUE)
    cat("Directory created:", output_directory, "\n")
  } else {
    cat("Directory already exists:", output_directory, "\n")
  }

  # Step 4: Save the scatterplot as TIFF and PNG
  cat("Step 4: Saving the scatterplot\n")

  tiff_file <- base::file.path(output_directory, "scatterplot.tiff")
  png_file <- base::file.path(output_directory, "scatterplot.png")

  ggplot2::ggsave(filename = tiff_file, plot = scatterplot, dpi = dpi, height = height, width = width)
  cat("Scatterplot saved as TIFF at:", tiff_file, "\n")

  ggplot2::ggsave(filename = png_file, plot = scatterplot, dpi = dpi, height = height, width = width)
  cat("Scatterplot saved as PNG at:", png_file, "\n")

  # Step 5: Return the scatterplot object
  cat("Function completed successfully. Returning the scatterplot object.\n")
  return(scatterplot)
  print(scatterplot)
}
