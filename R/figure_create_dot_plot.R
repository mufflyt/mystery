#' Create a Dot Plot with Error Bars for Mystery Caller Studies with Logging and Error Handling
#'
#' This function generates a dot plot for visualizing median values with error bars across different categories, such as insurance types.
#' It includes error handling, meaningful variable names, and default behaviors for ease of use. Extensive logging tracks inputs, transformations, and outputs.
#'
#' @param dataset A dataframe containing the data to be plotted. Must contain the variables specified in `category_var`, `value_var`, `lower_bound_var`, and `upper_bound_var`.
#' @param category_var A string representing the column name for the categorical variable on the x-axis (e.g., insurance type).
#' @param value_var A string representing the column name for the numeric variable on the y-axis (e.g., median days).
#' @param lower_bound_var A string representing the column name for the lower bound of the error bars (e.g., first quartile).
#' @param upper_bound_var A string representing the column name for the upper bound of the error bars (e.g., third quartile).
#' @param dpi An integer specifying the resolution of the saved plot in dots per inch (DPI). Default is 100.
#' @param output_directory A string representing the directory where the plot files will be saved. Default is "output".
#' @param filename_prefix A string used as the prefix for the generated plot filenames. The filenames will have a timestamp appended for uniqueness.
#' @param x_label A string specifying the label for the x-axis. Default is `NULL` (uses `category_var`).
#' @param y_label A string specifying the label for the y-axis. Default is `NULL` (uses `value_var`).
#' @param plot_title A string specifying the title of the plot. Default is `NULL` (no title).
#' @param verbose A boolean indicating whether to print messages about the saved plot locations. Default is TRUE.
#'
#' @return This function displays the plot and saves it to the specified directory.
#' @importFrom assertthat assert_that is.string is.dir is.flag has_name
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar labs theme_minimal theme element_text ggsave
#' @export
#'
#' @examples
#' create_dot_plot(
#'   dataset = df_plot,
#'   category_var = "insurance",
#'   value_var = "median_days",
#'   lower_bound_var = "q1",
#'   upper_bound_var = "q3",
#'   dpi = 100,
#'   output_directory = "output/plots",
#'   filename_prefix = "insurance_vs_days",
#'   x_label = "Insurance",
#'   y_label = "Median Business Days",
#'   plot_title = "Comparison of Business Days by Insurance"
#' )
create_dot_plot <- function(dataset,
                            category_var,
                            value_var = "median_days",
                            lower_bound_var = "q1",
                            upper_bound_var = "q3",
                            dpi = 100,
                            output_directory = "output",
                            filename_prefix = "dot_plot",
                            x_label = NULL,
                            y_label = NULL,
                            plot_title = NULL,
                            verbose = TRUE) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.data.frame(dataset), msg = "`dataset` must be a data frame.")
  assertthat::assert_that(assertthat::is.string(category_var), msg = "`category_var` must be a string.")
  assertthat::assert_that(assertthat::is.string(value_var), msg = "`value_var` must be a string.")
  assertthat::assert_that(assertthat::is.string(lower_bound_var), msg = "`lower_bound_var` must be a string.")
  assertthat::assert_that(assertthat::is.string(upper_bound_var), msg = "`upper_bound_var` must be a string.")
  assertthat::assert_that(assertthat::is.dir(output_directory), msg = paste0("`", output_directory, "` does not exist."))
  assertthat::assert_that(assertthat::is.flag(verbose), msg = "`verbose` must be a logical value (TRUE/FALSE).")

  # Ensure required columns exist in dataset
  required_vars <- c(category_var, value_var, lower_bound_var, upper_bound_var)
  missing_vars <- setdiff(required_vars, names(dataset))
  assertthat::assert_that(length(missing_vars) == 0, msg = paste("The following columns are missing from the dataset:", paste(missing_vars, collapse = ", ")))

  # Set default axis labels if not provided
  x_label <- if (is.null(x_label)) category_var else x_label
  y_label <- if (is.null(y_label)) value_var else y_label

  # Summarize data for plotting
  logger::log_info("Summarizing data for plotting...")
  plot_data <- dataset %>%
    dplyr::group_by(.data[[category_var]]) %>%
    dplyr::summarise(
      median_value = median(.data[[value_var]], na.rm = TRUE),
      lower_bound = quantile(.data[[value_var]], 0.25, na.rm = TRUE),
      upper_bound = quantile(.data[[value_var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  # Create dot plot
  logger::log_info("Creating dot plot...")
  dot_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[category_var]], y = .data[["median_value"]])) +
    ggplot2::geom_point(size = 4, color = "blue") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["lower_bound"]], ymax = .data[["upper_bound"]]), width = 0.2, color = "black") +
    ggplot2::labs(title = plot_title, x = x_label, y = y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  # Display the plot if verbose
  if (verbose) {
    print(dot_plot)
  }

  # Generate filenames
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tiff_filename <- file.path(output_directory, paste0(filename_prefix, "_", timestamp, ".tiff"))
  png_filename <- file.path(output_directory, paste0(filename_prefix, "_", timestamp, ".png"))

  # Save the plot
  logger::log_info(paste("Saving plot to:", tiff_filename, "and", png_filename))
  ggplot2::ggsave(filename = tiff_filename, plot = dot_plot, dpi = dpi, width = 8, height = 6)
  ggplot2::ggsave(filename = png_filename, plot = dot_plot, dpi = dpi, width = 8, height = 6)

  # Log completion
  logger::log_info("Dot plot created and saved successfully.")

  # Return the plot
  return(dot_plot)
}
