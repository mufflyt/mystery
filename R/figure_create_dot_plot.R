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
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar labs theme_minimal theme element_text ggsave
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#' @export
#'
#' @examples
#' create_dot_plot(
#'     dataset = df_plot,
#'     category_var = "insurance",
#'     value_var = "median_days",
#'     lower_bound_var = "q1",
#'     upper_bound_var = "q3",
#'     dpi = 100,
#'     output_directory = "output/plots",
#'     filename_prefix = "insurance_vs_days",
#'     x_label = "Insurance",
#'     y_label = "Median Business Days",
#'     plot_title = "Comparison of Business Days by Insurance"
#' )
create_dot_plot <- function(dataset,
                            category_var,
                            value_var = "median_days",            # Default value var
                            lower_bound_var = "q1",               # Default lower bound
                            upper_bound_var = "q3",               # Default upper bound
                            dpi = 100,
                            output_directory = "output",
                            filename_prefix = "dot_plot",
                            x_label = NULL,
                            y_label = NULL,
                            plot_title = NULL,
                            verbose = TRUE) {

  # Error handling for input validation
  if (!is.data.frame(dataset)) {
    stop("Input dataset must be a dataframe.")
  }

  # Calculate overall statistics across all categories dynamically
  logger::log_info(paste("Calculating overall statistics for all categories in", category_var))

  df_summary <- dataset %>%
    dplyr::group_by(.data[[category_var]]) %>%
    dplyr::summarise(
      median_value = median(.data[[value_var]], na.rm = TRUE),
      q1 = quantile(.data[[value_var]], 0.25, na.rm = TRUE),
      q3 = quantile(.data[[value_var]], 0.75, na.rm = TRUE)
    )

  logger::log_info("Summarized dataset:")
  print(df_summary)

  # Error check for required columns in the summarized dataset
  required_vars <- c(category_var, "median_value", lower_bound_var, upper_bound_var)
  missing_vars <- required_vars[!required_vars %in% names(df_summary)]

  if (length(missing_vars) > 0) {
    stop(paste("The following columns are missing from the summarized dataset:", paste(missing_vars, collapse = ", ")))
  }

  # Set default axis labels if not provided
  x_label <- if (is.null(x_label)) category_var else x_label
  y_label <- if (is.null(y_label)) value_var else y_label

  # Log the axis labels being used
  logger::log_info(paste("Using x_label:", x_label, ", y_label:", y_label))

  # Create the dot plot
  tryCatch({
    dot_plot <- ggplot2::ggplot(df_summary, ggplot2::aes(x = .data[[category_var]], y = .data[["median_value"]], color = .data[[category_var]])) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[lower_bound_var]], ymax = .data[[upper_bound_var]]), width = 0.2) +
      ggplot2::labs(
        title = plot_title,
        x = x_label,
        y = y_label,
        color = x_label
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10)
      )

    # Log the successful creation of the plot
    logger::log_info("Dot plot created successfully.")

    # Display the plot
    print(dot_plot)

    # Generate file names with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    tiff_filename <- file.path(output_directory, paste0(filename_prefix, "_", timestamp, ".tiff"))
    jpg_filename <- file.path(output_directory, paste0(filename_prefix, "_", timestamp, ".jpg"))

    # Log file paths
    logger::log_info(paste("Saving plot as TIFF at:", tiff_filename))
    logger::log_info(paste("Saving plot as JPG at:", jpg_filename))

    # Save the plot as TIFF and JPG
    ggplot2::ggsave(filename = tiff_filename, plot = dot_plot, dpi = dpi, width = 8, height = 6)
    ggplot2::ggsave(filename = jpg_filename, plot = dot_plot, dpi = dpi, width = 8, height = 6)

    # Verbose output to indicate success
    if (verbose) {
      message("Plots saved to: ", tiff_filename, " and ", jpg_filename)
    }

    # Log success
    logger::log_info("Plot saved successfully to the output directory.")
  }, error = function(e) {
    logger::log_error(paste("Error in creating dot plot:", e$message))
    stop(e)
  })

  # Log the completion of the function
  logger::log_info("create_dot_plot function completed.")

  return(dot_plot)
}
