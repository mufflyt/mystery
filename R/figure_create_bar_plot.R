#' Create a Bar Plot with Total Sample Size in the Title
#'
#' This function generates a bar plot based on a categorical variable and facets the plot by a grouping variable.
#' The plot title automatically includes the total sample size (N). The function also supports custom axis labels,
#' and it returns the plot object for further manipulation or saving.
#'
#' @param input_data A dataframe containing the data to be plotted.
#' @param category_var A string representing the column name for the categorical variable on the x-axis (e.g., insurance type).
#' @param grouping_var A string representing the column name for the facet wrap (grouping variable).
#' @param title A string specifying the title of the plot. Default is `NULL`, and the function will generate a title based on `category_var` and `grouping_var`.
#' @param x_axis_label A string specifying the label for the x-axis. Default is `NULL`, and the function will use the column name for the x-axis variable.
#' @param y_axis_label A string specifying the label for the y-axis. Default is "Count".
#' @param output_directory A string representing the directory where the plot files will be saved. Default is "output".
#' @param filename_prefix A string used as the prefix for the generated plot filenames. Default is "bar_plot".
#' @param verbose A boolean indicating whether to print messages about the saved plot locations. Default is TRUE.
#'
#' @return This function returns the plot object for further manipulation or saving.
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_text facet_wrap after_stat theme_light theme ggsave element_text element_rect
#' @importFrom assertthat assert_that is.string has_name is.dir is.flag
#' @examples
#' # Example 1: Basic usage with a categorical and facet variable
#' create_bar_plot(
#'   input_data = my_data,
#'   category_var = "insurance_type",
#'   grouping_var = "region",
#'   title = "Insurance Type Distribution by Region",
#'   x_axis_label = "Insurance Type",
#'   y_axis_label = "Number of Observations"
#' )
#' @export
create_bar_plot <- function(input_data,
                            category_var,          # Variable for the x-axis (categorical)
                            grouping_var,          # Variable for facet wrap (grouping)
                            title = NULL,          # Title for the plot
                            x_axis_label = NULL,   # Label for x-axis
                            y_axis_label = "Count", # Label for y-axis (default to 'Count')
                            output_directory = "output", # Where to save the plot
                            filename_prefix = "bar_plot", # Prefix for the saved file
                            verbose = TRUE) {
  # Validate inputs using assertthat
  assertthat::assert_that(is.data.frame(input_data), msg = "`input_data` must be a dataframe.")
  assertthat::assert_that(assertthat::is.string(category_var), msg = "`category_var` must be a string.")
  assertthat::assert_that(assertthat::is.string(grouping_var), msg = "`grouping_var` must be a string.")
  assertthat::assert_that(assertthat::has_name(input_data, category_var),
                          msg = paste0("The column `", category_var, "` is not found in the input data."))
  assertthat::assert_that(assertthat::has_name(input_data, grouping_var),
                          msg = paste0("The column `", grouping_var, "` is not found in the input data."))
  assertthat::assert_that(assertthat::is.string(output_directory), msg = "`output_directory` must be a string.")
  assertthat::assert_that(assertthat::is.dir(output_directory),
                          msg = paste0("The directory `", output_directory, "` does not exist."))
  assertthat::assert_that(assertthat::is.string(filename_prefix), msg = "`filename_prefix` must be a string.")
  assertthat::assert_that(assertthat::is.flag(verbose), msg = "`verbose` must be a logical (TRUE/FALSE).")

  # Calculate the total sample size
  total_sample_size <- nrow(input_data)

  # If title is not provided, generate a default title
  if (is.null(title)) {
    title <- paste("Bar Plot of", category_var, "by", grouping_var)
  }

  # Default x-axis label if not provided
  if (is.null(x_axis_label)) {
    x_axis_label <- category_var
  }

  # Log inputs if verbose
  if (verbose) {
    message(paste("Creating bar plot for", category_var, "faceted by", grouping_var))
    message(paste("Total sample size is:", total_sample_size))
  }

  # Create the bar plot
  bar_plot <- ggplot2::ggplot(input_data, ggplot2::aes(x = !!rlang::sym(category_var))) +
    ggplot2::geom_bar(fill = "gray80", color = "black") +  # Bar plot with gray fill and black outline
    ggplot2::geom_text(stat = 'count', ggplot2::aes(label = ggplot2::after_stat(count)), vjust = 1) +  # Display count labels
    ggplot2::facet_wrap(as.formula(paste("~", grouping_var))) +  # Facet by the specified variable
    ggplot2::ggtitle(paste(title, "(N =", format(total_sample_size, big.mark = ","), ")")) +  # Title with total sample size
    ggplot2::xlab(x_axis_label) +
    ggplot2::ylab(y_axis_label) +
    ggplot2::theme_light() +  # Use light theme
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), # Tilt x-axis labels
      strip.text = ggplot2::element_text(size = 12, color = "black"),
      strip.background = ggplot2::element_rect(color = "grey10", fill = "white")
    )

  # Save the plot
  file_path <- file.path(output_directory, paste0(filename_prefix, ".png"))
  ggplot2::ggsave(file_path, bar_plot, width = 8, height = 6, dpi = 300)
  if (verbose) {
    message("Bar plot saved to:", file_path)
  }

  # Return the plot object for further use or testing
  return(bar_plot)
}
