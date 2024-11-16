#' Create a Bar Plot with Total Sample Size in the Title
#'
#' This function generates a bar plot based on a categorical variable and facets the plot by a grouping variable. The plot title automatically includes the total sample size (N). The function also supports custom axis labels, and it returns the plot object for further manipulation or saving.
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
#' @importFrom grDevices png tiff
#' @importFrom utils head
#' @importFrom stats as.formula
#'
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
#'
#' # Example 2: Using default axis labels and auto-generated title
#' create_bar_plot(
#'   input_data = my_data,
#'   category_var = "insurance_type",
#'   grouping_var = "region"
#' )
#'
#' # Example 3: Saving the plot with a custom filename prefix and disabling verbose output
#' create_bar_plot(
#'   input_data = my_data,
#'   category_var = "insurance_type",
#'   grouping_var = "region",
#'   output_directory = "plots",
#'   filename_prefix = "insurance_vs_region",
#'   verbose = FALSE
#' )
create_bar_plot <- function(input_data,
                            category_var,          # Variable for the x-axis (categorical)
                            grouping_var,          # Variable for facet wrap (grouping)
                            title = NULL,          # Title for the plot
                            x_axis_label = NULL,   # Label for x-axis
                            y_axis_label = "Count", # Label for y-axis (default to 'Count')
                            output_directory = "output", # Where to save the plot
                            filename_prefix = "bar_plot", # Prefix for the saved file
                            verbose = TRUE) {

  # Load necessary library
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required but not installed. Please install ggplot2.")
  }

  # Error handling for input validation
  if (!is.data.frame(input_data)) {
    stop("Input data must be a dataframe.")
  }

  if (!category_var %in% names(input_data)) {
    stop(paste("The x-axis variable", category_var, "is not found in the input data."))
  }

  if (!grouping_var %in% names(input_data)) {
    stop(paste("The facet variable", grouping_var, "is not found in the input data."))
  }

  # If title is not provided, generate a default title
  if (is.null(title)) {
    title <- paste("Bar Plot of", category_var, "by", grouping_var)
  }

  # Default x-axis label if not provided
  if (is.null(x_axis_label)) {
    x_axis_label <- category_var
  }

  # Calculate the total sample size
  total_sample_size <- nrow(input_data)

  # Log the input parameters
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
      strip.text = ggplot2::element_text(size = 12, color = "black"),
      strip.background = ggplot2::element_rect(color = "grey10", fill = "white")
    )

  # Log the successful creation of the plot
  if (verbose) {
    message("Bar plot created successfully.")
  }

  # Return the plot object for further use or testing
  return(bar_plot)
}
