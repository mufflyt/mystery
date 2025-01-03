#' Create a Histogram Plot with Logging and Validation
#'
#' This function generates a histogram plot with optional faceting and includes the total sample size
#' in the plot title. It logs all inputs, outputs, data transformations, and file paths to the console.
#'
#' @param df A data frame containing the data to be plotted. The data should include the columns
#'   specified by `x_var` and `facet_var`.
#' @param x_var A string specifying the name of the variable to plot on the x-axis.
#' @param facet_var A string specifying the name of the variable for faceting. Each unique value in
#'   this variable creates a separate facet.
#' @param binwidth A numeric value specifying the width of the histogram bins. Default is 1.
#' @param title A string specifying the title of the plot. The total sample size will be appended
#'   automatically to this title.
#' @param x_label A string specifying the label for the x-axis. Default is an empty string.
#' @param y_label A string specifying the label for the y-axis. Default is "Count".
#' @param output_file Optional. A string specifying the file path to save the plot as an image. If
#'   NULL, the plot is not saved. Default is NULL.
#'
#' @return A ggplot object representing the histogram. The plot is also saved to the specified
#'   `output_file` if provided.
#'
#' @importFrom ggplot2 ggplot aes_string geom_histogram facet_wrap ggtitle xlab ylab theme_light
#' @importFrom ggplot2 theme element_text element_rect ggsave
#' @importFrom stats as.formula
#' @importFrom logger log_info
#' @importFrom glue glue
#' @importFrom assertthat assert_that is.string has_name
#' @export
create_histogram_plot <- function(df, x_var, facet_var, binwidth = 1,
                                  title = "", x_label = "", y_label = "Count",
                                  output_file = NULL) {
  # Log inputs
  logger::log_info("Creating a histogram plot with the following inputs:")
  logger::log_info("  x_var: {x_var}")
  logger::log_info("  facet_var: {facet_var}")
  logger::log_info("  binwidth: {binwidth}")
  logger::log_info("  title: {title}")
  logger::log_info("  x_label: {x_label}")
  logger::log_info("  y_label: {y_label}")
  if (!is.null(output_file)) {
    logger::log_info("  output_file: {output_file}")
  }

  # Validate inputs using assertthat
  assertthat::assert_that(is.data.frame(df), msg = "The input `df` must be a data frame.")
  assertthat::assert_that(assertthat::is.string(x_var), msg = "`x_var` must be a string.")
  assertthat::assert_that(assertthat::is.string(facet_var), msg = "`facet_var` must be a string.")
  assertthat::assert_that(assertthat::has_name(df, x_var),
    msg = glue::glue("The variable '{x_var}' is not found in the data frame.")
  )
  assertthat::assert_that(assertthat::has_name(df, facet_var),
    msg = glue::glue("The variable '{facet_var}' is not found in the data frame.")
  )

  # Log data transformation
  total_n <- nrow(df)
  logger::log_info("Calculated total sample size (N): {total_n}")

  # Create the plot
  logger::log_info("Generating the histogram plot...")
  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = x_var)) +
    ggplot2::geom_histogram(binwidth = binwidth, color = "black", fill = "skyblue", alpha = 0.7) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var))) +
    ggplot2::ggtitle(glue::glue("{title} (N = {format(total_n, big.mark = ',')})")) +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 12, color = "black"),
      strip.background = ggplot2::element_rect(color = "grey10", fill = "white")
    )
  logger::log_info("Histogram plot created successfully.")

  # Save the plot if output_file is provided
  if (!is.null(output_file)) {
    ggplot2::ggsave(filename = output_file, plot = plot)
    logger::log_info("Plot saved to file: {output_file}")
  }

  # Log output
  logger::log_info("Returning the histogram plot object.")

  return(plot)
}
