#' Create a Line Plot with Optional Transformations and Grouping
#'
#' This function creates a line plot using ggplot2 with options for transforming the y-axis, grouping lines,
#' and saving the plot with a specified resolution. The plot can be saved in both TIFF and PNG formats with
#' automatic filename generation.
#'
#' @param data A dataframe containing the data to be plotted. Must include the variables specified in `x_var`
#'   and `y_var`.
#' @param x_var A string representing the column name for the x-axis variable. This should be a categorical or
#'   factor variable.
#' @param y_var A string representing the column name for the y-axis variable. This should be a numeric variable.
#' @param y_transform A string specifying the transformation for the y-axis: "log" for log transformation (log1p),
#'   "sqrt" for square root transformation, or "none" for no transformation. Default is "none".
#' @param dpi An integer specifying the resolution of the saved plot in dots per inch (DPI). Default is 100.
#' @param output_dir A string representing the directory where the plot files will be saved. Default is "output".
#' @param file_prefix A string used as the prefix for the generated plot filenames. The filenames will have a
#'   timestamp appended to ensure uniqueness. Default is "line_plot".
#' @param use_geom_line A boolean indicating whether to include lines connecting points for grouped data.
#'   Default is FALSE.
#' @param geom_line_group A string representing the column name to group the lines by when `use_geom_line` is TRUE.
#'   This should be a categorical or factor variable.
#' @param point_color A string specifying the color of the points. Default is "viridis", which uses the viridis
#'   color palette.
#' @param line_color A string specifying the color of the summary line (median). Default is "red".
#' @param verbose A boolean indicating whether to print messages about the saved plot locations. Default is TRUE.
#'
#' @return This function saves the plot to the specified directory and returns the ggplot object.
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot geom_point geom_line stat_summary ylab theme_minimal element_rect element_blank
#' @importFrom ggplot2 ggsave
#' @importFrom viridis viridis_pal
#' @importFrom rlang sym
#' @import assertthat
#' @export
#'
#' @examples
#' # Example 1: Basic line plot with no transformations
#' create_line_plot(
#'   data = iris,
#'   x_var = "Species",
#'   y_var = "Sepal.Length",
#'   y_transform = "none",
#'   dpi = 100,
#'   output_dir = "output",
#'   file_prefix = "iris_sepal_length"
#' )
#'
#' # Example 2: Line plot with log transformation and grouped lines
#' create_line_plot(
#'   data = mtcars,
#'   x_var = "cyl",
#'   y_var = "mpg",
#'   y_transform = "log",
#'   dpi = 150,
#'   output_dir = "plots",
#'   file_prefix = "mtcars_log_mpg",
#'   use_geom_line = TRUE,
#'   geom_line_group = "gear"
#' )
#'
#' # Example 3: Line plot with square root transformation and customized aesthetics
#' create_line_plot(
#'   data = mtcars,
#'   x_var = "gear",
#'   y_var = "hp",
#'   y_transform = "sqrt",
#'   dpi = 300,
#'   output_dir = "custom_plots",
#'   file_prefix = "mtcars_sqrt_hp",
#'   point_color = "blue",
#'   line_color = "green",
#'   verbose = TRUE
#' )
create_line_plot <- function(data,
                             x_var,
                             y_var,
                             y_transform = "none",
                             dpi = 100,
                             output_dir = "output",
                             file_prefix = "line_plot",
                             use_geom_line = FALSE,
                             geom_line_group = NULL,
                             point_color = "viridis",
                             line_color = "red",
                             verbose = TRUE) {
  # Validate inputs using assertthat
  assertthat::assert_that(is.data.frame(data), msg = "`data` must be a data frame.")
  assertthat::assert_that(assertthat::is.string(x_var), msg = "`x_var` must be a string.")
  assertthat::assert_that(assertthat::is.string(y_var), msg = "`y_var` must be a string.")
  assertthat::assert_that(x_var %in% colnames(data), msg = paste0("Column `", x_var, "` not found in `data`."))
  assertthat::assert_that(y_var %in% colnames(data), msg = paste0("Column `", y_var, "` not found in `data`."))
  assertthat::assert_that(y_transform %in% c("none", "log", "sqrt"), msg = "`y_transform` must be one of 'none', 'log', or 'sqrt'.")
  assertthat::assert_that(assertthat::is.string(output_dir), msg = "`output_dir` must be a string.")
  assertthat::assert_that(is.numeric(dpi) && dpi > 0, msg = "`dpi` must be a positive numeric value.")
  assertthat::assert_that(is.logical(use_geom_line), msg = "`use_geom_line` must be a logical value.")
  if (use_geom_line) {
    assertthat::assert_that(!is.null(geom_line_group), msg = "`geom_line_group` must be specified when `use_geom_line` is TRUE.")
    assertthat::assert_that(geom_line_group %in% colnames(data), msg = paste0("Column `", geom_line_group, "` not found in `data`."))
  }

  # Remove NA values from the y_var column
  data <- dplyr::filter(data, !is.na(.data[[y_var]]))

  # Handle transformations
  if (y_transform == "log") {
    data <- dplyr::mutate(data, !!rlang::sym(y_var) := log1p(.data[[y_var]]))
    y_label <- paste("Log (", y_var, ")", sep = "")
  } else if (y_transform == "sqrt") {
    data <- dplyr::mutate(data, !!rlang::sym(y_var) := sqrt(.data[[y_var]]))
    y_label <- paste("Sqrt (", y_var, ")", sep = "")
  } else {
    y_label <- y_var
  }

  # Create the plot
  line_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
    ggplot2::geom_point(color = ifelse(point_color == "viridis", viridis::viridis_pal()(1), point_color))

  if (use_geom_line & !is.null(geom_line_group)) {
    line_plot <- line_plot +
      ggplot2::geom_line(ggplot2::aes_string(group = geom_line_group), color = "gray80")
  }

  line_plot <- line_plot +
    ggplot2::stat_summary(fun = median, geom = "line", color = line_color, alpha = 0.7, size = 1, ggplot2::aes(group = 1)) +
    ggplot2::ylab(y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      axis.title.x = ggplot2::element_blank()
    )

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) {
      message("Created output directory: ", output_dir)
    }
  }

  # Automatic Filename Generation
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tiff_filename <- file.path(output_dir, paste0(file_prefix, "_", timestamp, ".tiff"))
  png_filename <- file.path(output_dir, paste0(file_prefix, "_", timestamp, ".png"))

  # Save the plot
  ggplot2::ggsave(filename = tiff_filename, plot = line_plot, dpi = dpi, height = 8, width = 11)
  ggplot2::ggsave(filename = png_filename, plot = line_plot, dpi = dpi, height = 8, width = 11)

  if (verbose) {
    message("Plots saved to: ", tiff_filename, " and ", png_filename)
  }

  return(line_plot)
}
