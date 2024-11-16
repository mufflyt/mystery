#' Save a ggplot Object to a File with Customizable Parameters
#'
#' The `save_plot` function saves a ggplot object to a specified file path with customizable parameters such as width, height, resolution, and more. It also handles directory creation, file format determination, and logs the saving process using the `logger` package.
#'
#' @param plot A `ggplot` object to be saved.
#' @param file_name A character string specifying the name of the file (including extension) to save the plot as.
#' @param save_directory A character string specifying the directory where the plot should be saved.
#' @param plot_width Numeric value specifying the width of the plot. Defaults to `8`.
#' @param plot_height Numeric value specifying the height of the plot. Defaults to `6`.
#' @param resolution_dpi Numeric value specifying the resolution in dots per inch (DPI). Defaults to `300`.
#' @param size_units Character string specifying the units for width and height. Defaults to `"in"` (inches).
#' @param scale_factor Numeric value specifying the scaling factor for the plot. Defaults to `1`.
#' @param file_format A character string specifying the file format to save the plot as (e.g., `"png"`, `"pdf"`). If `NULL`, the format is inferred from the file extension of `file_name`. Defaults to `NULL`.
#' @param background_color Character string specifying the background color of the plot. Defaults to `"white"`.
#' @param verbose_output Logical value indicating whether to print messages to the console upon successful saving. Defaults to `TRUE`.
#'
#' @return This function does not return a value. It performs the side effect of saving the plot to a file.
#'
#' @details
#' The function checks for the necessary packages (`logger`, `ggplot2`, and `tools`) and loads them as required. It ensures that the save directory exists, creating it if necessary. The file format is determined based on the file extension if not explicitly provided. The function logs each step of the process and handles errors gracefully, providing informative messages.
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(ggplot2)
#' library(tyler) # Assuming save_plot is part of the 'tyler' package
#'
#' # Create a sample ggplot
#' sample_plot <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' # Save the plot with default parameters
#' save_plot(
#'   plot = sample_plot,
#'   file_name = "my_plot.png",
#'   save_directory = "plots"
#' )
#'
#' # Save the plot with custom parameters
#' save_plot(
#'   plot = sample_plot,
#'   file_name = "my_plot.pdf",
#'   save_directory = "plots",
#'   plot_width = 10,
#'   plot_height = 8,
#'   resolution_dpi = 600,
#'   background_color = "transparent",
#'   verbose_output = FALSE
#' )
#' }
#'
#' @seealso [ggplot2::ggsave()], [logger::log_info()]
#'
#' @export
save_plot <- function(plot, file_name, save_directory, plot_width = 8, plot_height = 6,
                      resolution_dpi = 300, size_units = "in", scale_factor = 1,
                      file_format = NULL, background_color = "white", verbose_output = TRUE) {

  # Load required packages
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("The logger package is required but is not installed. Please install it via install.packages('logger').")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required but is not installed. Please install it via install.packages('ggplot2').")
  }
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("The tools package is required but is not installed. It is a part of base R, please check your installation.")
  }

  # Log the inputs to the function
  logger::log_info("save_plot function called with arguments: plot, file_name = {file_name}, save_directory = {save_directory}, plot_width = {plot_width}, plot_height = {plot_height}, resolution_dpi = {resolution_dpi}, size_units = {size_units}, scale_factor = {scale_factor}, file_format = {file_format}, background_color = {background_color}, verbose_output = {verbose_output}")

  # Create the output directory if it does not exist
  if (!dir.exists(save_directory)) {
    logger::log_info("Save directory '{save_directory}' does not exist. Creating directory...")
    dir.create(save_directory, recursive = TRUE, showWarnings = FALSE)
    logger::log_info("Directory created at '{save_directory}'")
  } else {
    logger::log_info("Save directory '{save_directory}' already exists.")
  }

  # Construct the full file path for saving the plot
  full_file_path <- file.path(save_directory, file_name)
  logger::log_info("Constructed full file path for saving: {full_file_path}")

  # Determine the device format based on file extension if not provided explicitly
  if (is.null(file_format)) {
    file_extension <- tools::file_ext(file_name)
    logger::log_info("Determining device format based on file extension: {file_extension}")

    # Use a switch statement to determine the file format
    file_format <- switch(
      tolower(file_extension),
      "tiff" = "tiff",
      "jpg" = "jpeg",
      "jpeg" = "jpeg",
      "png" = "png",
      "pdf" = "pdf",
      "eps" = "eps",
      "svg" = "svg",
      {
        logger::log_error("Unsupported file extension: {file_extension}. Supported extensions are tiff, jpg, jpeg, png, pdf, eps, and svg.")
        stop("Unsupported file extension: ", file_extension, ". Supported extensions are tiff, jpg, jpeg, png, pdf, eps, and svg.")
      }
    )
    logger::log_info("Determined file format: {file_format}")
  } else {
    logger::log_info("File format provided explicitly: {file_format}")
  }

  # Log plot saving action
  logger::log_info("Saving plot with the following parameters: file path = {full_file_path}, width = {plot_width}, height = {plot_height}, dpi = {resolution_dpi}, units = {size_units}, scale = {scale_factor}, background color = {background_color}")

  # Save the plot using ggplot2::ggsave with error handling
  tryCatch({
    ggplot2::ggsave(filename = full_file_path, plot = plot, width = plot_width, height = plot_height,
                    dpi = resolution_dpi, units = size_units, scale = scale_factor,
                    device = file_format, bg = background_color)
    logger::log_info("Plot successfully saved at: {full_file_path}")

    # Print message indicating where the file has been saved, if verbose_output is TRUE
    if (verbose_output) {
      message("Plot saved at: ", full_file_path)
    }
  }, error = function(e) {
    logger::log_error("Failed to save plot. Error: {e$message}")
    stop("Failed to save plot: ", e$message)
  })

  # Log the output of the function
  logger::log_info("save_plot function execution completed. Plot saved at: {full_file_path}")
}
