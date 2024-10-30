#' Plot and Save Estimated Marginal Means (EMMs) with Error Handling
#'
#' This function computes the estimated marginal means (EMMs) from a model object, creates a plot of the EMMs with confidence intervals, and saves the plot to a specified directory. It includes robust error handling, detailed logging, and default behaviors that should work out of the box.
#'
#' @param model_object A fitted model object from which EMMs are to be computed. This can be a generalized linear model (GLM), linear model, or other suitable models.
#' @param specs A character string or formula specifying the predictor variable(s) for which EMMs are to be computed. For example, this could be treatment groups, scenarios, or demographic variables.
#' @param variable_of_interest A character string specifying the variable to be plotted on the x-axis. Typically, this would be the same as the `specs` parameter.
#' @param color_by A character string specifying the variable used to color the points and error bars. This could be a categorical variable such as gender, insurance type, or academic affiliation.
#' @param output_dir A character string specifying the directory where the plot will be saved. Defaults to "Ari/Figures".
#' @param y_min Minimum value for the y-axis. Defaults to `NULL`, which means it will be calculated automatically from the data.
#' @param y_max Maximum value for the y-axis. Defaults to `NULL`, which means it will be calculated automatically from the data.
#' @return A list containing the estimated marginal means data (`data`) and the ggplot object (`plot`).
#'
#' @details This function logs each data transformation, the inputs, the outputs, and where the output files are saved. It uses the `emmeans` package to compute EMMs and creates the plot using `ggplot2`. The function ensures logging of key steps to assist in debugging and auditing.
#'
#' @importFrom emmeans emmeans
#' @importFrom dplyr as_tibble
#' @importFrom ggplot2 ggplot aes_string geom_point geom_errorbar scale_color_brewer ggtitle xlab ylab theme_minimal theme element_text element_blank position_dodge ggsave margin scale_x_discrete
#' @importFrom stringr str_wrap
#' @importFrom tools toTitleCase
#'
#' @export
plot_and_save_emmeans <- function(model_object, specs, variable_of_interest, color_by, output_dir = "Ari/Figures", y_min = NULL, y_max = NULL) {

  # Logging function inputs
  cat("Logging inputs...\n")
  cat("Model Object: ", class(model_object), "\n")

  # Fixing the logging for 'specs' to handle formulas
  cat("Specs: ", paste(deparse(specs), collapse = " "), "\n")

  cat("Variable of Interest: ", variable_of_interest, "\n")
  cat("Color By: ", color_by, "\n")
  cat("Output Directory: ", output_dir, "\n")
  cat("Y-Axis Min: ", y_min, "\n")
  cat("Y-Axis Max: ", y_max, "\n")

  # Check required packages
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required but not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Package 'tools' is required but not installed.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }

  # Ensure output directory exists, log creation
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Output directory created: ", output_dir, "\n")
  } else {
    cat("Using existing output directory: ", output_dir, "\n")
  }

  # Generate a timestamp for file names
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Compute estimated marginal means and log the process
  cat("Computing estimated marginal means...\n")
  edata <- tryCatch({
    emmeans::emmeans(object = model_object, specs = specs, type = "response") %>%
      dplyr::as_tibble()
  }, error = function(e) {
    stop("Error computing estimated marginal means: ", e$message)
  })

  # Log the retrieved data
  cat("Logging estimated marginal means data...\n")
  print(head(edata))

  # Check range of estimated data, log y_min and y_max settings
  rate_range <- range(c(edata$asymp.LCL, edata$asymp.UCL), na.rm = TRUE)
  cat("Range of estimated marginal means with CIs: ", rate_range, "\n")

  # Set default y_min and y_max if not provided
  if (is.null(y_min)) {
    y_min <- rate_range[1] - 5
    cat("Y-axis min set to: ", y_min, "\n")
  }
  if (is.null(y_max)) {
    y_max <- rate_range[2] + 5
    cat("Y-axis max set to: ", y_max, "\n")
  }

  # Create the plot and log the process
  cat("Creating the plot...\n")
  plot <- ggplot2::ggplot(edata, ggplot2::aes_string(x = variable_of_interest, y = "rate")) +
    ggplot2::geom_point(ggplot2::aes_string(color = color_by), size = 4, stroke = 2,
                        position = ggplot2::position_dodge(width = 0.2)) +
    ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "asymp.LCL", ymax = "asymp.UCL", color = color_by),
                           width = 0.2, position = ggplot2::position_dodge(width = 0.2), size = 1.2) +
    ggplot2::scale_color_brewer(palette = "Set1") +  # Change color palette
    ggplot2::ggtitle(paste("Estimated Marginal Means -", tools::toTitleCase(variable_of_interest))) +
    ggplot2::xlab(tools::toTitleCase(variable_of_interest)) +
    ggplot2::ylab("Business Days Until Appointment (Mean ± 95% CI)") +  # Clearer y-axis label
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1, size = 10, color = "black"),
      axis.title.x = ggplot2::element_text(size = 12),  # Increase x-axis title size
      axis.title.y = ggplot2::element_text(size = 12),  # Increase y-axis title size
      plot.title = ggplot2::element_text(size = 14, face = "bold"),  # Emphasize title
      plot.margin = ggplot2::margin(10, 10, 10, 10),  # Increase plot margins
      legend.position = "none"  # Remove legend as before
    ) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))  # Wrap long labels for better readability

  # Log the creation of the plot
  cat("Plot created successfully.\n")
  print(plot)

  # Define the file name and path for saving the plot
  file_name <- paste0(output_dir, "/interaction_", variable_of_interest, "_comparison_plot_", timestamp, ".png")
  cat("Saving plot to: ", file_name, "\n")

  # Save the plot to the specified directory
  tryCatch({
    ggplot2::ggsave(filename = file_name, plot = plot, width = 10, height = 6, bg = "white")
    cat("Plot saved successfully to: ", file_name, "\n")
  }, error = function(e) {
    stop("Error saving plot: ", e$message)
  })

  # Log the output data and plot
  cat("Returning the estimated data and plot object.\n")

  # Return the data and plot in a named list
  return(list(data = edata, plot = plot))

}
