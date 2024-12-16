#' Create and Plot Interaction Effects in GLMM
#'
#' This function reads data from a specified file, fits a generalized linear mixed model (GLMM) with a specified
#' interaction term, and creates a plot to visualize the interaction effects. The plot is saved to a specified directory.
#'
#' @param data_path A character string specifying the path to the .rds file containing the dataset.
#' @param response_variable A character string specifying the name of the response variable in the dataset.
#' @param variable_of_interest A character string specifying the first categorical predictor variable in the interaction.
#' @param interaction_variable A character string specifying the second categorical predictor variable in the interaction.
#' @param random_intercept A character string specifying the variable to be used as the random intercept in the model (e.g., "city").
#' @param output_path A character string specifying the directory where the interaction plot will be saved.
#' @param resolution An integer specifying the resolution (in DPI) for saving the plot. Defaults to 100.
#'
#' @return A list containing the fitted GLMM (`model`) and the summarized data used for the effects plot (`effects_plot_data`).
#'
#' @examples
#' \dontrun{
#' result <- create_and_plot_interaction(
#'   data_path = "data/phase2_analysis.rds",
#'   response_variable = "business_days_until_appointment",
#'   variable_of_interest = "appointment_center",
#'   interaction_variable = "gender",
#'   random_intercept = "city",
#'   output_path = "results/figures",
#'   resolution = 100
#' )
#' }
#' @importFrom lme4 glmer
#' @importFrom dplyr rename mutate group_by summarise
#' @importFrom ggplot2 ggsave ggplot aes geom_point geom_line labs theme_minimal
#' @importFrom assertthat assert_that is.string is.number is.dir
#' @export
create_and_plot_interaction <- function(data_path, response_variable, variable_of_interest, interaction_variable, random_intercept, output_path, resolution = 100) {
  # Validate inputs using assertthat
  assertthat::assert_that(assertthat::is.string(data_path) && file.exists(data_path),
                          msg = "`data_path` must be a valid file path.")
  assertthat::assert_that(assertthat::is.string(response_variable),
                          msg = "`response_variable` must be a non-empty character string.")
  assertthat::assert_that(assertthat::is.string(variable_of_interest),
                          msg = "`variable_of_interest` must be a non-empty character string.")
  assertthat::assert_that(assertthat::is.string(interaction_variable),
                          msg = "`interaction_variable` must be a non-empty character string.")
  assertthat::assert_that(assertthat::is.string(random_intercept),
                          msg = "`random_intercept` must be a non-empty character string.")
  assertthat::assert_that(assertthat::is.dir(output_path),
                          msg = "`output_path` must be a valid directory.")
  assertthat::assert_that(assertthat::is.number(resolution) && resolution > 0,
                          msg = "`resolution` must be a positive number.")

  # Read the data
  data <- readRDS(data_path)
  assertthat::assert_that(is.data.frame(data), msg = "`data` must be a data frame.")

  # Check if required columns exist
  required_columns <- c(response_variable, variable_of_interest, interaction_variable, random_intercept)
  missing_columns <- setdiff(required_columns, colnames(data))
  assertthat::assert_that(length(missing_columns) == 0,
                          msg = paste("The following required columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))

  # Rename and transform columns
  data <- data %>%
    dplyr::rename(
      response_var = !!rlang::sym(response_variable),
      var_interest = !!rlang::sym(variable_of_interest),
      int_var = !!rlang::sym(interaction_variable)
    ) %>%
    dplyr::mutate(
      response_var = as.numeric(response_var),
      var_interest = as.factor(var_interest),
      int_var = as.factor(int_var)
    )

  # Remove rows with NA values
  data <- stats::na.omit(data)

  # Construct and fit the model
  model_formula <- as.formula(paste("response_var ~ int_var * var_interest + (1 |", random_intercept, ")"))
  glmer_model <- lme4::glmer(model_formula, data = data, family = poisson(link = "log"))

  # Summarize interaction effects for plotting
  pred_data <- data %>%
    dplyr::mutate(pred = predict(glmer_model, type = "response")) %>%
    dplyr::group_by(int_var, var_interest) %>%
    dplyr::summarise(mean_pred = mean(pred), .groups = "drop")

  # Create and save the plot
  plot <- ggplot(pred_data, aes(x = int_var, y = mean_pred, color = var_interest)) +
    geom_point() +
    geom_line(aes(group = var_interest)) +
    labs(title = "Interaction Effect Plot", y = response_variable, x = interaction_variable) +
    theme_minimal()

  plot_filename <- file.path(output_path, paste0("interaction_", variable_of_interest, "_", interaction_variable, ".png"))
  ggplot2::ggsave(plot_filename, plot, width = 6, height = 4, dpi = resolution)

  # Return results
  return(list(model = glmer_model, effects_plot_data = pred_data))
}
