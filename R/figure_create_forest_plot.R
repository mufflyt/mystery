#' Create a Forest Plot for Significant Predictors with Logging
#'
#' This function generates a forest plot displaying significant predictors' coefficients and confidence intervals
#' for a Poisson regression model. It logs each step of the process, providing insights into the predictors included,
#' coefficients, and confidence intervals. The forest plot highlights the direction and significance of effects
#' for the analyzed variables.
#'
#' @param df A data frame containing the dataset used for the regression analysis.
#' @param target_variable A string representing the target variable (dependent variable) for the regression model.
#' @param significant_vars A data frame containing significant predictors with their variable names (`Variable`),
#'        coefficients (`Coefficient`), and confidence intervals (`Lower_CI`, `Upper_CI`).
#'
#' @return A `ggplot` object representing the forest plot with coefficients and confidence intervals for each significant predictor.
#'
#' @details
#' The function loops through significant predictors to fit individual Poisson regression models for each predictor
#' against the target variable. It extracts coefficients and confidence intervals, compiles the results, and visualizes
#' them as a forest plot. A vertical dashed red line at `x = 0` indicates no effect.
#'
#' @import ggplot2
#' @importFrom stats glm coef confint as.formula
#' @export
#'
#' @examples
#' # Example 1: Preparing a dataset and identifying significant predictors
#'
#' # Define predictor variables by excluding specific columns
#' predictor_vars <- prepare_dataset(
#'   df,
#'   target_variable = "cleaned_does_the_physician_accept_medicaid_numeric",
#'   excluded_columns = c("does_the_physician_accept_medicaid",
#'                        "cleaned_does_the_physician_accept_medicaid")
#' )
#'
#' # Fit Poisson models for each predictor and extract p-values
#' results <- fit_poisson_models(
#'   df,
#'   target_variable = "cleaned_does_the_physician_accept_medicaid_numeric",
#'   predictor_vars = predictor_vars
#' )
#'
#' # Filter for significant predictors (p-value < 0.2)
#' significant_vars <- results %>%
#'   dplyr::filter(P_Value < 0.2)
#'
#' # Determine directionality of effects for significant predictors
#' significant_vars_with_direction <- determine_direction(
#'   df,
#'   target_variable = "cleaned_does_the_physician_accept_medicaid_numeric",
#'   significant_vars = significant_vars
#' )
#'
#' # Create a forest plot for significant predictors
#' forest_plot <- create_forest_plot(
#'   df,
#'   target_variable = "cleaned_does_the_physician_accept_medicaid_numeric",
#'   significant_vars = significant_vars_with_direction
#' )
#' print(forest_plot)
#'
#' # Example 2: Saving the forest plot to a file
#' ggplot2::ggsave("forest_plot.png", plot = forest_plot, width = 10, height = 6)
#'
#' @keywords visualization regression plot forest-plot
create_forest_plot <- function(df, target_variable, significant_vars) {
  # Logging the start of the process
  cat("Creating forest plot...\n")
  cat("Target Variable:", target_variable, "\n")

  # Initialize an empty data frame to store plot data
  forest_plot_data <- data.frame(
    Variable = character(),
    Coefficient = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each significant variable and fit the model to extract coefficients and confidence intervals
  for (var in significant_vars$Variable) {
    cat("Processing significant variable for forest plot:", var, "\n")

    formula <- stats::as.formula(paste(target_variable, "~", var))
    model <- stats::glm(formula, family = poisson(link = "log"), data = df)
    coef_value <- coef(model)[2]
    conf_int <- stats::confint(model)[2, ]

    # Log the coefficients and confidence intervals
    cat("Coefficient:", coef_value, "Lower CI:", conf_int[1], "Upper CI:", conf_int[2], "\n")

    # Append the results to the forest_plot_data
    forest_plot_data <- rbind(forest_plot_data, data.frame(
      Variable = var,
      Coefficient = coef_value,
      Lower_CI = conf_int[1],
      Upper_CI = conf_int[2]
    ))
  }

  # Create the forest plot using ggplot2
  forest_plot <- ggplot2::ggplot(forest_plot_data, ggplot2::aes(x = Coefficient, y = Variable)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Forest Plot of Significant Predictors\nfor Accepting Medicaid",
                  x = "Log-Odds Coefficient",
                  y = "Predictor Variables") +
    ggplot2::theme_minimal()

  # Log the completion of the forest plot creation
  cat("Forest plot created.\n")

  # Return the plot object
  print(forest_plot)
  return(forest_plot)
}
