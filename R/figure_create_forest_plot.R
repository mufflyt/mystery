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
#' @import assertthat
#' @import ggplot2
#' @importFrom stats glm coef confint as.formula
#' @export
#'
#' @examples
#' # Example: Creating a forest plot for significant predictors
#' forest_plot <- create_forest_plot(
#'   df = data,
#'   target_variable = "accepts_medicaid",
#'   significant_vars = significant_predictors
#' )
#' print(forest_plot)
create_forest_plot <- function(df, target_variable, significant_vars) {
  # Validate inputs using assertthat
  assertthat::assert_that(
    assertthat::is.data.frame(df),
    msg = "`df` must be a data frame."
  )
  assertthat::assert_that(
    assertthat::is.string(target_variable),
    msg = "`target_variable` must be a non-empty string."
  )
  assertthat::assert_that(
    target_variable %in% colnames(df),
    msg = paste("`target_variable`", target_variable, "is not a column in the data frame.")
  )
  assertthat::assert_that(
    assertthat::is.data.frame(significant_vars),
    msg = "`significant_vars` must be a data frame."
  )
  required_cols <- c("Variable")
  missing_cols <- setdiff(required_cols, colnames(significant_vars))
  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = paste("The following columns are missing from `significant_vars`:", paste(missing_cols, collapse = ", "))
  )

  # Logging the start of the process
  cat("Creating forest plot...\n")
  cat("Target Variable:", target_variable, "\n")
  cat("Significant Variables:", paste(significant_vars$Variable, collapse = ", "), "\n")

  # Initialize an empty data frame to store plot data
  forest_plot_data <- data.frame(
    Variable = character(),
    Coefficient = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each significant variable
  for (var in significant_vars$Variable) {
    cat("Processing variable:", var, "\n")

    # Validate that the variable exists in the data
    assertthat::assert_that(
      var %in% colnames(df),
      msg = paste("Variable", var, "is not a column in the data frame.")
    )

    # Fit the model
    formula <- stats::as.formula(paste(target_variable, "~", var))
    model <- stats::glm(formula, family = poisson(link = "log"), data = df)

    # Extract coefficients and confidence intervals
    coef_value <- coef(model)[2]
    conf_int <- stats::confint(model)[2, ]

    # Log the extracted values
    cat("Coefficient:", coef_value, "Lower CI:", conf_int[1], "Upper CI:", conf_int[2], "\n")

    # Append the results
    forest_plot_data <- rbind(forest_plot_data, data.frame(
      Variable = var,
      Coefficient = coef_value,
      Lower_CI = conf_int[1],
      Upper_CI = conf_int[2]
    ))
  }

  # Validate that there are results to plot
  assertthat::assert_that(
    nrow(forest_plot_data) > 0,
    msg = "No valid predictors were processed for the forest plot."
  )

  # Create the forest plot
  forest_plot <- ggplot2::ggplot(forest_plot_data, ggplot2::aes(x = Coefficient, y = Variable)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "Forest Plot of Significant Predictors",
      x = "Log-Odds Coefficient",
      y = "Predictor Variables"
    ) +
    ggplot2::theme_minimal()

  # Log the completion of the process
  cat("Forest plot created successfully.\n")

  # Return the plot
  return(forest_plot)
}
