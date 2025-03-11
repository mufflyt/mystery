#' Calculate and Interpret Overdispersion Metrics for a Poisson Model
#'
#' @param poisson_model A fitted Poisson model object (e.g., created with `stats::glm` using
#'   `family = poisson`). The function expects a valid Poisson regression model.
#' @return A list with two components:
#'   \item{metrics}{A named vector containing the chi-squared statistic, the ratio, the residual
#'   degrees of freedom, and the p-value.}
#'   \item{interpretation}{A character string providing an interpretation of the overdispersion
#'   results.}
#' @details Overdispersion occurs when the variance is greater than the mean in count data, violating
#' the assumptions of a Poisson regression. This function helps assess whether overdispersion is
#' present in the model and offers guidance for addressing it if detected.
#'
#' The interpretation categorizes overdispersion into four levels:
#' \itemize{
#'   \item \strong{No significant overdispersion}: The model's fit is adequate.
#'   \item \strong{Slight overdispersion}: May not require adjustments.
#'   \item \strong{Mild overdispersion}: A Negative Binomial model could be beneficial.
#'   \item \strong{Significant overdispersion}: Consider using a Negative Binomial model or adding
#'   random effects.
#' }
#'
#' @importFrom stats residuals df.residual pchisq
#' @importFrom logger log_info log_error
#' @examples
#' # Example 1: Basic Poisson Model with InsectSprays dataset
#' poisson_model <- stats::glm(count ~ spray, data = datasets::InsectSprays, family = poisson)
#' overdispersion_result <- overdisp_fun(poisson_model)
#'
#' # Print metrics
#' # print(overdispersion_result$metrics)
#'
#' # Example 2: Poisson Model with simulated count data
#' set.seed(123)
#' data_sim <- data.frame(
#'   y = rpois(100, lambda = 3),
#'   x = runif(100)
#' )
#' poisson_model_sim <- stats::glm(y ~ x, data = data_sim, family = poisson)
#' overdispersion_result_sim <- overdisp_fun(poisson_model_sim)
#'
#' # Print metrics
#' # print(overdispersion_result_sim$metrics)
#'
#' # Print interpretation
#' # print(overdispersion_result_sim$interpretation)
#'
#' # Example 3: Model with significant overdispersion
#' data_overdisp <- data.frame(
#'   y = c(rpois(50, lambda = 5), rpois(50, lambda = 20)),
#'   x = c(rep(0, 50), rep(1, 50))
#' )
#' poisson_model_overdisp <- stats::glm(y ~ x, data = data_overdisp, family = poisson)
#' overdispersion_result_overdisp <- overdisp_fun(poisson_model_overdisp)
#'
#' # Print metrics
#' # print(overdispersion_result_overdisp$metrics)
#'
#' # Print interpretation
#' # print(overdispersion_result_overdisp$interpretation)
#' @export
results_section_poisson_overdispersion_testing <- function(poisson_model) {
  # Log function start
  logger::log_info("Starting overdispersion calculation.")

  # Validate the input model
  validate_model(poisson_model)

  # Calculate overdispersion metrics
  tryCatch(
    {
      rdf <- calculate_residual_df(poisson_model)
      logger::log_info("Residual degrees of freedom: {rdf}")

      pearson_residuals <- calculate_pearson_residuals(poisson_model)
      logger::log_info("Pearson residuals calculated.")

      pearson_chisq <- calculate_pearson_chisq(pearson_residuals)
      logger::log_info("Pearson chi-squared statistic: {pearson_chisq}")

      pearson_ratio <- calculate_ratio(pearson_chisq, rdf)
      logger::log_info("Pearson chi-squared to degrees of freedom ratio: {pearson_ratio}")

      p_value <- calculate_p_value(pearson_chisq, rdf)
      logger::log_info("P-value for overdispersion: {p_value}")

      overdispersion_metrics <- c(
        chisq = pearson_chisq,
        ratio = pearson_ratio,
        rdf = rdf,
        p = p_value
      )

      logger::log_info("Overdispersion metrics calculated successfully.")

      # Interpret the overdispersion metrics
      interpretation <- interpret_overdispersion(overdispersion_metrics)
      logger::log_info("Interpretation: {interpretation}")

      # Create a summary table
      summary_table <- data.frame(
        Metric = c("Chi-squared", "Ratio", "Residual DF", "P-value", "Interpretation"),
        Value = c(
          overdispersion_metrics["chisq"],
          overdispersion_metrics["ratio"],
          overdispersion_metrics["rdf"],
          overdispersion_metrics["p"],
          interpretation
        )
      )
      logger::log_info("Summary table created.")

      # Return results and interpretation
      result <- list(
        metrics = overdispersion_metrics,
        interpretation = interpretation,
        summary_table = summary_table
      )
      return(result)
    },
    error = function(e) {
      logger::log_error("Error in overdispersion calculation: {e$message}")
      stop(e)
    }
  )
}

# Helper functions

# Validate the input model
# @noRd
validate_model <- function(poisson_model) {
  if (!inherits(poisson_model, "glm")) {
    stop("The input model must be a generalized linear model (glm).")
  }
  if (poisson_model$family$family != "poisson") {
    stop("The model must use the Poisson family.")
  }
  logger::log_info("Input model is valid.")
}

# Calculate residual degrees of freedom
# @noRd
calculate_residual_df <- function(model) {
  stats::df.residual(model)
}

# Calculate Pearson residuals
# @noRd
calculate_pearson_residuals <- function(model) {
  stats::residuals(model, type = "pearson")
}

# Calculate Pearson chi-squared statistic
# @noRd
calculate_pearson_chisq <- function(residuals) {
  sum(residuals^2)
}

# Calculate the ratio of the Pearson chi-squared statistic to residual degrees of freedom
# @noRd
calculate_ratio <- function(pearson_chisq, residual_df) {
  pearson_chisq / residual_df
}

# Calculate the p-value for the Pearson chi-squared statistic
# @noRd
calculate_p_value <- function(pearson_chisq, residual_df) {
  stats::pchisq(pearson_chisq, df = residual_df, lower.tail = FALSE)
}

# Interpret the overdispersion results
# @noRd
interpret_overdispersion <- function(overdispersion_metrics) {
  ratio <- overdispersion_metrics["ratio"]
  p_value <- overdispersion_metrics["p"]

  if (p_value < 0.05) {
    if (ratio > 1.5) {
      interpretation <- "Significant overdispersion detected. Consider using a Negative Binomial model or adding random effects to account for overdispersion."
    } else if (ratio > 1.2) {
      interpretation <- "Mild overdispersion detected. A Negative Binomial model might still improve the fit, but it may not be strictly necessary."
    } else {
      interpretation <- "Slight overdispersion detected, but it may not be of practical concern."
    }
  } else {
    interpretation <- "No significant overdispersion detected."
  }

  return(interpretation)
}
