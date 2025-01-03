#' Fit Poisson Models for Analyzing Wait Times
#'
#' This function fits Poisson regression models to analyze the effect of predictors on
#' a target variable, such as wait times for appointments. It returns a summary of
#' predictors and their significance levels (p-values).
#'
#' @param data A dataframe containing the variables for the analysis.
#' @param target_var A string specifying the name of the target variable (e.g., "wait_time").
#' @param predictors A character vector of predictor variable names to include in the models.
#'
#' @return A tibble containing predictors, their p-values, and formatted p-values.
#' @importFrom dplyr tibble bind_rows mutate
#' @importFrom stats glm as.formula
#' @importFrom logger log_info log_error
#'
#' @examples
#' # Example: Fitting Poisson models with wait time data
#' wait_data <- data.frame(
#'   wait_time = rpois(100, lambda = 5),
#'   insurance_type = sample(c("Medicaid", "Private"), 100, replace = TRUE),
#'   physician_id = sample(1:10, 100, replace = TRUE),
#'   caller_scenario = sample(c("Vaginitis", "UTI", "Pregnancy Test"), 100, replace = TRUE)
#' )
#'
#' fit_results <- fit_poisson_models(
#'   data = wait_data,
#'   target_var = "wait_time",
#'   predictors = c("insurance_type", "caller_scenario")
#' )
#'
#' print(fit_results)
#' @export
fit_poisson_models <- function(data, target_var, predictors) {
  # Validate inputs
  if (!is.data.frame(data)) {
    logger::log_error("The input 'data' must be a valid data frame.")
    stop("The input 'data' must be a valid data frame.")
  }

  if (!target_var %in% names(data)) {
    logger::log_error(glue::glue("Target variable '{target_var}' not found in the data."))
    stop(glue::glue("Target variable '{target_var}' not found in the data."))
  }

  missing_predictors <- predictors[!predictors %in% names(data)]
  if (length(missing_predictors) > 0) {
    logger::log_error(glue::glue("The following predictors are missing in the data: {toString(missing_predictors)}"))
    stop(glue::glue("Missing predictor variables: {toString(missing_predictors)}"))
  }

  # Log input parameters
  logger::log_info(glue::glue("Fitting Poisson models for target variable '{target_var}'"))
  logger::log_info(glue::glue("Predictors: {toString(predictors)}"))

  # Initialize results tibble
  model_results <- dplyr::tibble(Predictor = character(), P_Value = numeric())

  # Fit models for each predictor
  for (predictor in predictors) {
    logger::log_info(glue::glue("Processing predictor: {predictor}"))

    # Check if predictor has sufficient unique values
    if (length(unique(data[[predictor]])) <= 1) {
      logger::log_info(glue::glue("Skipping predictor '{predictor}' (insufficient unique values)."))
      next
    }

    # Fit Poisson model
    formula <- stats::as.formula(glue::glue("{target_var} ~ {predictor}"))
    logger::log_info(glue::glue("Model formula: {deparse(formula)}"))

    model <- tryCatch(
      stats::glm(formula, family = poisson(link = "log"), data = data),
      error = function(e) {
        logger::log_error(glue::glue("Error fitting model for '{predictor}': {e$message}"))
        return(NULL)
      }
    )

    if (!is.null(model)) {
      p_value <- summary(model)$coefficients[2, 4] # Extract p-value for predictor
      model_results <- dplyr::bind_rows(
        model_results,
        dplyr::tibble(Predictor = predictor, P_Value = p_value)
      )
      logger::log_info(glue::glue("P-Value for '{predictor}': {p_value}"))
    }
  }

  # Format p-values
  model_results <- model_results %>%
    dplyr::mutate(Formatted_P_Value = ifelse(P_Value < 0.01, "<0.01", sprintf("%.2f", P_Value)))

  # Log and return results
  logger::log_info("Finished fitting Poisson models. Returning results.")
  return(model_results)
}
