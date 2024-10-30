#' Fit Poisson Models and Extract P-Values with Logging
#'
#' This function fits Poisson models for each predictor variable against a target variable.
#' It logs the process, including inputs, outputs, transformations, and any variables that are skipped due to insufficient variance.
#'
#' @param data A data frame containing the dataset.
#' @param target_var A string representing the name of the target variable.
#' @param predictors A vector of strings representing the names of predictor variables.
#' @return A tibble containing the predictor variables, their corresponding p-values, and formatted p-values.
#' @importFrom dplyr mutate bind_rows
#' @importFrom stats glm as.formula
#' @importFrom logger log_info log_error
#' @export
#'
#' @examples
#' # Example 1: Basic usage with a dataset and predictor variables
#' result <- fit_poisson_models(data = my_data_frame, target_var = "outcome", predictors = c("age", "gender", "income"))
#'
#' # Example 2: Using a larger dataset with many predictors
#' predictor_vars <- names(my_large_data_frame)[3:20]  # Exclude first two columns
#' result <- fit_poisson_models(data = my_large_data_frame, target_var = "accepts_medicaid", predictors = predictor_vars)
#'
#' # Example 3: Handling a dataset where some predictors may have only one unique value
#' result <- fit_poisson_models(data = my_data_frame, target_var = "outcome", predictors = c("region", "specialty", "age_group"))

fit_poisson_models <- function(data, target_var, predictors) {

  # Input validation
  if (!is.data.frame(data)) {
    log_error("The input 'data' must be a valid data frame.")
    stop("The input 'data' must be a valid data frame.")
  }

  if (!target_var %in% names(data)) {
    log_error("Target variable '{target_var}' not found in the data.")
    stop("Target variable not found in the data.")
  }

  if (!all(predictors %in% names(data))) {
    missing_predictors <- predictors[!predictors %in% names(data)]
    log_error("The following predictor variables are missing in the data: {toString(missing_predictors)}")
    stop("Some predictor variables are not present in the data.")
  }

  # Log the input parameters
  log_info("Fitting Poisson models with the following parameters:")
  log_info("Target Variable: {target_var}")
  log_info("Predictor Variables: {toString(predictors)}")

  # Initialize an empty tibble to store the results
  model_results <- dplyr::tibble(
    Predictor = character(),
    P_Value = numeric()
  )

  # Loop over each predictor variable
  for (predictor in predictors) {
    log_info("Processing predictor: {predictor}")

    # Check if the predictor variable has more than one unique value
    unique_values <- length(unique(data[[predictor]]))
    if (unique_values > 1) {
      # Construct the formula for the Poisson regression model
      formula <- stats::as.formula(paste(target_var, "~", predictor))
      log_info("Constructed formula: {deparse(formula)}")

      # Fit the Poisson model
      model <- tryCatch({
        stats::glm(formula, family = poisson(link = "log"), data = data)
      }, error = function(e) {
        log_error("Error fitting model for predictor '{predictor}': {e$message}")
        return(NULL)
      })

      if (!is.null(model)) {
        p_value <- summary(model)$coefficients[2, 4]  # Extract p-value for the predictor

        # Append the result to the model_results tibble
        model_results <- dplyr::bind_rows(model_results, dplyr::tibble(Predictor = predictor, P_Value = p_value))
        log_info("P-Value for {predictor}: {p_value}")
      }
    } else {
      log_info("Skipping predictor '{predictor}' because it has only one unique value.")
    }
  }

  # Format the p-values
  model_results <- model_results %>%
    dplyr::mutate(
      Formatted_P_Value = ifelse(P_Value < 0.01, "<0.01", sprintf("%.2f", P_Value))
    )

  # Log the final output and display the results
  log_info("Finished fitting Poisson models. Final results:")
  print(model_results)

  # Return the model results tibble
  log_info("Returning the final model results tibble with predictor variables and p-values.")
  return(model_results)
}
