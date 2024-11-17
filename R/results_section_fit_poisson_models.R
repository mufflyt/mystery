#' Fit Poisson Models for Analyzing Wait Times in Mystery Caller Study
#'
#' This function fits Poisson regression models to analyze wait times for appointments based on caller scenario data, such as private insurance versus Medicaid. The model outputs provide insights into whether insurance type and other variables impact wait times for new patient appointments.
#'
#' @param wait_times_data A data frame containing information on appointment wait times and insurance type. Must include columns: `wait_time`, `insurance_type`, `physician_id`, `caller_scenario`, and additional demographic or study-specific variables.
#' @param outcome_var A string representing the column name for the outcome variable, typically `wait_time`.
#' @param group_vars A character vector of grouping variables to include as random effects, such as `physician_id`.
#' @param fixed_effects A character vector specifying fixed effect variables, including `insurance_type` and additional relevant predictors.
#' @param verbose A logical. If `TRUE`, logs model diagnostics and summaries to the console.
#'
#' @return A list with fitted Poisson models for each insurance scenario and a summary of fixed and random effect estimates.
#' @importFrom lme4 glmer
#' @importFrom broom.mixed tidy
#' @importFrom dplyr filter select mutate
#' @importFrom stats formula
#' @importFrom logger log_info log_warn
#' @importFrom stats lm na.omit poisson sd setNames
#'
#' @examples
#' # Example 1: Fitting Poisson model with wait time data
#' wait_data <- data.frame(
#'   wait_time = rpois(100, lambda = 5),
#'   insurance_type = sample(c("Medicaid", "Private"), 100, replace = TRUE),
#'   physician_id = sample(1:10, 100, replace = TRUE),
#'   caller_scenario = sample(c("Vaginitis", "UTI", "Pregnancy Test"), 100, replace = TRUE),
#'   demographic_var = rnorm(100)
#' )
#' fit_results <- fit_poisson_models(
#'   wait_times_data = wait_data,
#'   outcome_var = "wait_time",
#'   group_vars = c("physician_id"),
#'   fixed_effects = c("insurance_type", "caller_scenario")
#' )
#'
#' # Example 2: Adding demographic information and viewing summary output
#' demographic_data <- data.frame(
#'   wait_time = rpois(200, lambda = 6),
#'   insurance_type = sample(c("Medicaid", "Private"), 200, replace = TRUE),
#'   physician_id = sample(1:20, 200, replace = TRUE),
#'   caller_scenario = sample(c("Vaginitis", "UTI", "TOA"), 200, replace = TRUE),
#'   age = sample(30:60, 200, replace = TRUE),
#'   specialty = sample(c("OBGYN", "Family Medicine"), 200, replace = TRUE)
#' )
#' fit_results_demographics <- fit_poisson_models(
#'   wait_times_data = demographic_data,
#'   outcome_var = "wait_time",
#'   group_vars = c("physician_id", "specialty"),
#'   fixed_effects = c("insurance_type", "caller_scenario", "age"),
#'   verbose = TRUE
#' )
#'
#' # Example 3: Evaluating impact of scenario type and demographics on wait times
#' scenario_data <- data.frame(
#'   wait_time = rpois(150, lambda = 4),
#'   insurance_type = sample(c("Medicaid", "Private"), 150, replace = TRUE),
#'   physician_id = sample(1:15, 150, replace = TRUE),
#'   scenario = sample(c("Emergency", "Urgent"), 150, replace = TRUE),
#'   gender = sample(c("Male", "Female"), 150, replace = TRUE)
#' )
#' scenario_results <- fit_poisson_models(
#'   wait_times_data = scenario_data,
#'   outcome_var = "wait_time",
#'   group_vars = "physician_id",
#'   fixed_effects = c("insurance_type", "scenario", "gender")
#' )
#' print(scenario_results)
#' @export

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
