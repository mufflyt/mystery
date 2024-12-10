#' Fit Mixed Effects Model for Mystery Caller Study with Logging
#'
#' This function fits a mixed-effects model to analyze the association between appointment wait times and insurance type, with random effects for state or physician ID. It logs the model creation and diagnostics at each step.
#'
#' @param wait_time_data A data frame with columns including `wait_time`, `insurance_type`, `state`, `physician_id`, and other demographic factors.
#' @param outcome_var A string specifying the outcome variable, typically `wait_time`.
#' @param random_effects A character vector of variables to include as random effects (e.g., `state`, `physician_id`).
#' @param fixed_effects A character vector of fixed effect variables to include in the model (e.g., `insurance_type`, `scenario`).
#' @param verbose Logical. If `TRUE`, prints detailed model diagnostics.
#' @return A list containing the fitted mixed-effects model and summaries.
#' @importFrom lme4 lmer
#' @importFrom dplyr mutate select filter
#' @importFrom logger log_info log_warn
#' @importFrom stats lm na.omit poisson sd setNames
#' @import effects
#'
#' @examples
#' # Example usage here
results_section_fit_mixed_model_with_logging <- function(
    wait_time_data, outcome_var, random_effects, fixed_effects, verbose = FALSE) {
  # Implementation of the function here
}

fit_mixed_model_with_logging <- function(wait_time_data,
                                         outcome_var = "log_business_days_until_appointment",
                                         random_effects = "(1 | NPI)",
                                         exclude_vars = c(response_var, "last", "business_days_until_appointment",
                                                          "cleaned_does_the_physician_accept_medicaid", "record_id", "ID",
                                                          "middle", "physician_information", "address",
                                                          "offered_a_clinic_appointment_to_be_seen", "reason_for_exclusions",
                                                          "state", "Grd_yr", "age_category", "notes", "first",
                                                          "does_the_physician_accept_medicaid", "insurance_type",
                                                          "zip", "Subspecialty", "NPI", "lng", "lat",
                                                          "including_this_physician_in_the_study",
                                                          "told_to_go_to_the_emergency_department"),
                                         model_type = "lmer",
                                         significance_cutoff = 0.2,
                                         save_path = NULL) {
  # Error handling for inputs
  if (!"data.frame" %in% class(data)) {
    log_error("Input data is not a valid data frame.")
    stop("Data must be a data frame.")
  }

  if (!response_var %in% names(data)) {
    log_error("Response variable '{response_var}' not found in the data.")
    stop("Response variable not found in the data.")
  }

  if (!model_type %in% c("lmer", "rlmer")) {
    log_error("Invalid model type specified: {model_type}")
    stop("Invalid model type. Use 'lmer' or 'rlmer'.")
  }

  if (!is.numeric(significance_cutoff) || significance_cutoff <= 0 || significance_cutoff >= 1) {
    log_error("Invalid significance level: {significance_cutoff}")
    stop("Significance level must be between 0 and 1.")
  }

  # Logging function inputs
  log_info("Function called with the following parameters:")
  log_info("Response Variable: {response_var}")
  log_info("Random Effect: {random_effect}")
  log_info("Excluded Columns: {toString(exclude_vars)}")
  log_info("Model Type: {model_type}")
  log_info("Significance Level: {significance_cutoff}")

  # Identify the predictor variables
  predictor_vars <- dplyr::setdiff(names(data), exclude_vars)
  log_info("Predictor variables identified: {toString(predictor_vars)}")

  # Initialize a data frame to store results
  results <- dplyr::tibble(
    Predictor = character(),
    P_Value = numeric(),
    IRR = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    Wait_Time_Effect = character()
  )

  # Loop over each predictor variable
  for (predictor in predictor_vars) {
    # Log current predictor being processed
    log_info("Processing predictor: {predictor}")

    # Check if the variable has more than one unique value
    if (length(unique(data[[predictor]])) > 1) {
      # Construct the formula for the mixed-effects model
      formula <- stats::as.formula(paste(response_var, "~", predictor, "+", random_effect))
      log_info("Constructed formula: {deparse(formula)}")

      # Fit the model based on the user’s choice (lmer or rlmer)
      if (model_type == "lmer") {
        log_info("Fitting linear mixed-effects model using lmer.")
        model <- lmerTest::lmer(formula, data = data, REML = FALSE)
      } else if (model_type == "rlmer") {
        log_info("Fitting robust linear mixed-effects model using rlmer.")
        model <- robustlmm::rlmer(formula, data = data)
      }

      # Extract the coefficient and standard error for the predictor variable
      coef_value <- summary(model)$coefficients[2, "Estimate"]
      std_error <- summary(model)$coefficients[2, "Std. Error"]
      log_info("Coefficient: {coef_value}, Standard Error: {std_error}")

      # Calculate the t-value and approximate p-value for the predictor variable
      t_value <- coef_value / std_error
      p_value <- 2 * (1 - stats::pnorm(abs(t_value)))  # Approximate p-value using normal distribution
      log_info("T-value: {t_value}, P-value: {p_value}")

      # Calculate the IRR (Incident Rate Ratio)
      irr <- exp(coef_value)
      log_info("IRR (Incident Rate Ratio): {irr}")

      # Determine the effect on wait time (longer or shorter)
      wait_time_effect <- ifelse(irr > 1, "longer wait time", "shorter wait time")
      log_info("Effect on wait time: {wait_time_effect}")

      # Calculate the 95% confidence intervals on the log scale and convert to IRR
      ci_log_lower <- coef_value - 1.96 * std_error
      ci_log_upper <- coef_value + 1.96 * std_error
      ci_lower <- exp(ci_log_lower)
      ci_upper <- exp(ci_log_upper)
      log_info("Confidence Intervals: CI Lower = {ci_lower}, CI Upper = {ci_upper}")

      # Store the results
      results <- dplyr::bind_rows(results, dplyr::tibble(
        Predictor = predictor,
        P_Value = p_value,
        IRR = irr,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        Wait_Time_Effect = wait_time_effect
      ))
      log_info("Stored results for predictor: {predictor}")

    } else {
      log_info("Skipping predictor '{predictor}' because it has only one unique value.")
    }
  }

  # Filter significant predictors based on the p-value threshold
  significant_predictors <- results %>%
    dplyr::filter(P_Value < significance_cutoff) %>%
    dplyr::arrange(P_Value)
  log_info("Filtered significant predictors. Found {nrow(significant_predictors)} significant predictors.")

  # Clean and format the significant predictors table
  significant_predictors_cleaned <- significant_predictors %>%
    dplyr::mutate(
      P_Value = ifelse(P_Value < 0.01, "<0.01", sprintf("%.3f", P_Value)),  # Round p-values to 3 decimal places or show <0.01
      IRR = sprintf("%.2f", IRR),  # Round IRR to 2 decimal places
      CI_Lower = sprintf("%.2f", CI_Lower),  # Round lower CI to 2 decimal places
      CI_Upper = sprintf("%.2f", CI_Upper)  # Round upper CI to 2 decimal places
    )
  log_info("Formatted the significant predictors table.")

  # If save_path is provided, save the table as a CSV file
  if (!is.null(save_path)) {
    readr::write_csv(significant_predictors_cleaned, save_path)
    log_info("Results saved to {save_path}")
  }

  # Display the cleaned-up table
  print(significant_predictors_cleaned)

  # Return the final table for further use
  log_info("Returning the cleaned-up significant predictors table.")
  return(significant_predictors_cleaned)
}
