#' Fit a Mixed-Effects Model with Logging and Robust Error Handling
#'
#' This function fits either a linear mixed-effects model (`lmer`) or a robust linear mixed-effects model (`rlmer`)
#' and logs every step. It is designed to handle errors robustly, provide progress and completion signals through system beeps, and
#' optionally save results to a file. The function is highly configurable but works out of the box with default settings.
#'
#' @param data A data frame containing the dataset.
#' @param response_var A character string specifying the response variable. Default is `"log_business_days_until_appointment"`.
#' @param random_effect A character string specifying the random effect. Default is `"(1 | NPI)"`.
#' @param exclude_vars A character vector specifying the columns to exclude from the predictor variables. Default is a list of variables typically excluded in the analysis.
#' @param model_type A character string indicating the model type: `"lmer"` for linear mixed-effects or `"rlmer"` for robust linear mixed-effects. Default is `"lmer"`.
#' @param significance_cutoff A numeric value for the p-value threshold for filtering significant predictors. Default is `0.2`.
#' @param save_path Optional. A character string specifying the file path where the results should be saved as a CSV. If not provided, results will not be saved.
#'
#' @return A tibble with significant predictors, p-values, IRR (Incident Rate Ratios), confidence intervals, and the associated wait time effects.
#' @importFrom robustlmm rlmer
#' @importFrom lmerTest lmer
#' @importFrom dplyr mutate filter arrange bind_rows setdiff
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#' @importFrom beepr beep
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @importFrom stats as.formula pnorm
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default settings
#' df <- my_data_frame
#' result <- fit_mixed_model_with_logging(data = df)
#'
#' # Example 2: Using a robust linear mixed-effects model (rlmer) and saving the results
#' result <- fit_mixed_model_with_logging(data = df, model_type = "rlmer", save_path = "results_rlmer.csv")
#'
#' # Example 3: Custom response variable and random effect, with significance level 0.05
#' result <- fit_mixed_model_with_logging(data = df,
#'                                        response_var = "some_other_response",
#'                                        random_effect = "(1 | group_id)",
#'                                        significance_cutoff = 0.05)
fit_mixed_model_with_logging <- function(data,
                                         response_var = "log_business_days_until_appointment",
                                         random_effect = "(1 | NPI)",
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

  # Beep to indicate the function is progressing
  log_info("Progress beep activated.")
  beepr::beep(1)

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

  # Beep to indicate function completion
  log_info("Completion beep activated.")
  beepr::beep(2)

  # Display the cleaned-up table
  print(significant_predictors_cleaned)

  # Return the final table for further use
  log_info("Returning the cleaned-up significant predictors table.")
  print(significant_predictors_cleaned)
  return(significant_predictors_cleaned)
}
