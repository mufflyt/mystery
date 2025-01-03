#' Perform Logistic Regression on Multiple Predictors with Logging
#'
#' This function performs logistic regression for multiple predictor variables
#' against a specified target variable. It logs the process, including inputs,
#' data transformations, and results. The function filters predictors based on
#' a given significance level.
#'
#' @param df A data frame containing the target and predictor variables.
#' @param target_variable A string representing the name of the target variable.
#' @param predictor_vars A vector of strings representing the names of predictor
#' variables.
#' @param significance_level A numeric value specifying the significance level
#' for filtering predictors.
#' @return A data frame containing significant predictors with their p-values
#' and formatted p-values.
#' @importFrom dplyr filter arrange mutate %>%
#' @importFrom stats glm as.formula
#' @export
#' @examples
#' # Assuming df is your data frame
#' target_variable <- "cleaned_does_the_physician_accept_medicaid_numeric"
#' predictor_vars <- setdiff(
#'   names(df),
#'   c(
#'     target_variable,
#'     "does_the_physician_accept_medicaid",
#'     "cleaned_does_the_physician_accept_medicaid"
#'   )
#' )
#' significance_logistic_regression <- 0.2
#'
#' significant_vars <- logistic_regression(
#'   df,
#'   target_variable,
#'   predictor_vars,
#'   significance_level = significance_logistic_regression
#' )
#' print(significant_vars)
logistic_regression <- function(df,
                                target_variable,
                                predictor_vars,
                                significance_level) {
  # Log the function inputs
  cat("Starting logistic_regression...\n")
  cat("Target Variable:", target_variable, "\n")
  cat(
    "Predictor Variables:",
    paste(predictor_vars, collapse = ", "),
    "\n"
  )
  cat("Significance Level:", significance_level, "\n")

  # Initialize an empty data frame to store results
  results <- data.frame(
    Variable = character(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each predictor variable
  for (var in predictor_vars) {
    cat("Processing predictor:", var, "\n")

    # Check if the variable has more than one unique value
    if (length(unique(df[[var]])) > 1) {
      # Construct the formula for logistic regression
      formula <- stats::as.formula(paste(target_variable, "~", var))

      # Fit the logistic regression model
      model <- stats::glm(formula,
        family = poisson(link = "log"),
        data = df
      )

      # Extract the p-value for the predictor variable
      p_value <- summary(model)$coefficients[2, 4]
      cat("P-Value for", var, ":", p_value, "\n")

      # Store the result in the results data frame
      results <- rbind(results, data.frame(Variable = var, P_Value = p_value))
    } else {
      cat(
        "Skipping predictor",
        var,
        "due to insufficient unique values.\n"
      )
    }
  }

  # Format the p-values
  results <- results %>%
    dplyr::mutate(Formatted_P_Value = ifelse(P_Value < 0.01, "<0.01",
      round(P_Value, 2)
    ))

  # Filter results based on the significance level and log the output
  significant_results <- results %>%
    dplyr::filter(P_Value < significance_level) %>%
    dplyr::arrange(P_Value)

  cat("Significant Predictors:\n")
  print(significant_results)

  # Return the significant results
  return(significant_results)
}
