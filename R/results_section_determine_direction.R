#' Determine the Direction of Effects for Significant Variables with Logging and Error Handling
#'
#' This function determines whether the effect of each significant predictor is positive ("Higher") or negative ("Lower").
#' It logs the process, including inputs, outputs, and each step of the analysis.
#'
#' @param data A data frame containing the dataset.
#' @param target_var A string representing the name of the target variable (e.g., the outcome or dependent variable).
#' @param significant_vars A data frame containing significant predictors with a column "Variable" and their p-values.
#' @return A data frame with an additional column "Direction" indicating whether each significant predictor is associated with a "Higher" or "Lower" effect.
#' @importFrom dplyr mutate pull
#' @importFrom stats glm as.formula coef
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#'
#'
#' @examples
#' # Example 1: Determine the direction of effects with a basic dataset
#' df <- data.frame(
#'   age = rnorm(100, mean = 50, sd = 10),
#'   gender = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
#'   accepts_medicaid = rbinom(100, 1, 0.5)
#' )
#' significant_vars <- data.frame(Variable = c("age", "gender"))
#' determine_direction(df, "accepts_medicaid", significant_vars)
#'
#' # Example 2: A dataset with multiple continuous predictors
#' df2 <- data.frame(
#'   income = rnorm(100, mean = 60000, sd = 15000),
#'   education_years = rnorm(100, mean = 16, sd = 2),
#'   accepts_insurance = rbinom(100, 1, 0.6)
#' )
#' significant_vars2 <- data.frame(Variable = c("income", "education_years"))
#' determine_direction(df2, "accepts_insurance", significant_vars2)
#'
#' # Example 3: Handling a dataset with categorical and continuous predictors
#' df3 <- data.frame(
#'   years_experience = rnorm(100, mean = 10, sd = 5),
#'   specialty = factor(sample(c("Cardiology", "Neurology"), 100, replace = TRUE)),
#'   accepts_medicare = rbinom(100, 1, 0.7)
#' )
#' significant_vars3 <- data.frame(Variable = c("years_experience", "specialty"))
#' determine_direction(df3, "accepts_medicare", significant_vars3)
#' @export
determine_direction <- function(data, target_var, significant_vars) {

  # Log inputs
  cat("Starting the determine_direction function...\n")
  cat("Target Variable:", target_var, "\n")
  cat("Significant Variables Data Frame:\n")
  print(significant_vars)

  # Input validation
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }

  if (!is.character(target_var) || length(target_var) != 1) {
    stop("Error: 'target_var' must be a single string representing the target variable.")
  }

  if (!is.data.frame(significant_vars) || !("Variable" %in% names(significant_vars))) {
    stop("Error: 'significant_vars' must be a data frame with a 'Variable' column.")
  }

  # Initialize an empty list to store directions
  directions <- c()

  # Step 1: Loop through each significant variable and fit a Poisson model
  cat("Step 1: Processing significant variables and fitting Poisson models...\n")

  for (var in significant_vars$Variable) {
    cat("Processing variable:", var, "\n")

    # Create the formula for the model
    formula <- stats::as.formula(stringr::str_c(target_var, "~", var))

    # Fit the Poisson regression model
    model <- stats::glm(formula, family = poisson(link = "log"), data = data)

    # Extract the coefficient for the predictor
    coef_value <- stats::coef(model)[2]

    # Determine the direction based on the coefficient value
    direction <- if (coef_value > 0) "Higher" else "Lower"
    directions <- c(directions, direction)

    # Log the result
    cat("Direction for variable", var, ":", direction, "(Coefficient =", coef_value, ")\n")
  }

  # Step 2: Add the direction column to significant_vars data frame
  cat("Step 2: Adding the direction column to the significant_vars data frame...\n")

  significant_vars <- significant_vars %>%
    dplyr::mutate(Direction = directions)

  # Log the final data frame with directions
  cat("Final significant variables with directions:\n")
  print(significant_vars)

  # Step 3: Return the final data frame with directions
  cat("determine_direction function completed successfully. Returning the result...\n")
  return(significant_vars)
}
