#' Create a Dynamic Formula for Mixed Effects Models
#'
#' Dynamically generates a formula for mixed-effects models by excluding specified columns and quoting
#' variables with special characters. Handles both fixed-effects and mixed-effects models by making
#' the grouping variable optional.
#'
#' @param dataset A dataframe containing the variables for the formula.
#' @param outcome_var A character string specifying the outcome variable. Defaults to "business_days_until_appointment".
#' @param group_var A character string specifying the grouping variable. If NULL, creates a fixed-effects formula. Defaults to NULL.
#' @param exclude_columns A character vector of column names to exclude from the formula. Defaults to a pre-defined list.
#'
#' @return A formula object representing the specified model.
#' @importFrom dplyr select setdiff
#' @importFrom logger log_info log_debug log_warn
#'
#' @examples
#' # Example 1: Default behavior with pre-defined exclude_columns
#' dataset <- data.frame(
#'   business_days_until_appointment = rpois(100, lambda = 5),
#'   NPI = sample(letters, 100, replace = TRUE),
#'   age = rnorm(100, mean = 50, sd = 10),
#'   gender = sample(c("Male", "Female"), 100, replace = TRUE)
#' )
#' create_formula(dataset)
#'
#' # Example 2: Custom outcome and grouping variables
#' dataset <- data.frame(
#'   wait_time = rpois(100, lambda = 7),
#'   provider_id = sample(letters, 100, replace = TRUE),
#'   income = rnorm(100, mean = 60000, sd = 15000),
#'   region = sample(c("Urban", "Rural"), 100, replace = TRUE)
#' )
#' create_formula(dataset, outcome_var = "wait_time", group_var = "provider_id")
#'
#' # Example 3: Specifying additional columns to exclude
#' dataset <- data.frame(
#'   business_days_until_appointment = rpois(100, lambda = 3),
#'   NPI = sample(letters, 100, replace = TRUE),
#'   specialty = sample(c("Cardiology", "Dermatology"), 100, replace = TRUE),
#'   phone_number = sample(1000000000:1999999999, 100)
#' )
#' create_formula(dataset, exclude_columns = c("phone_number", "specialty"))
create_formula <- function(
    dataset,
    outcome_var = "business_days_until_appointment",
    group_var = NULL,
    exclude_columns = c(
      "first", "last", "call_date", "appointment_date", "Able_to_make_appointment", "phone",
      "name_of_person_completing_form_thank_you", "complete", "Retired_exclusion",
      "will_this_physician_see_children_if_they_ask_about_insurance_say_they_have_blue_cross_blue_shield",
      "reason_for_exclusions",
      "notes_if_the_phone_number_is_incorrect_please_google_the_correct_phone_number_and_put_the_correct_phone_number_here_call_or_text_lizzy_309_989_0432_dr_muffly_cell_phone_720_810_9863_we_are_happy_to_answer_questions",
      "contacted_>_0_business_days_to_appt", "contacted", "Medical_school", "residency_training",
      "Teledermatology"
    )
) {
  # Log inputs
  logger::log_info("Starting create_formula function")
  logger::log_debug("Input dataset has {ncol(dataset)} columns and {nrow(dataset)} rows.")
  logger::log_debug("Outcome variable: {outcome_var}")
  if (!is.null(group_var)) {
    logger::log_debug("Group variable: {group_var}")
  } else {
    logger::log_info("No grouping variable specified. Creating a fixed-effects formula.")
  }
  logger::log_debug("Exclude columns: {paste(exclude_columns, collapse = ', ')}")

  # Helper function to quote variables
  quote_variables <- function(vars) {
    logger::log_debug("Quoting variables with special characters.")
    sapply(vars, function(var) {
      if (grepl("[^a-zA-Z0-9_]", var)) {
        paste0("`", var, "`")
      } else {
        var
      }
    })
  }

  # Identify predictor variables
  logger::log_debug("Identifying predictor variables.")
  predictor_vars <- dplyr::setdiff(names(dataset), c(outcome_var, group_var, exclude_columns))
  logger::log_info("Identified {length(predictor_vars)} predictor variables.")

  # Quote predictor variables
  predictor_vars_quoted <- quote_variables(predictor_vars)

  # Construct formula text
  logger::log_debug("Constructing formula text.")
  if (!is.null(group_var)) {
    formula_text <- paste(
      outcome_var, "~",
      paste(predictor_vars_quoted, collapse = " + "),
      "+ (1|", group_var, ")"
    )
  } else {
    formula_text <- paste(
      outcome_var, "~",
      paste(predictor_vars_quoted, collapse = " + ")
    )
  }

  # Convert to formula object
  formula <- stats::as.formula(formula_text)
  logger::log_info("Generated formula: {formula_text}")

  # Return formula
  return(formula)
}
