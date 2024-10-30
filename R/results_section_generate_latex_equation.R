#' Generate LaTeX Equation with Logging
#'
#' This function generates a LaTeX equation that incorporates the provided "patient_scenario_label".
#' The function logs the input, processes the input string by escaping LaTeX characters,
#' and constructs a LaTeX equation. It logs all operations and can output to both the console
#' and a log file (if provided).
#'
#' @param patient_scenario_label A string representing the text (typically a "Patient Scenario")
#' to be inserted into the LaTeX equation. Default is "Default Patient Scenario".
#' @param log_file A string representing the full path to a file where logs will be written.
#' If NULL (default), logs are printed only to the console.
#'
#' @return The LaTeX code as a string, ready to be inserted into an RMarkdown or LaTeX document.
#'
#' @details
#' The function generates dynamic LaTeX code using the provided `patient_scenario_label`, ensuring
#' special LaTeX characters (such as underscores) are escaped properly. It logs the entire process,
#' including the input validation, transformations, and final output. If no `patient_scenario_label`
#' is provided, a default value is used.
#'
#' @examples
#' # Example 1: Basic usage with logging to the console
#' \dontrun{
#' generate_latex_equation("Patient Scenario")
#' }
#'
#' # Example 2: Handle underscores in the patient_scenario_label
#' \dontrun{
#' generate_latex_equation("Patient_Scenario_With_Underscores")
#' }
#'
#' # Example 3: Logging the process to a file
#' \dontrun{
#' log_file_path <- "latex_generation_log.txt"
#' generate_latex_equation("Patient Scenario", log_file = log_file_path)
#' }
#'
#' @importFrom logger log_info log_appender appender_file
#' @importFrom stringr str_replace_all
#' @import glue
#' @export
generate_latex_equation <- function(patient_scenario_label = "Default Patient Scenario", log_file = NULL) {

  # Set up logger to log to a file if log_file is provided
  if (!is.null(log_file)) {
    logger::log_appender(logger::appender_file(log_file))
  }

  # Log function inputs
  logger::log_info(glue::glue("Function called with patient_scenario_label: '{patient_scenario_label}'"))

  # Input validation: Check if patient_scenario_label is provided and is a character string
  if (missing(patient_scenario_label) || is.null(patient_scenario_label)) {
    warning_msg <- "Warning: No patient_scenario_label provided. Using default value."
    logger::log_info(warning_msg)
    patient_scenario_label <- "Default Patient Scenario"
  } else if (!is.character(patient_scenario_label)) {
    error_msg <- "Error: patient_scenario_label must be a character string."
    logger::log_info(error_msg)
    stop(error_msg)
  }

  # Escape special LaTeX characters in the input string
  logger::log_info("Escaping LaTeX special characters in patient_scenario_label.")
  escaped_label <- stringr::str_replace_all(patient_scenario_label, "_", "\\\\_")
  logger::log_info(glue::glue("Escaped patient_scenario_label: '{escaped_label}'"))

  # Construct the LaTeX code using custom delimiters for glue
  logger::log_info("Constructing LaTeX code.")
  latex_code <- glue::glue(
    "$$\n",
    "\\begin{{align*}}\n",
    "P(\\text{{Business Days until New Patient Appointment}} = x) &= \\frac{{e^{{-\\lambda}} \\cdot \\lambda^x}}{{x!}} \\\\\n",
    "\\sqrt{{\\lambda}} &= \\beta_0 \\\\\n",
    "& + \\beta_1 \\cdot \\underline{{\\mathbf{{\\large{{\\text<<escaped_label>>}}}}}} \\\\\n",
    "& + ( 1 | \\text{{Physician NPI}})\n",
    "\\end{{align*}}\n",
    "$$",
    .open = "<<", .close = ">>"
  )

  # Log the LaTeX output
  logger::log_info("Generated LaTeX code.")
  logger::log_info(latex_code)

  # Inform the user that LaTeX syntax should not go inside an R code chunk
  logger::log_info("LaTeX Syntax does NOT go in a code chunk!!!")

  # Return the LaTeX code
  return(latex_code)
}
