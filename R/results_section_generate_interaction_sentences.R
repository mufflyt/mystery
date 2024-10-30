#' Generate Interaction Sentences with Logging and Error Handling
#'
#' This function generates sentences based on the interaction of two variables in a fitted model.
#' It logs all steps, including inputs, outputs, data transformations, and p-value extraction.
#'
#' @param interaction_model A fitted model object (e.g., from glm or lmer).
#' @param variable1 A string representing the first interacting variable.
#' @param variable2 A string representing the second interacting variable.
#' @param model_summary A summary object of the model. If NULL, it will be generated automatically.
#' @param confidence_level The confidence level for the confidence intervals. Default is 0.95.
#' @param log_transform A logical indicating if the rate should be log-transformed. Default is TRUE.
#' @param output_format A string specifying the output format: "text" or "markdown". Default is "text".
#' @return A list of sentences describing the interaction between the two variables.
#' @importFrom emmeans emmeans
#' @importFrom dplyr select filter mutate
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom logger log_info log_error
#' @export
#'
#' @examples
#' # Example 1: Basic usage with text output
#' \dontrun{
#' result <- generate_interaction_sentences(interaction_model, "scenario", "insurance", output_format = "text")
#' }
#'
#' # Example 2: Markdown format output
#' \dontrun{
#' result <- generate_interaction_sentences(interaction_model, "scenario", "insurance", output_format = "markdown")
#' }
#'
#' # Example 3: Custom confidence level
#' \dontrun{
#' result <- generate_interaction_sentences(interaction_model, "scenario", "insurance", confidence_level = 0.90)
#' }

generate_interaction_sentences <- function(interaction_model, variable1, variable2,
                                           model_summary = NULL, confidence_level = 0.95,
                                           log_transform = TRUE, output_format = "text") {

  # Log function inputs
  log_info("Starting generate_interaction_sentences function...")
  log_info("Variables: {variable1}, {variable2}")
  log_info("Confidence level: {confidence_level}, Log transform: {log_transform}")
  log_info("Output format: {output_format}")

  # Error Handling for Inputs
  if (!inherits(interaction_model, "glm") && !inherits(interaction_model, "lmerMod")) {
    log_error("Invalid model provided. Must be a GLM or mixed-effects model.")
    stop("Error: The model provided must be a GLM or mixed-effects model.")
  }

  if (!requireNamespace("emmeans", quietly = TRUE)) {
    log_error("Package 'emmeans' is required but not installed.")
    stop("Package 'emmeans' is required but not installed.")
  }

  if (!is.character(variable1) || !is.character(variable2)) {
    log_error("Both 'variable1' and 'variable2' must be strings representing variable names.")
    stop("Error: 'variable1' and 'variable2' must be strings.")
  }

  if (!output_format %in% c("text", "markdown")) {
    log_error("Unsupported output format provided: {output_format}")
    stop("Error: Unsupported output format. Please use 'text' or 'markdown'.")
  }

  # Use the provided model summary if available; otherwise, calculate it
  if (is.null(model_summary)) {
    log_info("Generating model summary...")
    model_summary <- summary(interaction_model)
  }

  # Construct the formula for the interaction
  interaction_formula <- as.formula(paste("~", variable1, "*", variable2))
  log_info("Interaction formula: {deparse(interaction_formula)}")

  # Compute estimated marginal means for the interaction
  interaction_estimates <- tryCatch({
    emmeans::emmeans(interaction_model, interaction_formula, type = "response")
  }, error = function(e) {
    log_error("Error computing emmeans for interaction: {e$message}")
    stop("Error in computing estimated marginal means.")
  })

  # Convert the emmeans result to a tibble
  interaction_data <- tryCatch({
    as.data.frame(interaction_estimates)
  }, error = function(e) {
    log_error("Error converting emmeans result to dataframe: {e$message}")
    stop("Error in converting emmeans results.")
  })

  log_info("Extracted interaction data:\n{interaction_data}")

  # Initialize an empty list to store sentences
  interaction_sentences <- list()

  # Get unique levels for both variables
  levels_var1 <- unique(interaction_data[[variable1]])
  levels_var2 <- unique(interaction_data[[variable2]])

  # Loop through each unique level of the first variable
  for (level1 in levels_var1) {
    # Filter data for the current level of the first variable
    scenario_data <- dplyr::filter(interaction_data, !!rlang::sym(variable1) == level1)
    log_info("Filtered data for {variable1} = {level1}:\n{scenario_data}")

    # Loop through each level of the second variable
    for (level2 in levels_var2) {
      # Extract data for the current level of the second variable
      current_data <- dplyr::filter(scenario_data, !!rlang::sym(variable2) == level2)
      log_info("{variable2} = {level2} data:\n{current_data}")

      # Extract the p-value for the interaction
      interaction_p_value <- model_summary$coefficients[
        grepl(paste0(level1, ":", variable2, level2), rownames(model_summary$coefficients)),
        "Pr(>|z|)"
      ]

      # Handle cases where no p-value is found
      if (length(interaction_p_value) == 0) {
        interaction_p_value <- NA
      }
      log_info("Interaction p-value for {variable1} = {level1} and {variable2} = {level2}: {interaction_p_value}")

      # Build the sentence
      if (output_format == "text") {
        sentence <- stringr::str_c(
          level1, " ", level2, ": Patients wait ", sprintf("%.1f", current_data$rate),
          " days, with a ", confidence_level * 100, "% confidence interval (CI) ranging from ",
          sprintf("%.1f", current_data$asymp.LCL), " to ", sprintf("%.1f", current_data$asymp.UCL),
          " days. The interaction p-value is ", sprintf("%.3f", interaction_p_value), "."
        )
      } else if (output_format == "markdown") {
        sentence <- stringr::str_c(
          "**", level1, " ", level2, ":** Patients wait **", sprintf("%.1f", current_data$rate),
          " days**, with a ", confidence_level * 100, "% confidence interval (CI) ranging from **",
          sprintf("%.1f", current_data$asymp.LCL), " to ", sprintf("%.1f", current_data$asymp.UCL),
          " days**. The interaction p-value is **", sprintf("%.3f", interaction_p_value), "**."
        )
      }

      # Append the sentence to the list
      interaction_sentences <- c(interaction_sentences, sentence)
    }
  }

  # Log the generated sentences
  log_info("Generated sentences:\n{interaction_sentences}")

  # Return the generated sentences
  return(interaction_sentences)
}
