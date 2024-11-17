#' Generate Interaction Sentences for Model Interpretation in Mystery Caller Studies
#'
#' This function generates interpretative sentences for interaction terms in a Poisson or logistic regression model, detailing how combinations of predictors impact outcomes such as wait times.
#'
#' @param model_object A fitted model object, typically from Poisson or logistic regression.
#' @param interaction_terms A character vector of interaction terms to interpret.
#' @param context A string specifying the context, such as "wait times by insurance type".
#' @return A list of strings, each interpreting an interaction term in the model.
#' @importFrom broom tidy
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_c
#' @importFrom purrr map_chr
#' @importFrom stats lm na.omit poisson sd setNames
#'
#' @examples
#' # Example 1: Generate interaction sentences for a simple model
#' interaction_model <- glm(wait_time ~ insurance_type * scenario,
#' data = poisson_data, family = poisson)
#' interaction_sentences <- results_section_generate_interaction_sentences(
#'   model_object = interaction_model,
#'   interaction_terms = c("insurance_type", "scenario"),
#'   context = "appointment wait times"
#' )
#' print(interaction_sentences)
#'
#' # Example 2: Interpret interactions in a more complex model
#' complex_model <- glm(wait_time ~ insurance_type * scenario * age,
#' data = poisson_data, family = poisson)
#' complex_interactions <- results_section_generate_interaction_sentences(
#'   model_object = complex_model,
#'   interaction_terms = c("insurance_type", "scenario", "age"),
#'   context = "wait times and demographic variables"
#' )
#' print(complex_interactions)
#'
#' # Example 3: Contextual interpretation for multiple predictors
#' scenario_model <- glm(wait_time ~ scenario * age * gender,
#' data = poisson_data, family = poisson)
#' scenario_interpretation <- results_section_generate_interaction_sentences(
#'   model_object = scenario_model,
#'   interaction_terms = c("scenario", "age", "gender"),
#'   context = "wait times across scenarios and demographics"
#' )
#' print(scenario_interpretation)
#' @export
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
