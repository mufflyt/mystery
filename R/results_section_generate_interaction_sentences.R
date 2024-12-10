#' Generate Interaction Sentences for Model Interpretation
#'
#' This function generates interpretative sentences for interaction terms in a
#' regression model (Poisson or logistic), detailing how combinations of predictors
#' impact outcomes such as wait times.
#'
#' @param interaction_model A fitted regression model object (e.g., from `glm` or `lmer`).
#' @param variable1 The first variable in the interaction (character string).
#' @param variable2 The second variable in the interaction (character string).
#' @param model_summary An optional model summary object. If not provided, it will
#'   be calculated from the `interaction_model`.
#' @param confidence_level The confidence level for reporting intervals. Default is 0.95.
#' @param log_transform Logical. If `TRUE`, log-transformed estimates will be used. Default is `TRUE`.
#' @param output_format The format of the output sentences. Options are `"text"` (default)
#'   or `"markdown"`.
#'
#' @return A list of strings, each interpreting an interaction term in the model.
#'
#' @details The function uses estimated marginal means to compute and interpret
#'   interaction effects. Sentences include confidence intervals and p-values
#'   where available.
#'
#' @importFrom emmeans emmeans
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_c
#' @importFrom logger log_info log_error
#' @importFrom rlang sym
#' @examples
#' # Example: Generate interaction sentences for a Poisson model
#' interaction_model <- glm(wait_time ~ insurance_type * scenario,
#'                          data = poisson_data, family = poisson)
#'
#' interaction_sentences <- generate_interaction_sentences(
#'   interaction_model = interaction_model,
#'   variable1 = "insurance_type",
#'   variable2 = "scenario",
#'   output_format = "text"
#' )
#' print(interaction_sentences)
#'
#' @export
generate_interaction_sentences <- function(interaction_model,
                                           variable1,
                                           variable2,
                                           model_summary = NULL,
                                           confidence_level = 0.95,
                                           log_transform = TRUE,
                                           output_format = "text") {
  # Log inputs
  logger::log_info("Starting generate_interaction_sentences...")
  logger::log_info("Variables: {variable1}, {variable2}")
  logger::log_info("Confidence level: {confidence_level}, Log transform: {log_transform}")
  logger::log_info("Output format: {output_format}")

  # Validate inputs
  if (!inherits(interaction_model, "glm") && !inherits(interaction_model, "lmerMod")) {
    logger::log_error("Invalid model: must be GLM or mixed-effects model.")
    stop("Error: The model must be a GLM or mixed-effects model.")
  }

  if (!requireNamespace("emmeans", quietly = TRUE)) {
    logger::log_error("The 'emmeans' package is required but not installed.")
    stop("Install the 'emmeans' package to use this function.")
  }

  if (!is.character(variable1) || !is.character(variable2)) {
    logger::log_error("Both 'variable1' and 'variable2' must be character strings.")
    stop("Error: 'variable1' and 'variable2' must be character strings.")
  }

  if (!output_format %in% c("text", "markdown")) {
    logger::log_error("Invalid output format: {output_format}")
    stop("Output format must be 'text' or 'markdown'.")
  }

  # Use provided model summary or generate one
  if (is.null(model_summary)) {
    logger::log_info("Generating model summary...")
    model_summary <- summary(interaction_model)
  }

  # Construct formula for interaction
  interaction_formula <- stats::as.formula(paste("~", variable1, "*", variable2))
  logger::log_info("Interaction formula: {deparse(interaction_formula)}")

  # Compute estimated marginal means for the interaction
  interaction_estimates <- tryCatch({
    emmeans::emmeans(interaction_model, interaction_formula, type = "response")
  }, error = function(e) {
    logger::log_error("Error computing emmeans: {e$message}")
    stop("Error computing estimated marginal means.")
  })

  # Convert the emmeans result to a data frame
  interaction_data <- tryCatch({
    as.data.frame(interaction_estimates)
  }, error = function(e) {
    logger::log_error("Error converting emmeans to data frame: {e$message}")
    stop("Error converting emmeans results.")
  })

  logger::log_info("Extracted interaction data:\n{interaction_data}")

  # Prepare sentences
  interaction_sentences <- list()
  levels_var1 <- unique(interaction_data[[variable1]])
  levels_var2 <- unique(interaction_data[[variable2]])

  for (level1 in levels_var1) {
    scenario_data <- dplyr::filter(interaction_data, !!rlang::sym(variable1) == level1)
    for (level2 in levels_var2) {
      current_data <- dplyr::filter(scenario_data, !!rlang::sym(variable2) == level2)

      interaction_p_value <- model_summary$coefficients[
        grepl(paste0(level1, ":", variable2, level2), rownames(model_summary$coefficients)),
        "Pr(>|z|)"
      ]

      interaction_p_value <- ifelse(length(interaction_p_value) == 0, NA, interaction_p_value)

      if (output_format == "text") {
        sentence <- stringr::str_c(
          level1, " ", level2, ": Patients wait ", sprintf("%.1f", current_data$rate),
          " days, with a ", confidence_level * 100, "% CI from ",
          sprintf("%.1f", current_data$asymp.LCL), " to ", sprintf("%.1f", current_data$asymp.UCL),
          " days. P-value: ", sprintf("%.3f", interaction_p_value)
        )
      } else if (output_format == "markdown") {
        sentence <- stringr::str_c(
          "**", level1, " ", level2, ":** Patients wait **", sprintf("%.1f", current_data$rate),
          " days**, CI: **[", sprintf("%.1f", current_data$asymp.LCL), ", ",
          sprintf("%.1f", current_data$asymp.UCL), "]**, P-value: **",
          sprintf("%.3f", interaction_p_value), "**."
        )
      }

      interaction_sentences <- c(interaction_sentences, sentence)
    }
  }

  logger::log_info("Generated interaction sentences.")
  return(interaction_sentences)
}
