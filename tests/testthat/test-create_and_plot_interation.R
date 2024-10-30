# Load necessary packages
library(lme4)
library(dplyr)
library(effects)
library(ggplot2)
library(rlang)
library(tibble)

create_and_plot_interaction <- function(data_path, response_variable, variable_of_interest, interaction_variable, random_intercept, output_path, resolution = 100) {
  # Read the data
  data <- readRDS(data_path)

  # Ensure the data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }

  # Log inputs
  cat("Inputs:\n")
  cat("response_variable:", response_variable, "\n")
  cat("variable_of_interest:", variable_of_interest, "\n")
  cat("interaction_variable:", interaction_variable, "\n")
  cat("random_intercept:", random_intercept, "\n")
  cat("output_path:", output_path, "\n")
  cat("resolution:", resolution, "\n\n")

  # Rename the columns for simplicity
  data <- data %>%
    dplyr::rename(
      response_var = !!rlang::sym(response_variable),
      var_interest = !!rlang::sym(variable_of_interest),
      int_var = !!rlang::sym(interaction_variable)
    )

  # Ensure the columns are in the correct format
  data <- data %>%
    dplyr::mutate(
      response_var = as.numeric(response_var),
      var_interest = as.factor(var_interest),
      int_var = as.factor(int_var)
    )

  # Log the first few rows of data to check formats
  cat("Data preview after renaming and type conversion:\n")
  print(head(data))
  cat("\n\n")

  # Construct the model formula
  interaction_term <- "int_var * var_interest"
  model_formula <- as.formula(paste("response_var ~", interaction_term, "+ (1 |", random_intercept, ")"))

  # Log model formula
  cat("Model formula:", deparse(model_formula), "\n\n")

  # Fit the model with interaction
  cat("Fitting the model...\n")
  glmer_model <- lme4::glmer(model_formula,
                             data = data,
                             family = poisson(link = "log"),
                             nAGQ = 0,
                             verbose = 0L)
  cat("Model fitted successfully.\n\n")

  # Log model summary
  cat("Model summary:\n")
  print(summary(glmer_model))
  cat("\n\n")

  # Create the effects plot
  cat("Creating effects plot...\n")
  eff <- tryCatch({
    effects::allEffects(glmer_model, typical = mean, xlevels = 20)
  }, error = function(e) {
    cat("Error creating effects plot:", e$message, "\n")
    return(NULL)
  })

  if (is.null(eff)) {
    cat("Effects plot could not be created. Falling back to manual effects calculation...\n")

    # Manual effects calculation
    effect_data <- data %>%
      group_by(int_var, var_interest) %>%
      summarise(mean_pred = mean(response_var, na.rm = TRUE), .groups = 'drop')

    cat("Manual effects calculation successful.\n")

    # Create plot manually
    eff_filename <- file.path(output_path, paste0("interaction_", interaction_variable, "_", variable_of_interest, ".png"))
    cat("Saving effects plot to:", eff_filename, "\n")
    png(filename = eff_filename, width = 6, height = 4, units = "in", res = resolution)
    ggplot(effect_data, aes(x = var_interest, y = mean_pred, color = int_var)) +
      geom_point() +
      geom_line(aes(group = int_var)) +
      labs(title = "Interaction Effect Plot", x = interaction_variable, y = response_variable) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12), axis.title = element_text(size = 10), axis.text = element_text(size = 8))
    dev.off()
    cat("Effects plot saved successfully.\n\n")

    return(list(model = glmer_model, effects_plot_data = effect_data))
  }

  # Save the effects plot
  eff_filename <- file.path(output_path, paste0("interaction_", interaction_variable, "_", variable_of_interest, ".png"))
  cat("Saving effects plot to:", eff_filename, "\n")
  png(filename = eff_filename, width = 6, height = 4, units = "in", res = resolution)
  plot(eff, main = "Interaction Effect Plot", cex.main = 0.8, cex.axis = 0.8, cex.lab = 0.8, cex = 0.8)
  dev.off()
  cat("Effects plot saved successfully.\n\n")

  # Log outputs
  cat("Outputs:\n")
  print(glmer_model)
  print("Effects plot object:\n")
  print(eff)

  # Return the model and effects plot
  return(list(model = glmer_model, effects_plot_data = eff))
}
