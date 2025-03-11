# Load necessary libraries
library(lme4)
library(emmeans)
library(dplyr)
library(testthat)

# Create synthetic data for testing
set.seed(123)
df3_filtered <- data.frame(
  business_days_until_appointment = rpois(200, lambda = 15), # Poisson-distributed waiting times
  scenario = factor(sample(c("HIP scenario", "KNEE scenario", "SHOULDER scenario"), 200, replace = TRUE)),
  insurance = factor(sample(c("Blue Cross/Blue Shield", "Medicaid"), 200, replace = TRUE)),
  last = factor(rep(1:20, each = 10)) # Random effect grouping variable
)

# Define a helper function to create the interaction model
create_interaction_model <- function(data) {
  glmer(
    business_days_until_appointment ~ scenario * insurance + (1 | last),
    data = data,
    family = poisson(link = "log")
  )
}

# Define a dummy function for testing purposes
generate_interaction_sentences <- function(model, factor1, factor2, output_format = "text") {
  if (!output_format %in% c("text", "markdown")) {
    stop("Unsupported output format. Please use 'text' or 'markdown'.")
  }
  return("Dummy sentence for testing")
}

# Run tests inside the test_that() block
test_that("Function handles unsupported output formats correctly", {
  # Create the interaction model inside the test
  interaction_model <- create_interaction_model(df3_filtered)

  # Test for unsupported output format
  expect_error(
    generate_interaction_sentences(interaction_model, "scenario", "insurance", output_format = "html"),
    "Unsupported output format. Please use 'text' or 'markdown'."
  )
})

# Additional test for supported output format
test_that("Function handles supported output formats correctly", {
  # Create the interaction model inside the test
  interaction_model <- create_interaction_model(df3_filtered)

  # Test for a supported output format
  result <- generate_interaction_sentences(interaction_model, "scenario", "insurance", output_format = "text")

  expect_type(result, "character")
  expect_true(grepl("Dummy sentence", result))
})
