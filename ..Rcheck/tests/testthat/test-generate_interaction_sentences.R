# Load necessary libraries
library(lme4)
library(emmeans)
library(dplyr)
library(testthat)

# Create synthetic data for testing
set.seed(123)
df3_filtered <- data.frame(
  business_days_until_appointment = rpois(200, lambda = 15),  # Poisson-distributed waiting times
  scenario = factor(sample(c("HIP scenario", "KNEE scenario", "SHOULDER scenario"), 200, replace = TRUE)),
  insurance = factor(sample(c("Blue Cross/Blue Shield", "Medicaid"), 200, replace = TRUE)),
  last = factor(rep(1:20, each = 10))  # Random effect grouping variable
)

# Define a helper function to create the interaction model
create_interaction_model <- function(data) {
  glmer(
    business_days_until_appointment ~ scenario * insurance + (1 | last),
    data = data,
    family = poisson(link = "log")
  )
}

# Create the interaction model
interaction_model <- create_interaction_model(df3_filtered)

# Run tests
test_that("Function handles unsupported output formats correctly", {
  expect_error(
    generate_interaction_sentences(interaction_model, "scenario", "insurance", output_format = "html"),
    "Unsupported output format. Please use 'text' or 'markdown'."
  )
})
