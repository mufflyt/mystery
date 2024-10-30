# Load necessary libraries
library(testthat)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(conflicted)

# Resolve package conflicts
conflicted::conflicts_prefer(dplyr::matches)

# Define a test file to encapsulate all test cases
test_that("Scatter plot creation tests", {
  # Define a mock dataset for testing
  df_mock <- data.frame(
    phone = rep(1:100, each = 2),
    insurance = rep(c("Medicaid", "Blue Cross/Blue Shield"), 100),
    business_days_until_appointment = runif(200, 1, 30)
  )

  # Modify the insurance variable to test cleaning steps (trim whitespace and lowercase)
  df_mock$insurance <- ifelse(df_mock$insurance == "Medicaid", " Medicaid ", "Blue Cross/Blue Shield")

  # Test: Function creates scatter plot correctly with default settings
  plot <- create_insurance_by_insurance_scatter_plot(
    df = df_mock,
    unique_variable = "phone"
  )
  expect_s3_class(plot, "ggplot")
  expect_true(file.exists("ortho_sports_med/figures/scatterplot.tiff"))
  expect_true(file.exists("ortho_sports_med/figures/scatterplot.png"))

  # Test: Function creates scatter plot correctly with custom settings
  plot <- create_insurance_by_insurance_scatter_plot(
    df = df_mock,
    unique_variable = "phone",
    insurance1 = "medicaid",
    insurance2 = "blue cross/blue shield",
    output_directory = "custom_directory/",
    dpi = 300,
    height = 10,
    width = 15,
    x_label = "Custom X Label",
    y_label = "Custom Y Label",
    plot_title = "Custom Plot Title",
    point_size = 4,
    point_alpha = 0.8,
    add_confidence_interval = FALSE
  )
  expect_s3_class(plot, "ggplot")
  expect_true(file.exists("custom_directory/scatterplot.tiff"))
  expect_true(file.exists("custom_directory/scatterplot.png"))

  # Test: Function handles missing or non-existent output directory
  plot <- create_insurance_by_insurance_scatter_plot(
    df = df_mock,
    unique_variable = "phone",
    output_directory = "non_existent_directory/"
  )
  expect_s3_class(plot, "ggplot")
  expect_true(file.exists("non_existent_directory/scatterplot.tiff"))
  expect_true(file.exists("non_existent_directory/scatterplot.png"))

  # Test: Function handles missing data appropriately
  df_missing <- df_mock
  df_missing$business_days_until_appointment[sample(1:nrow(df_missing), 10)] <- NA

  plot <- create_insurance_by_insurance_scatter_plot(
    df = df_missing,
    unique_variable = "phone"
  )
  expect_s3_class(plot, "ggplot")
  expect_true(file.exists("ortho_sports_med/figures/scatterplot.tiff"))
  expect_true(file.exists("ortho_sports_med/figures/scatterplot.png"))

  # Test: Function correctly cleans insurance variable
  df_cleaned <- df_mock %>%
    mutate(insurance = str_trim(insurance),
           insurance = tolower(insurance))

  plot <- create_insurance_by_insurance_scatter_plot(
    df = df_cleaned,
    unique_variable = "phone"
  )
  expect_s3_class(plot, "ggplot")
  expect_true(file.exists("ortho_sports_med/figures/scatterplot.tiff"))
  expect_true(file.exists("ortho_sports_med/figures/scatterplot.png"))

  # Test: Function throws error with invalid unique_variable
  expect_error(
    create_insurance_by_insurance_scatter_plot(df = df_mock, unique_variable = "invalid_variable"),
    "Column `invalid_variable` doesn't exist"
  )
})
