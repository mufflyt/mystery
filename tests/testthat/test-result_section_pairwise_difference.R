library(testthat)
library(tibble)

# Sample data for testing
test_data <- tibble::tibble(
  scenario = rep(c("Group_A", "Group_B", "Group_C"), each = 30),
  measurement = c(rnorm(30, mean = 10), rnorm(30, mean = 15), rnorm(30, mean = 20))
)

# Define tests
test_that("result_section_pairwise_difference handles valid input correctly", {
  result <- result_section_pairwise_difference(
    scenario_data = test_data,
    numeric_var = "measurement",
    x_var = "scenario"
  )

  # Check if result is a list
  expect_type(result, "list")

  # Check if result contains expected keys
  expect_named(result, c("pairwise_trends", "trend_summary", "interpretation_sentences"))

  # Check if pairwise_trends is a tibble with expected columns
  expect_s3_class(result$pairwise_trends, "tbl_df")
  expect_named(result$pairwise_trends, c("Group1", "Group2", "Direction", "p_value", "p_value_formatted"))

  # Check if interpretation_sentences is a character vector
  expect_type(result$interpretation_sentences, "character")
  expect_gt(length(result$interpretation_sentences), 0)
})

test_that("result_section_pairwise_difference calculates correct medians and trends", {
  result <- result_section_pairwise_difference(
    scenario_data = test_data,
    numeric_var = "measurement",
    x_var = "scenario"
  )

  # Extract summary stats
  medians <- test_data %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarise(Median = median(measurement, na.rm = TRUE), .groups = "drop")

  # Check if medians match
  for (group in unique(test_data$scenario)) {
    computed_median <- medians %>% dplyr::filter(scenario == group) %>% dplyr::pull(Median)
    expect_equal(
      result$pairwise_trends %>%
        dplyr::filter(Group1 == group | Group2 == group) %>%
        nrow(),
      nrow(test_data %>% dplyr::filter(scenario == group))
    )
  }
})

test_that("result_section_pairwise_difference handles single-group data gracefully", {
  single_group_data <- tibble::tibble(
    scenario = rep("Group_A", 30),
    measurement = rnorm(30, mean = 10)
  )

  result <- result_section_pairwise_difference(
    scenario_data = single_group_data,
    numeric_var = "measurement",
    x_var = "scenario"
  )

  # Check that pairwise_trends is empty
  expect_equal(nrow(result$pairwise_trends), 0)

  # Check that interpretation_sentences is empty
  expect_equal(length(result$interpretation_sentences), 0)
})

test_that("result_section_pairwise_difference handles missing values", {
  data_with_na <- test_data
  data_with_na$measurement[1:5] <- NA

  result <- result_section_pairwise_difference(
    scenario_data = data_with_na,
    numeric_var = "measurement",
    x_var = "scenario"
  )

  # Check that missing values are handled correctly
  expect_s3_class(result$pairwise_trends, "tbl_df")
  expect_gt(nrow(result$pairwise_trends), 0)

  # Check that interpretation_sentences are still generated
  expect_type(result$interpretation_sentences, "character")
  expect_gt(length(result$interpretation_sentences), 0)
})

test_that("result_section_pairwise_difference handles non-numeric variables gracefully", {
  expect_error(
    result_section_pairwise_difference(
      scenario_data = test_data,
      numeric_var = "scenario",  # Non-numeric variable
      x_var = "measurement"
    ),
    "not numeric"
  )
})

test_that("result_section_pairwise_difference handles non-existent columns", {
  expect_error(
    result_section_pairwise_difference(
      scenario_data = test_data,
      numeric_var = "nonexistent_column",
      x_var = "scenario"
    ),
    "nonexistent_column"
  )

  expect_error(
    result_section_pairwise_difference(
      scenario_data = test_data,
      numeric_var = "measurement",
      x_var = "nonexistent_column"
    ),
    "nonexistent_column"
  )
})
