# Load the testthat library
library(testthat)

# Define test cases for the check_normality function
test_that("check_normality works correctly for normal data", {
  # Create a sample dataframe with normally distributed data
  set.seed(123)
  df_normal <- data.frame(business_days_until_appointment = rnorm(100, mean = 10, sd = 2))

  # Call the function
  result <- check_normality(df_normal, "business_days_until_appointment")

  # Check that the result is a list with mean and sd
  expect_type(result, "list")
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))

  # Check that the mean and sd are approximately correct
  expect_equal(result$mean, 10, tolerance = 0.5)
  expect_equal(result$sd, 2, tolerance = 0.5)
})

test_that("check_normality works correctly for non-normal data", {
  # Create a sample dataframe with non-normally distributed data
  set.seed(123)
  df_non_normal <- data.frame(business_days_until_appointment = rexp(100, rate = 0.1))

  # Call the function
  result <- check_normality(df_non_normal, "business_days_until_appointment")

  # Check that the result is a list with median and iqr
  expect_type(result, "list")
  expect_true("median" %in% names(result))
  expect_true("iqr" %in% names(result))

  # Check that the median and iqr are approximately correct
  expect_equal(result$median, median(df_non_normal$business_days_until_appointment), tolerance = 0.5)
  expect_equal(result$iqr, IQR(df_non_normal$business_days_until_appointment), tolerance = 0.5)
})

test_that("check_normality handles missing values correctly", {
  # Create a sample dataframe with missing values
  df_missing <- data.frame(business_days_until_appointment = c(NA, rnorm(99, mean = 10, sd = 2)))

  # Call the function
  result <- check_normality(df_missing, "business_days_until_appointment")

  # Check that the result is a list with mean and sd
  expect_type(result, "list")
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))

  # Check that the mean and sd are approximately correct
  expect_equal(result$mean, 10, tolerance = 0.5)
  expect_equal(result$sd, 2, tolerance = 0.5)
})

test_that("check_normality handles mixed data types correctly", {
  # Create a sample dataframe with mixed data types
  df_mixed <- data.frame(business_days_until_appointment = c(10, 20, "a", NA))

  # Call the function and expect an error or specific behavior
  expect_error(check_normality(df_mixed, "business_days_until_appointment"),
               regexp = "numeric")
})

test_that("check_normality works correctly for small sample size", {
  # Create a sample dataframe with less than 3 non-NA values
  df_small_sample <- data.frame(business_days_until_appointment = c(10, 20))

  # Call the function and expect an error
  expect_error(check_normality(df_small_sample, "business_days_until_appointment"),
               regexp = "Sample size must be at least 3")
})
