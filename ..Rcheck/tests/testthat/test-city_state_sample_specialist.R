library(testthat)
library(dplyr)
library(logger)


# Sample data for testing
# Sample data for testing
# Sample data for testing
# Sample data for testing
test_data <- data.frame(
  city = rep(c("Abbeville", "New York", "Los Angeles"), each = 10),
  state_code = rep(c("SC", "NY", "CA"), each = 10),
  specialty_primary = c(
    # 10 entries for Abbeville, SC (5 General Dermatology)
    rep("General Dermatology", 5),    # 5 General Dermatology in Abbeville
    rep(NA, 5),                       # No specialists in Abbeville

    # 10 entries for New York, NY (3 General Dermatology, 7 Pediatric Dermatology)
    rep("General Dermatology", 3),    # 3 General Dermatology in New York
    rep("Pediatric Dermatology", 7),  # 7 Pediatric Dermatology in New York

    # 10 entries for Los Angeles, CA (4 General Dermatology, 6 Pediatric Dermatology)
    rep("General Dermatology", 4),    # 4 General Dermatology in Los Angeles
    rep("Pediatric Dermatology", 6)   # 6 Pediatric Dermatology in Los Angeles
  ),
  stringsAsFactors = FALSE
)

# View the created test data
print(test_data)



# Define the test
test_that("city_state_sample_specialists filters and samples correctly", {

  # Test 1: Function returns correct sample for valid input
  result <- city_state_sample_specialists(
    data = test_data,
    generalist = "General Dermatology",
    specialist1 = "Pediatric Dermatology",
    general_sample_size = 4,
    specialist1_sample_size = 2,
    seed = 1978
  )

  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the correct number of cities and state_code combinations meet the requirements
  filtered_data <- test_data %>%
    group_by(city, state_code) %>%
    summarize(
      num_generalists = sum(specialty_primary == "General Dermatology"),
      num_specialists1 = sum(specialty_primary == "Pediatric Dermatology")
    ) %>%
    filter(num_generalists >= 4 & num_specialists1 >= 2)

  expect_equal(n_distinct(result$city), nrow(filtered_data))

  # Check if all rows in the result match the city-state combinations that meet the criteria
  valid_city_state <- filtered_data %>% select(city, state_code)
  result_city_state <- result %>% select(city, state_code) %>% distinct()

  expect_true(all(result_city_state$city %in% valid_city_state$city))
  expect_true(all(result_city_state$state_code %in% valid_city_state$state_code))
})

# Test 2: Function returns an empty data frame if no city-state combinations meet the criteria
test_that("city_state_sample_specialists returns empty data frame for invalid input", {

  result <- city_state_sample_specialists(
    data = test_data,
    generalist = "General Dermatology",
    specialist1 = "Pediatric Dermatology",
    general_sample_size = 10,   # Intentionally large to avoid matches
    specialist1_sample_size = 5, # Intentionally large to avoid matches
    seed = 1978
  )

  # Check if the result is an empty data frame
  expect_true(nrow(result) == 0)
})

# Test 3: Check if the function handles NULL specialist2 and specialist3 correctly
test_that("city_state_sample_specialists handles NULL for optional specialists", {

  result <- city_state_sample_specialists(
    data = test_data,
    generalist = "General Dermatology",
    specialist1 = "Pediatric Dermatology",
    general_sample_size = 4,
    specialist1_sample_size = 2,
    specialist2 = NULL,
    specialist2_sample_size = 0,
    specialist3 = NULL,
    specialist3_sample_size = 0,
    seed = 1978
  )

  # Ensure the result contains rows
  expect_true(nrow(result) > 0)

  # Ensure the result does not contain additional specialists
  expect_false("specialist2" %in% colnames(result))
  expect_false("specialist3" %in% colnames(result))
})

# Test 4: Check output when invalid input (no generalist or specialist found)
test_that("city_state_sample_specialists handles invalid input (no specialists)", {

  result <- city_state_sample_specialists(
    data = test_data,
    generalist = "Nonexistent Generalist",  # No such generalist in data
    specialist1 = "Nonexistent Specialist", # No such specialist in data
    general_sample_size = 1,
    specialist1_sample_size = 1,
    seed = 1978
  )

  # Expect result to be empty since no such generalist or specialist exists
  expect_true(nrow(result) == 0)
})

