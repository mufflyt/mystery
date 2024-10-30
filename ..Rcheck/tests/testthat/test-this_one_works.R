# Load necessary libraries
library(conflicted)
library(testthat)
library(mockery)
library(dplyr)
library(readr)
library(httr)
library(jsonlite)

# Resolve conflicts before loading other libraries
conflicted::conflicts_prefer(purrr::flatten)

# Corrected mock functions
mock_get <- function(url, ...) {
  cat("Mock GET request to URL:", url, "\n")
  if (grepl("verify", url)) {
    response <- list(status_code = 200, content = function(...) '{"name":"Dr. John Doe"}')
  } else {
    response <- list(status_code = 404, content = function(...) '')
  }
  class(response) <- "response"
  return(response)
}

mock_fromJSON <- function(text, ...) {
  cat("Mock fromJSON called\n")
  return(if (nchar(text) > 0) list(name = "Dr. John Doe") else list())
}

mock_read_csv <- function(...) {
  cat("Mock read_csv called\n")
  return(data.frame(WrongIDs = c(9045998, 9045999)))
}

# Tests
test_that("Mock GET request works for valid URL", {
  mockery::stub(httr::GET, 'httr::GET', mock_get)
  response <- httr::GET("https://api.example.com/verify")
  expect_equal(response$status_code, 200)
  expect_equal(jsonlite::fromJSON(response$content())$name, "Dr. John Doe")
})

test_that("Mock GET request fails for invalid URL", {
  response <- mock_get("https://api.example.com/invalid")
  expect_equal(response$status_code, 404)
  expect_equal(jsonlite::fromJSON(response$content())$name, "")
})

test_that("Mock read_csv is called", {
  mockery::stub(readr::read_csv, 'readr::read_csv', mock_read_csv)
  df <- readr::read_csv("path/to/nonexistent.csv")
  expect_equal(nrow(df), 2)  # Check that the mock returns 2 rows
  expect_equal(df$WrongIDs, c(9045998, 9045999))  # Check the content of the mock
})
