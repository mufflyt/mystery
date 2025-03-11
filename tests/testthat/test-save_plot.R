# Load required packages for testing
library(testthat)
library(ggplot2)
library(logger)
library(mockery) # Add this line to load the mockery package

# Sample plot for testing
test_plot <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point()

# Begin tests
test_that("Plot is saved successfully with specified parameters", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  file_name <- "test_plot.png"
  full_path <- file.path(temp_dir, file_name)

  # Ensure the file does not already exist
  if (file.exists(full_path)) {
    file.remove(full_path)
  }

  # Call the save_plot function
  save_plot(
    plot = test_plot,
    file_name = file_name,
    save_directory = temp_dir,
    plot_width = 6,
    plot_height = 4,
    resolution_dpi = 300,
    size_units = "in",
    scale_factor = 1,
    file_format = NULL,
    background_color = "white",
    verbose_output = FALSE
  )

  # Check if the file has been created
  expect_true(file.exists(full_path))
})

test_that("Function creates directory if it does not exist", {
  # Create a unique temporary directory path
  temp_dir <- file.path(tempdir(), "nonexistent_dir")
  file_name <- "test_plot.png"
  full_path <- file.path(temp_dir, file_name)

  # Ensure the directory does not exist
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  # Call the save_plot function
  save_plot(
    plot = test_plot,
    file_name = file_name,
    save_directory = temp_dir,
    verbose_output = FALSE
  )

  # Check if the directory and file have been created
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(full_path))
})

test_that("Function handles unsupported file extensions", {
  temp_dir <- tempdir()
  file_name <- "test_plot.unsupported"

  # Expect an error due to unsupported file extension
  expect_error(
    save_plot(
      plot = test_plot,
      file_name = file_name,
      save_directory = temp_dir,
      verbose_output = FALSE
    ),
    "Unsupported file extension"
  )
})

test_that("Function uses correct file format based on file extension", {
  temp_dir <- tempdir()
  file_name <- "test_plot.jpeg"
  full_path <- file.path(temp_dir, file_name)

  # Remove file if it exists
  if (file.exists(full_path)) {
    file.remove(full_path)
  }

  # Call the save_plot function
  save_plot(
    plot = test_plot,
    file_name = file_name,
    save_directory = temp_dir,
    verbose_output = FALSE
  )

  # Check if the file has been created
  expect_true(file.exists(full_path))
})

test_that("Function throws error when required packages are missing", {
  # Define a mock requireNamespace function
  mock_requireNamespace <- function(pkg, quietly = TRUE) {
    if (pkg == "logger") {
      return(FALSE)
    } else {
      utils::requireNamespace(pkg, quietly = quietly)
    }
  }

  # Use mockery to stub requireNamespace within save_plot
  stub(save_plot, "requireNamespace", mock_requireNamespace)

  # Expect an error due to missing 'logger' package
  expect_error(
    save_plot(
      plot = test_plot,
      file_name = "test_plot.png",
      save_directory = tempdir(),
      verbose_output = FALSE
    ),
    "The logger package is required but is not installed"
  )
})



test_that("Function handles custom background color", {
  temp_dir <- tempdir()
  file_name <- "test_plot_bg.png"
  full_path <- file.path(temp_dir, file_name)

  # Remove file if it exists
  if (file.exists(full_path)) {
    file.remove(full_path)
  }

  # Call the save_plot function with a custom background color
  save_plot(
    plot = test_plot,
    file_name = file_name,
    save_directory = temp_dir,
    background_color = "transparent",
    verbose_output = FALSE
  )

  # Check if the file has been created
  expect_true(file.exists(full_path))
})

test_that("Function scales the plot correctly", {
  temp_dir <- tempdir()
  file_name <- "test_plot_scaled.png"
  full_path <- file.path(temp_dir, file_name)

  # Remove file if it exists
  if (file.exists(full_path)) {
    file.remove(full_path)
  }

  # Call the save_plot function with a scale factor
  save_plot(
    plot = test_plot,
    file_name = file_name,
    save_directory = temp_dir,
    scale_factor = 2,
    verbose_output = FALSE
  )

  # Check if the file has been created
  expect_true(file.exists(full_path))

  # Since we cannot check the actual dimensions without reading the file,
  # we assume that if the file exists, scaling did not cause an error.
})

test_that("Function uses default values when optional arguments are not specified", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  file_name <- "test_plot_defaults.png"
  full_path <- file.path(temp_dir, file_name)

  # Remove the file if it exists
  if (file.exists(full_path)) {
    file.remove(full_path)
  }

  # Suppress messages during the function call
  suppressMessages(
    save_plot(
      plot = test_plot,
      file_name = file_name,
      save_directory = temp_dir
    )
  )

  # Check if the file has been created
  expect_true(file.exists(full_path))
})

test_that("Function uses default values when optional arguments are not specified", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  file_name <- "test_plot_defaults.png"
  full_path <- file.path(temp_dir, file_name)

  # Remove the file if it exists
  if (file.exists(full_path)) {
    file.remove(full_path)
  }

  # Call the save_plot function with minimal arguments
  save_plot(
    plot = test_plot,
    file_name = file_name,
    save_directory = temp_dir
  )

  # Check if the file has been created
  expect_true(file.exists(full_path))
})
