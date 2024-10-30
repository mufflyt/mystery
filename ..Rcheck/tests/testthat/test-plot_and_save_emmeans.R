library(testthat)
library(ggplot2)
library(emmeans)
library(dplyr)

message("Testing plot_and_save_emmeans function")

# Setup a basic model for testing
data(mtcars)
test_model <- lm(mpg ~ cyl + gear, data = mtcars)


# Cleanup test files if needed
unlink(list.files(tempdir(), pattern = "png$", full.names = TRUE), force = TRUE)
