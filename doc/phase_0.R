## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(maps)
library(tyler)       # Custom package; ensure it’s installed
library(exploratory) 

## -----------------------------------------------------------------------------
# Loading data from the patient-facing directory
# This data represents phase 0, where phone numbers have already been confirmed.
# We will filter this data to ensure distinct entries based on name, degree, and state.
readr::read_csv(
  system.file("extdata/ortho_spine/phase_0/spine_only.csv", package = "tyler")
) %>%
  dplyr::distinct(first, middle, last, degree, state, .keep_all = TRUE)

# Load pre-searched NPI results (replace with actual search if needed)
ortho_spine_batch_npi_search <- readr::read_csv(
  system.file("extdata/ortho_spine/phase_0/npi_results.csv", package = "tyler")
)

