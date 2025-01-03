# Load necessary libraries
library(readxl)
library(dplyr)
library(tigris)
library(tibble)

# Load RUCA codes data
ruca_data <- readxl::read_excel("data-raw/ruca2010revised.xlsx", skip = 1)

# Load ZIP-to-tract crosswalk data
zip_to_tract <- readxl::read_excel("data-raw/ZIP_TRACT_092024.xlsx")

# Clean RUCA data
ruca_clean <- ruca_data %>%
  dplyr::select(
    tract_fips = `State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`,
    primary_ruca = `Primary RUCA Code 2010`,
    secondary_ruca = `Secondary RUCA Code, 2010 (see errata)`
  ) %>%
  dplyr::mutate(tract_fips = as.character(tract_fips))

# Clean ZIP-to-tract data
zip_to_tract_clean <- zip_to_tract %>%
  dplyr::filter(RES_RATIO > 0) %>%
  dplyr::select(
    zip = ZIP,
    tract_fips = TRACT,
    res_ratio = RES_RATIO,
    USPS_ZIP_PREF_CITY,
    USPS_ZIP_PREF_STATE
  ) %>%
  dplyr::mutate(
    zip = as.character(zip),
    tract_fips = as.character(tract_fips)
  )

# Merge datasets
zip_to_ruca <- zip_to_tract_clean %>%
  dplyr::left_join(ruca_clean, by = "tract_fips")

# Map state abbreviations to full state names using tigris::fips_codes
state_mapping <- tigris::fips_codes %>%
  dplyr::select(state_code = state, state_name) %>%
  dplyr::distinct()

# Add full state names to the dataset
zip_to_ruca <- zip_to_ruca %>%
  dplyr::left_join(state_mapping, by = c("USPS_ZIP_PREF_STATE" = "state_code")) %>%
  dplyr::select(-USPS_ZIP_PREF_STATE) %>%
  mutate(USPS_ZIP_PREF_CITY = stringr::str_to_title(USPS_ZIP_PREF_CITY))

# RUCA code descriptions
ruca_descriptions <- tibble::tibble(
  ruca_code = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  description = c(
    "Urban area",
    "Large rural town",
    "Small rural town",
    "Isolated rural area",
    "Urban commuting area",
    "Small rural commuting area",
    "Large rural commuting area",
    "Isolated rural commuting area",
    "Urban cluster",
    "Remote rural area"
  )
)

# Replace numeric RUCA codes with descriptions
zip_to_ruca <- zip_to_ruca %>%
  dplyr::left_join(ruca_descriptions, by = c("primary_ruca" = "ruca_code")) %>%
  dplyr::rename(primary_ruca_desc = description) %>%
  dplyr::left_join(ruca_descriptions, by = c("secondary_ruca" = "ruca_code")) %>%
  dplyr::rename(secondary_ruca_desc = description)

# Define RUCA categories based on AHRQ guidelines
zip_to_ruca <- zip_to_ruca %>%
  dplyr::mutate(
    AHRQ_rural_vs_urban = dplyr::case_when(
      primary_ruca %in% c(1.0, 1.1, 2.0, 2.1, 3.0, 4.1, 5.1, 7.1, 8.1, 10.1) ~ "Urban",
      primary_ruca %in% c(4.0, 4.2, 5.0, 5.2, 6.0, 6.1) ~ "Large rural town",
      primary_ruca %in% c(7.0, 7.2, 7.3, 7.4, 8.0, 8.2, 8.3, 8.4, 9.0, 9.1, 9.2) ~ "Small rural town",
      primary_ruca %in% c(10.0, 10.2, 10.3, 10.4, 10.5, 10.6) ~ "Isolated rural",
      TRUE ~ NA_character_
    )
  )

# Reorder the columns in the zip_to_ruca dataset
zip_to_ruca <- zip_to_ruca %>%
  dplyr::select(
    zip,
    USPS_ZIP_PREF_CITY,
    state_name,
    AHRQ_rural_vs_urban,
    primary_ruca_desc,
    primary_ruca,
    secondary_ruca,
    secondary_ruca_desc,
    tract_fips,
    res_ratio
  )

# To keep only distinct zip variables while retaining the mode of primary_ruca_desc
get_mode_row <- function(data, column) {
  mode_value <- get_mode(data[[column]])
  mode_row <- data[data[[column]] == mode_value, ][1, ] # Select the first row with the mode
  return(mode_row)
}

zip_to_ruca_distinct <- zip_to_ruca %>%
  dplyr::group_by(zip) %>%
  dplyr::summarise(
    # Find the row corresponding to the mode of 'primary_ruca_desc'
    primary_mode_row = list(get_mode_row(dplyr::cur_data(), "primary_ruca_desc"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(primary_mode_row = purrr::map(primary_mode_row, as_tibble)) %>%
  tidyr::unnest(primary_mode_row)


# View the updated dataset
print(zip_to_ruca_distinct, n = 10)

zip_to_ruca <- zip_to_ruca_distinct
