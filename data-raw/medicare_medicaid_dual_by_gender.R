# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(tigris)
library(readr)

# PUMS (Public Use Microdata Sample) data provides individual-level data from the ACS
# It includes variables such as health insurance status, demographic details, and more.
# Key variables for health insurance:
#   HINS1: Employer-based insurance
#   HINS2: Direct-purchase insurance
#   HINS3: Medicare
#   HINS4: Medicaid
#   HINS5: TRICARE/military health care
#   HINS6: VA health care
#   HINS7: Uninsured status

year <- 2022
survey <- "acs5"

### WORKS BUT TAKES LONG TIME!
# Uncomment the below code to fetch PUMS data directly (can take a while)
# pums_data <- tidycensus::get_pums(
#   variables = c("HINS3", "HINS4", "AGEP", "SEX"),  # Variables for Medicare, Medicaid, Age, and Gender
#   state = c("all"),                               # Fetch data for all states
#   year = year,                                    # Data year
#   survey =  survey                              # 5-year ACS
# ) %>%  readr::write_rds("data-raw/pums_data_Medicare_Medicaid_by_State.rds")

# ---- Load PUMS Data ----
pums_data <- readr::read_rds("data-raw/pums_data_Medicare_Medicaid_by_State.rds")

# ---- Summarize Medicaid Coverage by State ----
medicaid_summary <- pums_data %>%
  dplyr::mutate(medicaid_covered = if_else(as.integer(HINS4) == 1, 1, 0)) %>%
  dplyr::group_by(ST) %>%
  dplyr::summarise(
    total_population = sum(PWGTP, na.rm = TRUE),
    medicaid_population = sum(PWGTP * medicaid_covered, na.rm = TRUE),
    medicaid_rate = (medicaid_population / total_population) * 100
  )

# ---- Summarize Medicare Coverage by State ----
medicare_summary <- pums_data %>%
  dplyr::mutate(medicare_covered = if_else(as.integer(HINS3) == 1, 1, 0)) %>%
  dplyr::group_by(ST) %>%
  dplyr::summarise(
    total_population = sum(PWGTP, na.rm = TRUE),
    medicare_population = sum(PWGTP * medicare_covered, na.rm = TRUE),
    medicare_rate = (medicare_population / total_population) * 100
  )

# ---- Estimate Dual Eligibility (Medicare and Medicaid) ----
dual_eligible <- pums_data %>%
  dplyr::group_by(ST) %>%
  dplyr::mutate(
    dual_status = if_else(HINS3 == 1 & HINS4 == 1, "Dual Eligible", "Not Dual")
  ) %>%
  dplyr::count(dual_status)

# ---- Function to Calculate Rates by Gender ----
calculate_rates <- function(data, gender_value) {
  data %>%
    dplyr::filter(SEX == gender_value) %>%
    dplyr::mutate(
      medicaid_covered = if_else(HINS4 == "1", 1, 0),
      medicare_covered = if_else(HINS3 == "1", 1, 0),
      dual_eligible = if_else(HINS3 == "1" & HINS4 == "1", 1, 0)
    ) %>%
    dplyr::group_by(ST) %>%
    dplyr::summarise(
      total_population = sum(PWGTP, na.rm = TRUE),
      medicaid_population = sum(PWGTP * medicaid_covered, na.rm = TRUE),
      medicare_population = sum(PWGTP * medicare_covered, na.rm = TRUE),
      dual_population = sum(PWGTP * dual_eligible, na.rm = TRUE),
      medicaid_rate = medicaid_population / total_population * 100,
      medicare_rate = medicare_population / total_population * 100,
      dual_rate = dual_population / total_population * 100,
      .groups = "drop"
    ) %>%
    dplyr::rename_with(~ paste0(if (gender_value == "1") "male_" else "female_", .), -ST)
}

# ---- Calculate Rates by Gender ----
male_summary <- calculate_rates(pums_data, "1")
female_summary <- calculate_rates(pums_data, "2")

# ---- Merge Male and Female Summaries ----
combined_summary <- male_summary %>%
  dplyr::full_join(female_summary, by = "ST")

# ---- Convert FIPS Codes to State Names ----
state_mapping <- tigris::fips_codes %>%
  dplyr::select(ST = state_code, state_name) %>%
  dplyr::distinct()

# ---- Add State Names and Metadata ----
medicare_medicaid_dual_by_gender <- combined_summary %>%
  dplyr::left_join(state_mapping, by = "ST") %>%
  dplyr::select(state_name, everything(), -ST) %>%
  dplyr::mutate(
    survey = survey,
    year = year,
    date_time_created = Sys.time()
  ) %>%
  dplyr::mutate(across(
    .cols = dplyr::ends_with("_rate"),
    .fns = ~ round(., 3)
  ))

# ---- Print the Final Summary ----
print(medicare_medicaid_dual_by_gender)
