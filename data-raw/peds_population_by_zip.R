library(tidycensus)
library(dplyr)

year <- 2022
survey <- "acs5"

# Get ACS data for a specific county
peds_population_by_zip_acs_data <- tidycensus::get_acs(
                    geography = "zip code tabulation area",
                    variables = c(male_under_5 = "B01001_003",
                                  female_under_5 = "B01001_004",
                                  male_5_to_9 = "B01001_005",
                                  female_5_to_9 = "B01001_006",
                                  male_10_to_14 = "B01001_007",
                                  female_10_to_14 = "B01001_008",
                                  male_15_to_17 = "B01001_009",
                                  female_15_to_17 = "B01001_010",
                                  male_18_to_19 = "B01001_011",
                                  female_18_to_19 = "B01001_012",
                                  male_20 = "B01001_013",
                                  female_20 = "B01001_014"),
                    year = year,
                    survey = survey) %>%
  # Reshape the data for a cleaner format
  select(GEOID, variable, estimate)

peds_population_by_zip_acs_data_summarised <- peds_population_by_zip_acs_data %>%
  group_by(GEOID) %>%
  summarise(total_under_21 = sum(estimate))

#readr::write_rds(peds_population_by_zip_acs_data_summarised, "/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/data/reference/peds_population_by_zip_acs_data_summarised.rds")
