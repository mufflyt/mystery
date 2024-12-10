library(tidycensus)
library(dplyr)
library(tidyr)

#census_api_key("your_api_key", install = TRUE)

year <- 2022
survey <- "acs5"

# Fetch data for men and women
men_and_women_by_zip <- get_acs(
  geography = "zip code tabulation area",
  variables = c(
    total_men = "B01001_002E",
    total_women = "B01001_026E"
  ),
  year = year,  # Replace with desired year
  survey = survey
) %>%
  # Reshape the data for a cleaner format
  select(GEOID, variable, estimate) %>%
  tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(
    zip_code = GEOID,
    total_men = B01001_002,
    total_women = B01001_026
  )

men_and_women_by_zip <- men_and_women_by_zip %>%
  mutate(total_population = total_men + total_women,
         year = year,
         survey = survey,
         date_time_created = Sys.time(),
         acs_codes = "Men: B01001_002, Women: B01001_026")

# Check the dataset
head(men_and_women_by_zip)

