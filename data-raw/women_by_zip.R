install.packages("tidycensus")
install.packages("dplyr")
library(tidycensus)
library(dplyr)

# census_api_key(CENSUS_API_KEY, install = TRUE)


# Load total female population by ZIP code
women_by_zip <- get_acs(
  geography = "zip code tabulation area",
  variables = "B01001_026E",
  year = 2022,  # Replace with your desired year
  survey = "acs5"
) %>%
  dplyr::select(-moe)

# Rename and inspect the data
women_by_zip <- women_by_zip %>%
  rename(
    zip_code = GEOID,
    total_women = estimate
  )

head(women_by_zip)
