library(tigris)
library(sf)
library(dplyr)
library(ggplot2)

# Updated ACOG districts data
acog_districts <- data.frame(
  State = c(
    "Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "District of Columbia",
    "Florida", "Georgia", "Hawaii", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska",
    "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
    "Wisconsin", "Maine", "Alaska", "Idaho", "Wyoming", "Montana", "Delaware"
  ),
  ACOG_District = c(
    "District VII", "District VIII", "District VII", "District IX", "District VIII", "District I", "District IV",
    "District XII", "District IV", "District VIII", "District VI", "District V", "District VI", "District VII",
    "District V", "District VII", "District IV", "District I", "District V", "District VI", "District VII",
    "District VII", "District VI", "District VIII", "District I", "District III", "District VIII", "District II",
    "District IV", "District VI", "District V", "District VII", "District VIII", "District III", "District IV",
    "District I", "District IV", "District VI", "District VII", "District XI", "District VIII", "District I",
    "District IV", "District VIII", "District IV", "District VI", "District I", "District VIII", "District VIII",
    "District VIII", "District VIII", "District III"
  ),
  State_Abbreviation = c(
    "AL", "AZ", "AR", "CA", "CO", "CT", "DC", "FL", "GA", "HI", "IL", "IN", "IA", "KS", "KY", "LA",
    "MD", "MA", "MI", "MN", "MS", "MO", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
    "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "ME", "AK",
    "ID", "WY", "MT", "DE"
  )
)

# Load state geometries
states_sf <- tigris::states(cb = TRUE, year = 2023)

# Filter out Hawaii and Alaska
# states_sf <- states_sf %>%
#   dplyr::filter(!NAME %in% c("Hawaii", "Alaska"))

# Merge ACOG district information with the state geometries
districts_sf <- states_sf %>%
  inner_join(acog_districts, by = c("NAME" = "State"))

# Group by ACOG district and combine geometries
acog_districts_sf <- districts_sf %>%
  group_by(ACOG_District) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")


ACOG_Districts_sf <- acog_districts_sf

# Save the ACOG_Districts_sf object as data for the tyler package
# usethis::use_data(ACOG_Districts_sf, overwrite = TRUE)

# Define shift_geometry function
shift_geometry <- function(sf_obj) {
  # Shift Hawaii and Alaska for better visualization
  sf_obj %>%
    mutate(geometry = case_when(
      NAME == "Alaska" ~ st_geometry(.) + c(50, -30), # Shift Alaska
      NAME == "Hawaii" ~ st_geometry(.) + c(60, -20), # Shift Hawaii
      TRUE ~ st_geometry(.)
    ))
}

# Apply the plot
ggplot(acog_districts_sf) +
  geom_sf(aes(fill = ACOG_District), color = "black") +
  coord_sf(crs = "ESRI:102003") +
  theme_void() +
  labs(title = "ACOG Districts Across the U.S.", fill = "ACOG District")
