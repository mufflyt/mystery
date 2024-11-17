# ENT Board of Governors Regions DataFrame with Idaho in Region 9
ent_bog_regions <- data.frame(
  State = c(
    "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
    "New Jersey", "New York", "Puerto Rico", "U.S. Virgin Islands",
    "Delaware", "District of Columbia", "Maryland", "Pennsylvania", "Virginia", "West Virginia",
    "Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee",
    "Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin",
    "Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas",
    "Iowa", "Kansas", "Missouri", "Nebraska",
    "Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming",
    "Alaska", "Oregon", "Washington", "Idaho",   # Idaho added to Region 9
    "Arizona", "California", "Hawaii", "Nevada"
  ),
  ENT_BOG_Region = c(
    rep("Region 1", 6),
    rep("Region 2", 4),
    rep("Region 3", 6),
    rep("Region 4", 8),
    rep("Region 5", 6),
    rep("Region 6", 5),
    rep("Region 7", 4),
    rep("Region 8", 6),
    rep("Region 9", 4),  # Updated to include Idaho
    rep("Region 10", 4)
  )
)

# Convert ENT_BOG_Region to factor with the appropriate order
ent_bog_regions$ENT_BOG_Region <- factor(ent_bog_regions$ENT_BOG_Region,
                                         levels = paste("Region", 1:10))

# Arrange the dataframe by ENT_BOG_Region
ent_bog_regions <- ent_bog_regions %>%
  arrange(ENT_BOG_Region)



acog_districts_df <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
            "District of Columbia", "Florida", "Georgia", "Hawaii", "Illinois", "Indiana", "Iowa",
            "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Maine", "Michigan",
            "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
            "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota",
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
            "Wyoming", "Idaho", "Delaware"),
  ACOG_District = c("District VII", "District VIII", "District VIII", "District VII", "District IX",
                    "District VIII", "District I", "District IV", "District XII", "District IV",
                    "District VIII", "District VI", "District V", "District VI", "District VII",
                    "District V", "District VII", "District IV", "District I", "District I", "District V",
                    "District VI", "District VII", "District VII", "District VIII", "District VI", "District VIII",
                    "District I", "District III", "District VIII", "District II", "District IV",
                    "District VI", "District V", "District VII", "District VIII", "District III",
                    "District IV", "District I", "District IV", "District VI", "District VII",
                    "District XI", "District VIII", "District I", "District IV", "District VIII",
                    "District IV", "District VI", "District VI", "District VIII", "District III")
)

# Set ACOG_District as a factor with the specified levels
acog_districts_df$ACOG_District <- factor(acog_districts_df$ACOG_District,
                                          levels = c("District I", "District II", "District III", "District IV",
                                                     "District V", "District VI", "District VII", "District VIII",
                                                     "District IX", "District X", "District XI", "District XII"))


# Create a data frame with the region and division information
us_census_bureau_regions_df <- data.frame(
  State = c("Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", "Connecticut",
            "District of Columbia", "Delaware", "Florida", "Georgia", "Hawaii", "Iowa", "Idaho",
            "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", "Maryland",
            "Maine", "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana", "North Carolina",
            "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada",
            "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Vermont", "Washington",
            "Wisconsin", "West Virginia", "Wyoming"),
  `State Code` = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA",
                 "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS",
                 "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA",
                 "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
  Region = c("West", "South", "South", "West", "West", "West", "Northeast", "South", "South",
             "South", "South", "West", "Midwest", "West", "Midwest", "Midwest", "Midwest",
             "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest", "Midwest",
             "South", "West", "South", "Midwest", "Midwest", "Northeast", "Northeast", "West",
             "West", "Northeast", "Midwest", "South", "West", "Northeast", "Northeast", "South",
             "Midwest", "South", "South", "West", "South", "Northeast", "West", "Midwest", "South", "West"),
  Division = c("Pacific", "East South Central", "West South Central", "Mountain", "Pacific", "Mountain",
               "New England", "South Atlantic", "Middle Atlantic", "South Atlantic", "South Atlantic",
               "Pacific", "West North Central", "Mountain", "East North Central", "East North Central",
               "West North Central", "East South Central", "West South Central", "New England",
               "South Atlantic", "New England", "East North Central", "West North Central", "West North Central",
               "East South Central", "Mountain", "South Atlantic", "West North Central", "West North Central",
               "New England", "Middle Atlantic", "Mountain", "Mountain", "Middle Atlantic",
               "East North Central", "West South Central", "Pacific", "Middle Atlantic", "New England",
               "South Atlantic", "West North Central", "East South Central", "West South Central",
               "Mountain", "South Atlantic", "New England", "Pacific", "East North Central", "South Atlantic",
               "Mountain")
)
