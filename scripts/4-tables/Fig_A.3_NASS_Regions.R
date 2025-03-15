source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Fig A.3: NASS Regions
# NASS regions derived from https://www.nass.usda.gov/Statistics_by_State/RFO/index.php

# Create region variable
Region <- c("Northeastern", "Eastern Mountain", "Southern", "Great Lakes",
            "Upper Midwest", "Heartland", "Delta", "Northern Plains",
            "Southern Plains", "Mountain", "Northwest", "Pacific", "Island Territories")

# Create matching state variable
States_Territories <- c("Pennsylvania, Delaware, Maryland, New Jersey, New York, 
                        Maine, Vermont, New Hampshire, Rhode Island, Massachusetts, 
                        & Connecticut",
                        "Kentucky, North Carolina, Tennessee, Virginia, & West Virginia",
                        "Georgia, Alabama, Florida, & South Carolina",
                        "Michigan, Indiana, & Ohio",
                        "Iowa, Minnesota, & Wisconsin",
                        "Missouri & Illinois",
                        "Arkansas, Louisianca, & Mississippi",
                        "Nebraska, Kansas, North Dakota, & South Dakota",
                        "Texas & Oklahoma",
                        "Colorado, Arizona, Montana, New Mexico, Utah, & Wyoming",
                        "Washington, Alaska, Idaho, & Oregon",
                        "California, Hawaii, & Nevada",
                        "Guam, Puerto Rico, American Samoa, Northern Mariana Islands, & U.S. Virgin Islands")

# Create dataframe integrating these two variables
NASS_Regions <- data.frame(Region = Region, State_Territories = States_Territories)

# Remove D.C.
us_map <- us_states %>%
  filter(NAME != "District of Columbia")

# Define regions
Region <- c("Northeastern", "Northeastern", "Northeastern", "Northeastern", "Northeastern", 
            "Northeastern", "Northeastern", "Northeastern", "Northeastern", "Northeastern", 
            "Northeastern", 
            "Eastern Mountain", "Eastern Mountain", "Eastern Mountain", 
            "Eastern Mountain", "Eastern Mountain", 
            "Southern", "Southern", "Southern", "Southern",
            "Great Lakes", "Great Lakes", "Great Lakes",
            "Upper Midwest", "Upper Midwest", "Upper Midwest",
            "Heartland", "Heartland", 
            "Delta", "Delta", "Delta", 
            "Northern Plains", "Northern Plains", "Northern Plains", "Northern Plains",
            "Southern Plains", "Southern Plains",
            "Mountain", "Mountain", "Mountain", "Mountain", "Mountain", "Mountain",
            "Northwest", "Northwest", "Northwest", "Northwest",
            "Pacific", "Pacific", "Pacific"
            #"Island Territories"
            )

NAME <- c("Pennsylvania", "Delaware", "Maryland", "New Jersey", "New York", 
                        "Maine", "Vermont", "New Hampshire", "Rhode Island", "Massachusetts", 
                        "Connecticut",
                        "Kentucky", "North Carolina", "Tennessee", "Virginia", "West Virginia",
                        "Georgia", "Alabama", "Florida", "South Carolina",
                        "Michigan", "Indiana", "Ohio",
                        "Iowa", "Minnesota", "Wisconsin",
                        "Missouri", "Illinois",
                        "Arkansas", "Louisiana", "Mississippi",
                        "Nebraska", "Kansas", "North Dakota", "South Dakota",
                        "Texas", "Oklahoma",
                        "Colorado", "Arizona", "Montana", "New Mexico", "Utah", "Wyoming",
                        "Washington", "Alaska", "Idaho", "Oregon",
                        "California", "Hawaii", "Nevada")

NASS_Regions_2 <- data.frame(Region, NAME)

# Combine dataframe of regional groupings with the map

us_map_regions <- us_map %>%
  left_join(NASS_Regions_2, by = "NAME") %>%
  mutate(Region = factor(Region, levels = c("Northeastern", "Eastern Mountain",
                                            "Southern", "Great Lakes", "Upper Midwest",
                                            "Heartland", "Delta", "Northern Plains",
                                            "Southern Plains", "Mountain", "Northwest",
                                            "Pacific")))

# Plot it
NASS_regions <- ggplot(us_map_regions) + geom_sf(aes(fill = Region)) + 
  scale_fill_viridis(discrete = TRUE, breaks = c("Northeastern", "Eastern Mountain",
                                            "Southern", "Great Lakes", "Upper Midwest",
                                            "Heartland", "Delta", "Northern Plains",
                                            "Southern Plains", "Mountain", "Northwest",
                                            "Pacific")) + 
  theme_void(base_size = 16) + labs(title = "NASS Regions") +
  theme(legend.position = "bottom")

NASS_regions
