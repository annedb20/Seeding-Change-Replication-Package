source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Table 4: NASS Regions
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

# Read in map data
library(sf)
us_map <- read_sf("/Users/annedb20/Downloads/states/states.geojson")
us_map <- us_map %>%
  filter(NAME != "District of Columbia")

# Shift locations of Alaska and Hawaii
alaska <- us_map %>%
  filter(NAME %in% "Alaska")
alaska_g <- st_geometry(alaska)
alaska_centroid <- st_centroid(st_union(alaska_g))

# Rotation function
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

alaska_trans <- (alaska_g - alaska_centroid) * rot(-1 * pi/180) / 2.3 + alaska_centroid + c(35, -40)

alaska <- alaska %>% st_set_geometry(alaska_trans) %>% st_set_crs(st_crs(us_map))

hawaii <- us_map %>% filter(NAME %in% 'Hawaii')

hawaii_g <- st_geometry(hawaii)
hawaii_centroid <- st_centroid(st_union(hawaii_g))

hawaii_trans <- (hawaii_g - hawaii_centroid) * rot(-35 * pi/180) + hawaii_centroid + c(53, 4)
hawaii <- hawaii %>% st_set_geometry(hawaii_trans) %>% st_set_crs(st_crs(us_map))

us_map_final <- us_map %>%
  filter(!NAME %in% c('Alaska', 'Hawaii')) %>%
  rbind(alaska) %>%
  rbind(hawaii)

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

us_map_regions <- us_map_final %>%
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
  theme_void(base_size = 16) + labs(title = "") +
  theme(legend.position = "bottom")

NASS_regions

# Save plot
ggsave("NASS_regions_map.pdf", NASS_regions, width = 12, height = 8)




