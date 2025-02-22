########################################################################
# Author: Anne Bell Caroll, Elinor Benami (elinor@vt.edu)
# Date: # Sat Feb 22 14:26:43 2025 ------------------------------
# Purpose:
# Notes:
# Data Download here: https://eric.clst.org/tech/usgeojson/ (Original Download)
########################################################################


# Load Libraries -----
# source("packages.R")
# source("scripts/1-load_data.R")
# source("scripts/2-clean_data.R")

# Load additional, script specific libraries 00000 
library(sf)
library(patchwork)
library(gtable)
library(tigris)
options(tigris_use_cache = TRUE) # turn on caching for faster loading

# Load additional, script-specific datasets ----- 
# state_geometries <- read_sf("/Users/annedb20/csa/gz_2010_us_040_00_5m.json")
state_geometries <- states(cb = FALSE, resolution = "500k") # this features year 2021 geometries

# 1. Filter for programs of interest and state- and practice-specific information -----
# (Annual Payments removed, as they're not practice-specific)
state_practices <- 
  Practice_Political_Download %>%
  filter(suppressed != "TRUE", geography_level != "National", 
         county_name == "Total", obligation_fy == "Total",
         certification_fy == "Total", practice_code != "Total", 
         practice_name != "Total", program != "AWEP", program!= "WHIP",
         program != "AMA", practice_code != "CROP", practice_code != "RANGE",
         practice_code != "PAST", practice_code != "MINPAY",
         practice_code != "E300EAP1",
         practice_code != "E300EAP2",
         practice_code != "NIPF", practice_code != "PCROP") %>%
  select(-geography_level, -county_name, -fips_code)

# Find sum of dollars_obligated for each region and practice -----
top_practice_codes_states <- state_practices %>%
  group_by(state, practice_code) %>%
  summarize(practice_name = practice_name,
            practice_dollars_obligated = sum(dollars_obligated)) %>%
  distinct() %>%
  ungroup() %>%
# Take top 5 practices in each region
  group_by(state) %>%
  top_n(5, practice_dollars_obligated) %>%
  mutate(ranking = rank(-practice_dollars_obligated)) %>%
  ungroup() %>%
  # Authors sorted practices into the following categories based off of practice descriptions 
  mutate(practice_type = case_when(practice_name %in% c("Clearing and Snagging",
                                                        "Grade Stabilization Structure",
                                                        "Stream Habitat Improvement and Management",
                                                        "Streambank and Shoreline Protection",
                                                        "Pond Sealing or Lining - Concrete",
                                                        "Irrigation Land Leveling", 
                                                        "Irrigation Pipeline",
                                                        "Irrigation Reservoir",
                                                        "Irrigation System, Microirrigation",
                                                        "Sprinkler System",
                                                        "Livestock Pipeline", 
                                                        "Watering Facility",
                                                        "Pumping Plant", 
                                                        "Water Well", 
                                                        "Structure for Water Control", 
                                                        "Underground Outlet",
                                                        "Bivalve Aquaculture Gear and Biofouling Control") ~ "Water Mgmt and Aquatic Habitat",
                                   practice_name %in% c("Animal Mortality Facility", 
                                                        "Composting Facility", 
                                                        "Waste Storage Facility",
                                                        "Waste Transfer",
                                                        "Reduce risk of pesticides in surface water by utilizing precision pesticide application techniques",
                                                        "Reduce risks of nutrient losses to surface water by utilizing precision ag technologies",
                                                        "Improving nutrient uptake efficiency and reducing risk of nutrient losses",
                                                        "Nutrient Management",
                                                        "Agrichemical Handling Facility",
                                                        "Pest Management Conservation System") ~ "Waste Mgmt and Chemical Control",
                                   practice_name %in% c("Energy Efficient Agricultural Operation", 
                                                        "Energy Efficient Building Envelope",
                                                        "Fence", 
                                                        "Trails and Walkways", 
                                                        "Roofs and Covers", 
                                                        "High Tunnel System",
                                                        "Access Road",
                                                        "Combustion System Improvement",
                                                        "Obstruction Removal") ~ "Infrastructure and Energy Efficiency",
                                   practice_name %in% c("Early Successional Habitat Development-Mgt", 
                                                        "Brush Management", 
                                                        "Herbaceous Weed Treatment",
                                                        "Conservation Crop Rotation", 
                                                        "Cover Crop",
                                                        "Planting for high carbon sequestration rate", 
                                                        "Pasture and Hay Planting", 
                                                        "Range Planting",
                                                        "Restoration of Rare or Declining Natural Communities",
                                                        "Prescribed Burning",
                                                        "Incorporating wildlife refuge areas in contingency plans for wildlife.",
                                                        "Prescribed Grazing",
                                                        "Grassland Conservation Initiative") ~ "Vegetation and Habitat Mgmt",
                                   practice_name %in% c("Heavy Use Area Protection", 
                                                        "Mulching",
                                                        "Residue and Tillage Management, Reduced Till",
                                                        "Residue and Tillage Management, No Till",
                                                        "Terrace") ~
                                     "Soil and Till Mgmt",
                                   practice_name %in% c("Tree/Shrub Establishment", 
                                                        "Tree/Shrub Site Preparation",
                                                        "Forest Stand Improvement",
                                                        "Woody Residue Treatment") ~ "Forestry and Tree Mgmt"),
         practice_type = factor(practice_type, levels = c("Waste Mgmt and Chemical Control",
                                                             "Water Mgmt and Aquatic Habitat", 
                                                             "Infrastructure and Energy Efficiency",
                                                             "Forestry and Tree Mgmt",
                                                             "Vegetation and Habitat Mgmt",
                                                             "Soil and Till Mgmt")))
         


# Relocate HI/AL to fit in map box more easily --------
alaska <- state_geometries %>% filter(STATEFP %in% "02")
alaska_g <- st_geometry(alaska)
alaska_centroid <- st_centroid(st_union(alaska_g))

hawaii <- state_geometries %>% filter(STATEFP %in% "15")
hawaii_g <- st_geometry(hawaii)
hawaii_centroid <- st_centroid(st_union(hawaii_g))

# Generate Rotation function
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# Transform state geometries

# Decrease Alaska size and set it under the continental US
alaska_trans <- (alaska_g - alaska_centroid) * rot(-1 * pi/180) / 2.3 + alaska_centroid + c(35, -40)
alaska <- alaska %>% st_set_geometry(alaska_trans) %>% st_set_crs(st_crs(state_geometries))

# Set Hawaii under continental US
hawaii_trans <- (hawaii_g - hawaii_centroid) * rot(-35 * pi/180) + hawaii_centroid + c(53, 4)
hawaii <- hawaii %>% st_set_geometry(hawaii_trans) %>% st_set_crs(st_crs(state_geometries))

state_geometries <- state_geometries %>%
  filter(!STATEFP %in% c('02', '15', '72', '')) %>%
  rbind(alaska) %>%
  rbind(hawaii) %>%
  rename(state = NAME)

# Merge sf data with practice data
top_practice_codes_states_geom <- 
  top_practice_codes_states %>% 
  merge(state_geometries, 
        by = "state",
        all = TRUE) %>%
    filter(!(
      state %in% c(
        "Northern Mariana Islands",
        "U.S. Virgin Islands",
        "District of Columbia",
        "Puerto Rico",
        "Guam",
        "American Samoa"
      )
    ))

top_practice_codes_states_geom <- st_as_sf(top_practice_codes_states_geom)
top_practice_codes_states_geom <- st_simplify(top_practice_codes_states_geom, dTolerance = 0.01)

# Add funding data ----- 
# Generate a dataset that only shows the total annual values (dollars obligated, contracts) by program and state
totals_state <- 
  Contract_Political_Download %>% 
  filter(geography_level == "State",
         county_name == "Total",
         historically_underserved == "Total",
         suppressed != "TRUE",
         !(program %in% c("WHIP","AMA", "AWEP")
  )) %>% 
  arrange(state, program, obligation_fy, dollars_obligated) %>% 
  group_by(state, program, obligation_fy) %>%
  # When using this dataset, taking the maximum dollars_obligated essentially filters for "total" contract statuses
  filter(dollars_obligated == max(dollars_obligated)) %>%
  ungroup()  

dollars_per_state <- 
  totals_state %>% 
  filter(dollars_obligated != "Total", 
         obligation_fy != "Total") %>% 
  mutate(dollars_obligated = dollars_obligated / 1e6) %>% # Divide by millions
  group_by(state) %>%
  summarize(sum_dollars = sum(dollars_obligated)) %>%
  ungroup() %>%
  filter(!(state %in% c("Guam", "U.S. Virgin Islands", "Puerto Rico", "Northern Mariana Islands", "American Samoa")))

dollars_per_state_geom <- 
  merge(dollars_per_state, state_geometries, all = TRUE) %>%
  filter(state != "District of Columbia")

dollars_per_state_geom <- st_as_sf(dollars_per_state_geom)
dollars_per_state_geom <- st_simplify(dollars_per_state_geom, dTolerance = 0.01)

# Create funding plot
funding_per_state_plot <- ggplot(dollars_per_state_geom, aes(
  fill = sum_dollars)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-128, -68), ylim = c(18, 50)) + # Adjust xlim to include Alaska
  theme_void() + 
  labs(fill = "Dollars (Millions)",
       subtitle = "Total Funding from EQIP, CSP, and RCPP") +
  theme(legend.position = "right",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1, "cm"),
        plot.subtitle = element_text(size = 16),
        plot.background = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(.25, .25, .25, .25), "cm"))

funding_per_state_plot

# One by one plots of practice rankings ---- 
# Define shorter labels for each practice type
short_labels <- c("Vegetation Mgmt"="Vegetation",
                  "Soil and Till Mgmt"="Soil & Till Mgmt",
                  "Forestry and Tree Mgmt"="Forestry",
                  "Water Mgmt and Aquatic Habitat"="Water Mgmt",
                  "Waste Mgmt and Chemical Control"="Waste Mgmt & Chem Control",
                  "Infrastructure and Energy Efficiency"="Infrastructure")

# Establish color palette
category_colors <- c("Vegetation and Habitat Mgmt"="#638959",
                     "Soil and Till Mgmt"= "#734434",
                     "Forestry and Tree Mgmt"="#134023",
                     "Water Mgmt and Aquatic Habitat"="#045a8d",
                     "Waste Mgmt and Chemical Control"="#8c96c6",
                     "Infrastructure and Energy Efficiency"="slategrey")

# # Set up color palette
# category_colors <- c("Vegetation and Habitat Mgmt"="palegreen3",
#                      "Soil and Till Mgmt"="salmon4",
#                      "Forestry and Tree Mgmt"="palegreen4",
#                      "Water Mgmt and Aquatic Habitat"="cornflowerblue",
#                      "Waste Mgmt and Chemical Control"="mediumpurple3",
#                      "Infrastructure and Energy Efficiency"="slategrey")

#### ---
# Define the function for individual maps
generate_practice_plot <- function(data = top_practice_codes_states_geom, 
                                   ranking, subtitle, legend_position = "none", 
                                   legend_text_size = 16, legend_title_size = 16, legend_key_size = 1) {
  plot <- 
    data %>%
    filter(ranking == ranking) %>%
    ggplot(aes(fill = practice_type)) +
    geom_sf() +
    scale_fill_manual(values = category_colors) +
    coord_sf(xlim = c(-128, -68), ylim = c(18, 50)) + # Adjust xlim to include Alaska
    theme_void() +
    labs(subtitle = subtitle, fill = "Practice Type") +
    theme(legend.position = legend_position,
          plot.subtitle = element_text(size = 16, hjust = 0.5),
          legend.text = element_text(size = legend_text_size),
          legend.title = element_text(size = legend_title_size),
          legend.key.size = unit(legend_key_size, "cm"))
  
  return(plot)
}

# Example usage
practice_one <- generate_practice_plot(top_practice_codes_states_geom, ranking = 1, "1st", "none")
practice_two <- generate_practice_plot(top_practice_codes_states_geom, 2, "2nd", "none")
practice_three <- generate_practice_plot(top_practice_codes_states_geom, 3, "3rd", "right", 13, 15, "1cm")
practice_four <- generate_practice_plot(top_practice_codes_states_geom, 4, "4th", "none")
practice_five <- generate_practice_plot(top_practice_codes_states_geom, 5, "5th", "none")



### Export ---- 
ggsave.latex(top_5_practices_by_state_map, 
             filename = file_path("figs/Top_5_Practices_by_Funding_v5.pdf"), 
             width = 5.85, height = 4, units = "in")
