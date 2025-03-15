source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure A.2: Top 5 Practice Types by State ----

# Create dataset with state- and practice-specific information, excluding CSP Annual Payments
state_practices_all <- Practice_Political_Download %>%
  filter(suppressed != "TRUE", geography_level != "National", 
         county_name == "Total", obligation_fy == "Total",
         certification_fy == "Total", practice_code != "Total", 
         practice_name != "Total", program != "AWEP", program!= "WHIP",
         program != "AMA", practice_code != "CROP", practice_code != "RANGE",
         practice_code != "PAST", practice_code != "MINPAY", 
         practice_code != "E300EAP1", practice_code != "E300EAP2",
         practice_code != "NIPF", practice_code != "PCROP", practice_code != "FARM",
         practice_code != "AGLAND") %>%
  select(-geography_level, -county_name, -fips_code) %>%
  # Add region variable (derived from USDA IRA Data Visualization Tool: https://publicdashboards.dl.usda.gov/t/FPAC_PUB/views/FY24IRAReport/FY24IRADashboard?%3Aembed=y&%3AisGuestRedirectFromVizportal=y)
  mutate(region = case_when(state %in% c("Alaska", "Hawaii", "Washington", 
                                         "Oregon", "California", "Idaho", 
                                         "Montana", "Wyoming", "Utah", "Nevada",
                                         "Arizona", "New Mexico", "Colorado") ~ "West",
                            state %in% c("Virginia", "Kentucky", "Tennessee",
                                         "North Carolina", "Arkansas", "Louisiana",
                                         "Florida", "Georgia", "Mississippi", 
                                         "Alabama", "South Carolina") ~ "Southeast", 
                            state %in% c("Maine", "New Hampshire", "Vermont", 
                                         "Massachusetts", "Connecticut", "Rhode Island", 
                                         "New York", "Pennsylvania", "New Jersey",
                                         "Delaware", "Maryland", "West Virginia", 
                                         "Ohio", "Michigan") ~ "Northeast",
                            state %in% c("Texas", "Oklahoma", "Kansas", "Missouri", 
                                         "Illinois", "Indiana", "Wisconsin", 
                                         "Minnesota", "Iowa", "Nebraska",
                                         "South Dakota", "North Dakota") ~ "Central",
                            state %in% c("U.S. Virgin Islands", "Guam", "Puerto Rico", 
                                         "Northern Mariana Islands", "American Samoa") ~ "Is. Terr."))

# Take sum of dollars obligated to each practice in each state
top_practice_codes <- 
  state_practices_all %>%
  group_by(state, practice_code) %>%
  summarize(region = region,
            practice_name = practice_name,
            practice_dollars_obligated = sum(dollars_obligated)) %>%
  ungroup()

# Filter out duplicate rows so ranking is valid
top_practice_codes <- distinct(top_practice_codes)

# Take top 5 practice codes for each state
top_practice_codes <- top_practice_codes %>%
  group_by(state) %>%
  top_n(5, practice_dollars_obligated) %>%
  mutate(ranking = rank(-practice_dollars_obligated)) %>%
  ungroup()

# Establish order for states
top_practice_codes$state <- factor(top_practice_codes$state, levels = 
                                     unique(top_practice_codes$state[order(top_practice_codes$region)]))

# Add categories for each practice
top_practice_codes <- top_practice_codes %>%
  mutate(practice_type = case_when(practice_name %in% c("Clearing and Snagging",
                                                        "Stream Habitat Improvement and Management",
                                                        "Streambank and Shoreline Protection",
                                                        "Pond Sealing or Lining - Concrete")
                                   ~ "Surface Water Improvement",
                                   practice_name %in% c("Animal Mortality Facility", 
                                                        "Composting Facility", "Waste Storage Facility",
                                                        "Waste Transfer") ~ "Waste Disposal",
                                   practice_name %in% c("Early Successional Habitat Development-Mgt", 
                                                        "Brush Management", 
                                                        "Herbaceous Weed Treatment",
                                                        "Conservation Crop Rotation", "Cover Crop",
                                                        "Planting for high carbon sequestration rate", 
                                                        "Pasture and Hay Planting", "Range Planting",
                                                        "Restoration of Rare or Declining Natural Communities",
                                                        "Prescribed Burning", "Vegetative Barrier")
                                   ~ "Vegetation Management to Enhance Soil and Habitat",
                                   practice_name %in% c("Energy Efficient Agricultural Operation", 
                                                        "Energy Efficient Building Envelope") ~ "Energy Efficient Activity",
                                   practice_name %in% c("Existing Activity Payment-Land Use",
                                                        "Existing Activity Payment-Resource Concern") ~ "Existing Activity Payment",
                                   practice_name %in% c("Fence", "Trails and Walkways", 
                                                        "Roofs and Covers", "High Tunnel System",
                                                        "Terrace", "Access Road", 
                                                        "Grade Stabilization Structure") ~ "Functional Infrastructure",
                                   practice_name %in% c("Heavy Use Area Protection", "Mulching") ~ "Ground Cover (excluding crops)",
                                   practice_name %in% c("Incorporating wildlife refuge areas in contingency plans for wildlife.",
                                                        "Prescribed Grazing") ~ "Grazing Plans",
                                   practice_name %in% c("Irrigation Land Leveling", 
                                                        "Irrigation Pipeline",
                                                        "Irrigation Reservoir",
                                                        "Irrigation System, Microirrigation",
                                                        "Sprinkler System") ~ "Irrigation",
                                   practice_name %in% c("Livestock Pipeline", "Watering Facility") ~ "Watering Livestock",
                                   practice_name %in% c("Pumping Plant", "Water Well", 
                                                        "Structure for Water Control", 
                                                        "Underground Outlet") ~ "Water Access",
                                   practice_name %in% c("Reduce risk of pesticides in surface water by utilizing precision pesticide application techniques",
                                                        "Reduce risks of nutrient losses to surface water by utilizing precision ag technologies",
                                                        "Nutrient Management", "Agrichemical Handling Facility",
                                                        "Improving nutrient uptake efficiency and reducing risk of nutrient losses",
                                                        "Pest Management Conservation System") ~ "Nutrient Control",
                                   practice_name %in% c("Tree/Shrub Establishment", 
                                                        "Tree/Shrub Site Preparation",
                                                        "Forest Stand Improvement",
                                                        "Woody Residue Treatment",
                                                        "Windbreak/Shelterbelt Establishment and Renovation") ~ 
                                     "Tree and Forestry Management",
                                   practice_name %in% "Bivalve Aquaculture Gear and Biofouling Control" ~ "Aquaculture",
                                   practice_name %in% "Combustion System Improvement" ~ "Combustion System Improvement",
                                   practice_name %in% c("Residue and Tillage Management, Reduced Till",
                                                        "Residue and Tillage Management, No Till") ~ "Residue and Tillage Mgt",
                                   practice_name %in% "Obstruction Removal" ~ "Obstruction Removal",
                                   practice_name %in% "Grassland Conservation Initiative" ~ "Grassland Conservation Initiative"),
         # Add abbreviations for easier viewing on the graph
         abbreviations = case_when(practice_name %in% "Clearing and Snagging" ~ "Clear & Snag",
                                   practice_name %in% "Grade Stabilization Structure" ~ "Grade Stablization",
                                   practice_name %in% "Stream Habitat Improvement and Management" ~ "Stream Habitat",
                                   practice_name %in% "Streambank and Shoreline Protection" ~ "Bank Protection",
                                   practice_name %in% "Animal Mortality Facility" ~ "Animal Mortality",
                                   practice_name %in% "Composting Facility" ~ "Compost",
                                   practice_name %in% "Waste Storage Facility" ~ "Waste Storage",
                                   practice_name %in% "Waste Transfer" ~ "Waste Transfer",
                                   practice_name %in% "Conservation Crop Rotation" ~ "Crop Rotate",
                                   practice_name %in% "Cover Crop" ~ "Cover Crop",
                                   practice_name %in% "Planting for high carbon sequestration rate" ~ "Planting-C Storage",
                                   practice_name %in% "Pasture and Hay Planting" ~ "Planting-Pasture/Hay",
                                   practice_name %in% "Range Planting" ~ "Planting-Range",
                                   practice_name %in% "Restoration of Rare or Declining Natural Communities" ~ "Habitat Restore",
                                   practice_name %in% "Early Successional Habitat Development-Mgt" ~ "Temp Habitat Mgt",
                                   practice_name %in% "Brush Management" ~ "Brush Mgt",
                                   practice_name %in% "Herbaceous Weed Treatment" ~ "Weed Trtmt",
                                   practice_name %in% "Cropland Annual Payment" ~ "Crop AP",
                                   practice_name %in% "Non-Industrial Private Forest Land Annual Payment" ~ "Forest AP",
                                   practice_name %in% "Pasture Annual Payment" ~ "Pasture AP",
                                   practice_name %in% "Pastured Cropland Annual Payment" ~ "Pasture-Crop AP",
                                   practice_name %in% "Rangeland Annual Payment" ~ "Range AP",
                                   practice_name %in% "Energy Efficient Agricultural Operation" ~ "EE Ag Op",
                                   practice_name %in% "Energy Efficient Building Envelope" ~ "EE Bldg Envelope",
                                   practice_name %in% "Existing Activity Payment-Land Use" ~ "EAP-Land Use",
                                   practice_name %in% "Existing Activity Payment-Resource Concern" ~ "EAP-Resource",
                                   practice_name %in% "Fence" ~ "Fence",
                                   practice_name %in% "Trails and Walkways" ~ "Trls & Wlkways",
                                   practice_name %in% "Roofs and Covers" ~ "Roofs & Covers",
                                   practice_name %in% "High Tunnel System" ~ "High Tunnel",
                                   practice_name %in% "Terrace" ~ "Terrace",
                                   practice_name %in% "Heavy Use Area Protection" ~ "Hvy Use Area Protn",
                                   practice_name %in% "Mulching" ~ "Mulching",
                                   practice_name %in% "Incorporating wildlife refuge areas in contingency plans for wildlife." ~ "Wildlife Refuge",
                                   practice_name %in% "Prescribed Grazing" ~ "Prescr. Grazing",
                                   practice_name %in% "Irrigation Land Leveling" ~ "Irgn Land Level",
                                   practice_name %in% "Irrigation Pipeline" ~ "Irgn Pipeline",
                                   practice_name %in% "Irrigation Reservoir" ~ "Irgn Reservoir",
                                   practice_name %in% "Irrigation System, Microirrigation" ~ "Micro-irgn",
                                   practice_name %in% "Sprinkler System" ~ "Sprinkler",
                                   practice_name %in% "Livestock Pipeline" ~ "Livestock Pipe", 
                                   practice_name %in% "Watering Facility" ~ "Wtr Facility",
                                   practice_name %in% "Pumping Plant" ~ "Pump Plant",
                                   practice_name %in% "Water Well" ~ "Well",
                                   practice_name %in% "Structure for Water Control" ~ "Wtr Ctrl Struct",
                                   practice_name %in% "Underground Outlet" ~ "Undergrd Outlet",
                                   practice_name %in% "Reduce risk of pesticides in surface water by utilizing precision pesticide application techniques" ~ "Precise Pesticide Appln",
                                   practice_name %in% "Reduce risks of nutrient losses to surface water by utilizing precision ag technologies" ~ "Precise Ag Tech",
                                   practice_name %in% "Nutrient Management" ~ "Nutrient Mgt",
                                   practice_name %in% "Tree/Shrub Establishment" ~ "Tree/Shrub Estmt",
                                   practice_name %in% "Tree/Shrub Site Preparation" ~ "Tree/Shrub Prep",
                                   practice_name %in% "Forest Stand Improvement" ~ "Forest Improvemt",
                                   practice_name %in% "Woody Residue Treatment" ~ "Woody Residue Trtmt",
                                   practice_name %in% "Bivalve Aquaculture Gear and Biofouling Control" ~ "Aquaculture Ctrl",
                                   practice_name %in% "Combustion System Improvement" ~ "Combustn Sys Improvemt",
                                   practice_name %in% "Residue and Tillage Management, Reduced Till" ~ "Reduced Till",
                                   practice_name %in% "Residue and Tillage Management, No Till" ~ "No Till",
                                   practice_name %in% "Obstruction Removal" ~ "Obstructn Removal",
                                   practice_name %in% "Grassland Conservation Initiative" ~ "GCI",
                                   practice_name %in% "Windbreak/Shelterbelt Establishment and Renovation" ~ "Tree Windbreak",
                                   practice_name %in% "Pest Management Conservation System" ~ "Pest Mgt",
                                   practice_name %in% "Improving nutrient uptake efficiency and reducing risk of nutrient losses" ~ "Nutrient Uptake",
                                   practice_name %in% "Agrichemical Handling Facility" ~ "Ag.chem. Handling",
                                   practice_name %in% "Access Road" ~ "Access Road",
                                   practice_name %in% "Vegetative Barrier" ~ "Veg. Barrier",
                                   practice_name %in% "Prescribed Burning" ~ "Prescribed Burn",
                                   practice_name %in% "Pond Sealing or Lining - Concrete" ~ "Concrete Lining")) %>%
  # Establish order for regions
  mutate(region = factor(region, levels = c("Northeast", "Southeast", "Central",
                                            "West", "Is. Terr.")))

# Create plot
top_practice_codes_plot <-ggplot(top_practice_codes, aes(x = ranking, y = state, 
                                                         fill = practice_type, 
                                                         group = region)) +
  geom_tile() + 
  facet_grid(region ~ ., scales = "free_y", space = "free_y") + # Separate by region
  geom_text(aes(label = abbreviations), color = "white", size = 3) + # Add abbreviations 
  scale_fill_viridis_d() + # Set color scale
  # Adjust font sizes for legibility
  theme(plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 9), legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 11), strip.text = element_text(size = 11),
        legend.position = "bottom", legend.key.size = unit(3, 'mm'),
        legend.key.width = unit(3, 'mm')) +
  guides(fill = guide_legend(nrow=5)) +
  labs(title = "Top 5 Practice Types by State", 
       x = "Practice Code Rank", y = "", fill = "Practice Type")

top_practice_codes_plot
