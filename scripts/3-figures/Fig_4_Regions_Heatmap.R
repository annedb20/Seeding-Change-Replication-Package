# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")

# Figure 4: Top 5 Practice Types by Region

# Filter for programs of interest and state- and practice-specific information
# (Annual Payments removed from consideration since they are not practice-specific)
state_practices_all_12_regions <- 
  Practice_Political_Download %>%
  filter(
    suppressed != "TRUE",
    geography_level != "National",
    county_name == "Total",
    obligation_fy == "Total",
    certification_fy == "Total",
    practice_code != "Total",
    practice_name != "Total",
    program != "AWEP",
    program != "WHIP",
    program != "AMA",
    practice_code != "CROP",
    practice_code != "RANGE",
    practice_code != "PAST",
    practice_code != "MINPAY",
    practice_code != "E300EAP1",
    practice_code != "E300EAP2",
    practice_code != "NIPF",
    practice_code != "PCROP"
  ) %>%
  select(-geography_level, -county_name, -fips_code) %>%  
  # Make new region variable, derived from https://www.nass.usda.gov/Statistics_by_State/RFO/index.php
  mutate(
    region = case_when(
      state %in% c(
        "Pennsylvania",
        "Delaware",
        "Maryland",
        "New Jersey",
        "New York",
        "Maine",
        "Vermont",
        "New Hampshire",
        "Rhode Island",
        "Massachusetts",
        "Connecticut"
      ) ~
        "Northeastern Region",
      state %in% c(
        "Kentucky",
        "North Carolina",
        "Tennessee",
        "Virginia",
        "West Virginia"
      ) ~
        "Eastern Mountain Region",
      state %in% c("Georgia", "Alabama", "Florida", "South Carolina") ~
        "Southern Region",
      state %in% c ("Michigan", "Indiana", "Ohio") ~
        "Great Lakes Region",
      state %in% c("Iowa", "Minnesota", "Wisconsin") ~
        "Upper Midwest Region",
      state %in% c("Missouri", "Illinois") ~
        "Heartland Region",
      state %in% c("Arkansas", "Louisiana", "Mississippi") ~
        "Delta Region",
      state %in% c("Nebraska", "Kansas", "North Dakota", "South Dakota") ~
        "Northern Plains Region",
      state %in% c("Texas", "Oklahoma") ~
        "Southern Plains Region",
      state %in% c(
        "Colorado",
        "Arizona",
        "Montana",
        "New Mexico",
        "Utah",
        "Wyoming"
      ) ~
        "Mountain Region",
      state %in% c("Washington", "Alaska", "Idaho", "Oregon") ~
        "Northwest Region",
      state %in% c("California", "Hawaii", "Nevada") ~
        "Pacific Region",
      state %in% c(
        "Guam",
        "Puerto Rico",
        "American Samoa",
        "Northern Mariana Islands",
        "U.S. Virgin Islands"
      ) ~
        "Island Territories"
    )
  )

# Find sum of dollars_obligated for each region and practice
top_practice_codes_12_regions <- 
  state_practices_all_12_regions %>%
  group_by(region, practice_code) %>%
  summarize(practice_name = practice_name,
            practice_dollars_obligated = sum(dollars_obligated)) %>%
  ungroup() 

# Filter out duplicate rows for ranking
top_practice_codes_12_regions <- distinct(top_practice_codes_12_regions)

# Take top 5 practices in each region
top_practice_codes_12_regions <-
  top_practice_codes_12_regions %>%
  group_by(region) %>%
  top_n(5, practice_dollars_obligated) %>%
  mutate(ranking = rank(-practice_dollars_obligated)) %>%
  ungroup()

# Add practice categories and abbreviations for better graph legibility
top_practice_codes_12_regions <- 
  top_practice_codes_12_regions %>%
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
                                                        "Woody Residue Treatment") ~ "Forestry and Tree Mgmt"),         abbreviations = case_when(practice_name == "Clearing and Snagging" ~ "Clear & Snag",
                                   practice_name == "Grade Stabilization Structure" ~ "Grade Stablization",
                                   practice_name == "Stream Habitat Improvement and Management" ~ "Stream Habitat",
                                   practice_name == "Streambank and Shoreline Protection" ~ "Bank Protection",
                                   practice_name == "Animal Mortality Facility" ~ "Animal Mortality",
                                   practice_name == "Composting Facility" ~ "Compost",
                                   practice_name == "Waste Storage Facility" ~ "Waste Storage",
                                   practice_name == "Waste Transfer" ~ "Waste Transfer",
                                   practice_name == "Conservation Crop Rotation" ~ "Crop Rotate",
                                   practice_name == "Cover Crop" ~ "Cover Crop",
                                   practice_name == "Planting for high carbon sequestration rate" ~ "Planting-C Storage",
                                   practice_name == "Pasture and Hay Planting" ~ "Planting-Pasture/Hay",
                                   practice_name == "Range Planting" ~ "Planting-Range",
                                   practice_name == "Restoration of Rare or Declining Natural Communities" ~ "Habitat Restore",
                                   practice_name == "Early Successional Habitat Development-Mgt" ~ "Temp Habitat Mgt",
                                   practice_name == "Brush Management" ~ "Brush Mgt",
                                   practice_name == "Herbaceous Weed Treatment" ~ "Weed Trtmt",
                                   practice_name == "Cropland Annual Payment" ~ "Crop AP",
                                   practice_name == "Non-Industrial Private Forest Land Annual Payment" ~ "Forest AP",
                                   practice_name == "Pasture Annual Payment" ~ "Pasture AP",
                                   practice_name == "Pastured Cropland Annual Payment" ~ "Pasture-Crop AP",
                                   practice_name == "Rangeland Annual Payment" ~ "Range AP",
                                   practice_name == "Energy Efficient Agricultural Operation" ~ "EE Ag Op",
                                   practice_name == "Energy Efficient Building Envelope" ~ "EE Bldg Envelope",
                                   practice_name == "Existing Activity Payment-Land Use" ~ "EAP-Land Use",
                                   practice_name == "Existing Activity Payment-Resource Concern" ~ "EAP-Resource",
                                   practice_name == "Fence" ~ "Fence",
                                   practice_name == "Trails and Walkways" ~ "Trls & Wlkways",
                                   practice_name == "Roofs and Covers" ~ "Roofs & Covers",
                                   practice_name == "High Tunnel System" ~ "High Tunnel",
                                   practice_name == "Terrace" ~ "Terrace",
                                   practice_name == "Heavy Use Area Protection" ~ "Hvy Use Area Protn",
                                   practice_name == "Mulching" ~ "Mulching",
                                   practice_name == "Incorporating wildlife refuge areas in contingency plans for wildlife." ~ "Wildlife Refuge",
                                   practice_name == "Prescribed Grazing" ~ "Prescr. Grazing",
                                   practice_name == "Irrigation Land Leveling" ~ "Irgn Land Level",
                                   practice_name == "Irrigation Pipeline" ~ "Irgn Pipeline",
                                   practice_name == "Irrigation Reservoir" ~ "Irgn Reservoir",
                                   practice_name == "Irrigation System, Microirrigation" ~ "Micro-irgn",
                                   practice_name == "Sprinkler System" ~ "Sprinkler",
                                   practice_name == "Livestock Pipeline" ~ "Livestock Pipe", 
                                   practice_name == "Watering Facility" ~ "Wtr Facility",
                                   practice_name == "Pumping Plant" ~ "Pump Plant",
                                   practice_name == "Water Well" ~ "Well",
                                   practice_name == "Structure for Water Control" ~ "Wtr Ctrl Struct",
                                   practice_name == "Underground Outlet" ~ "Undergrd Outlet",
                                   practice_name == "Reduce risk of pesticides in surface water by utilizing precision pesticide application techniques" ~ "Precise Pesticide Appln",
                                   practice_name == "Reduce risks of nutrient losses to surface water by utilizing precision ag technologies" ~ "Precise Ag Tech",
                                   practice_name == "Nutrient Management" ~ "Nutrient Mgt",
                                   practice_name == "Tree/Shrub Establishment" ~ "Tree/Shrub Estmt",
                                   practice_name == "Tree/Shrub Site Preparation" ~ "Tree/Shrub Prep",
                                   practice_name == "Forest Stand Improvement" ~ "Forest Improvemt",
                                   practice_name == "Woody Residue Treatment" ~ "Woody Residue Trtmt",
                                   practice_name == "Bivalve Aquaculture Gear and Biofouling Control" ~ "Aquaculture Ctrl",
                                   practice_name == "Combustion System Improvement" ~ "Combustn Sys",
                                   practice_name == "Residue and Tillage Management, Reduced Till" ~ "Reduced Till",
                                   practice_name == "Obstruction Removal" ~ "Obstructn Removal"))

# Establish region as a factor with West to East order (appears East to West on plot)
top_practice_codes_12_regions <- 
  top_practice_codes_12_regions %>%
  mutate(region = factor(region, levels = c("Island Territories", "Pacific Region",
                                            "Northwest Region", "Mountain Region",
                                            "Southern Plains Region",
                                            "Northern Plains Region",
                                            "Delta Region", "Heartland Region",
                                            "Upper Midwest Region", 
                                            "Great Lakes Region", "Southern Region",
                                            "Eastern Mountain Region",
                                            "Northeastern Region")))

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

# Create plot
top_5_practice_codes_by_region_plot <- 
  ggplot(top_practice_codes_12_regions, aes(x = ranking, y = region, fill = practice_type), alpha = 0.95) + 
  geom_tile() + 
  geom_text(aes(label = abbreviations), color = "white", size = 2.25) + #Give abbreviated labels
  scale_fill_manual(values = category_colors,
                    labels = short_labels) +
  labs(x = "Practice Ranking", y = "Region", fill = "Practice Type"
       #title = "Top 5 Practice Types by Region"
       ) +
  scale_y_discrete(labels = function(x) gsub(" Region", "", x), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0.5, 5.5), breaks = 1:5, expand = c(0, 0)) +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 0, vjust = 0.5, size = 7),
        axis.title.y = element_text(vjust = 0.5, size = 8, face = "bold"), 
        legend.title = element_text(size = 8, face="bold", hjust = 0.5),
        legend.title.align = 0.5,    # Center the title relative to legend box
        legend.box = "horizontal",  # Layout legend items horizontally
        legend.key.width = unit(1.5, "lines"),  # Adjust key width, optional
        legend.key.height = unit(1, "lines"),  # Adjust key height, optional
        legend.margin = margin(t = 0, b = 5, l = 0, r = 5),  # Reduce margin around the legend to decrease the gap
        legend.text = element_text(size = 6),   # Larger text size for legend labels
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 8), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
        text = element_text(family = "sans") # Apply text size for x-axis title
  ) +
  guides(fill = guide_legend(title.position = "left", ncol = 3))


top_5_practice_codes_by_region_plot

ggsave.latex(top_5_practice_codes_by_region_plot, 
             filename = file_path("figs/heatmap_practices_by_region_v7.pdf"), 
             width = 5.85, height = 4, units = "in")
