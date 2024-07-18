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

# Print in latex form
NASS_Regions <- NASS_Regions %>%
  kable(caption = "NASS Regions", format = "latex", booktabs = TRUE, 
        escape = FALSE, align = "lrrr")
print(NASS_Regions)

