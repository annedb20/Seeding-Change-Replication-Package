source("packages.R")
source("1-load_data.R")

### Clean data for manipulation

## Program Funding and Outlays
Program_Funding_and_Outlays <- Program_Funding_and_Outlays %>%
  mutate(Funding_Billions = Funding_Millions/1000, # Adjust numbers to billions
         Outlays_Billions = Outlays_Millions/1000) %>%
  select(- Funding_Millions, - Outlays_Millions) %>% # Remove unused columns
  mutate(Program = factor(Program, levels = c("RCPP IRA", # Set order for programs
                                              "CSP IRA",
                                              "EQIP IRA",
                                              "RCPP Farm Bill",
                                              "CSP Farm Bill",
                                              "EQIP Farm Bill")))

## Program Funding Levels
Program_Funding_Levels <- Program_Funding_Levels %>%
  clean_names() %>% 
  pivot_longer(cols = -x1, names_to = "year", values_to = "billions") %>% 
  rename(program_name = x1) %>% 
  mutate(year = as.numeric(str_remove(year, "x")),
         program_name = if_else(str_detect(program_name, "Easment"),
                                "Agricultural Conservation Easement Program (ACEP)",
                                program_name),
         # Reorder the factor levels in the desired order
         program_name = fct_relevel(program_name, "Partnerships for Climate-Smart Commodities (PCSC)", after = 0)) %>% 
  filter(!str_detect(program_name, "ACEP|Rental")) 

## PCSC Projects
# Clean white space and capitalizations from practices 
PCSC_Projects$`Available Practices` <- 
  str_replace_all(PCSC_Projects$`Available Practices`, "(^|,|,)\\s*", "\\1")
PCSC_Projects$`Available Practices` <-
  str_to_title(PCSC_Projects$`Available Practices`)
# Rename column to match NASS
names(PCSC_Projects)[names(PCSC_Projects) == "States Covered Under Agreement"] <- "State"

# Clean white space and capitalizations from commodities
PCSC_Projects <- PCSC_Projects %>%
  mutate(Major_Commodities_Under_Agreement = gsub(",\\s*", ",", `Major Commodities Under Agreement`),
         Major_Commodities_Under_Agreement = str_to_title(Major_Commodities_Under_Agreement)) %>%
  select(-`Major Commodities Under Agreement`)

# Clean and rename Actor Type variable
PCSC_Projects$`Actor Type` <- gsub("Firm ", "Firm", PCSC_Projects$`Actor Type`)
PCSC_Projects <- PCSC_Projects %>%
  rename(Actor.Type = `Actor Type`)

## Contract Download Table HUP
# Change column names to lower case
colnames(Contract_Download_Table_HUP) <- tolower(colnames(Contract_Download_Table_HUP))

# Add underscores in place of white space
colnames(Contract_Download_Table_HUP) <- gsub("\\s+", "_", colnames(Contract_Download_Table_HUP), perl = TRUE)

# Filter for relevant programs
Contract_Download_Table_HUP <- Contract_Download_Table_HUP %>% 
  select(-`index()`) %>% 
  filter(hist._underserved != "Total", 
         contract_status != "Total") %>% 
  # Add a new column that collapses IRA and Farmbill programs into one, 
  # As well as CSP-GCI into CSP and RCPP-CSP and RCPP-EQIP
  mutate(
    program_short = case_when(
      str_detect(program, "RCPP") ~ "RCPP",
      str_detect(program, "CSP") ~ "CSP",
      str_detect(program, "CStwP") ~ "CSP",
      str_detect(program, "EQIP") ~ "EQIP",
      TRUE ~ program)) # Keep the original value if no match

# Change dollars_obligated variable to a numeric one
Contract_Download_Table_HUP$dollars_obligated = gsub( #Remove dollar signs
  "\\$", "", Contract_Download_Table_HUP$dollars_obligated)
Contract_Download_Table_HUP$dollars_obligated = gsub( #Remove commas
  "\\,", "", Contract_Download_Table_HUP$dollars_obligated)
Contract_Download_Table_HUP$dollars_obligated <- as.numeric(
  Contract_Download_Table_HUP$dollars_obligated)

## Producer Numbers by State
Producer_Numbers_by_State$State <- str_to_title(Producer_Numbers_by_State$State) # Format capitalization

## Census Datasets: clean and join practice datasets
Census_Cover_Crop <- Census_Cover_Crop %>%
  clean_names()

Census_Tillage <- Census_Tillage %>%
  clean_names()

Census_No._Crop_Operations <- Census_No._Crop_Operations %>%
  clean_names()

Census_Cropland_Acres <- Census_Cropland_Acres %>%
  clean_names()

# Join practice datasets
Census_Practices <- full_join(Census_Cover_Crop, Census_Tillage)
rm(Census_Cover_Crop)
rm(Census_Tillage)

# Make practice values numeric
Census_Practices$value <- gsub(",", "", Census_Practices$value)
Census_Practices$value <- as.numeric(Census_Practices$value)

# Manually input County fips codes for Alaskan counties
Census_Practices <- Census_Practices %>%
  mutate(county_ansi = case_when(state == "ALASKA" & county == "KENAI PENINSULA" ~ "122", 
                                 state == "ALASKA" & county == "ANCHORAGE" ~ "020",
                                 state == "ALASKA" & county == "JUNEAU" ~ "110",
                                 state == "ALASKA" & county == "ALEUTIAN ISLANDS" ~ "010",
                                 state == "ALASKA" & county == "FAIRBANKS NORTH STAR" ~ "090",
                                 TRUE ~ county_ansi))

# Extract acreage
Census_Cropland_Acres <- Census_Cropland_Acres %>%
  select(year, state, county, value) %>%
  rename(crop_acre_number = value)

# Make acres numeric
Census_Cropland_Acres$crop_acre_number <- gsub(",", "", Census_Cropland_Acres$crop_acre_number)
Census_Cropland_Acres$crop_acre_number <- as.numeric(Census_Cropland_Acres$crop_acre_number)
# NAs introduced where acreage was too small to disclose for privacy reasons

# Extract number of operations
Census_No._Crop_Operations <- Census_No._Crop_Operations %>%
  select(year, state, county, value) %>%
  rename(no._crop_operations = value)

# Make no. operations numeric
Census_No._Crop_Operations$no._crop_operations <- gsub(",", "", Census_No._Crop_Operations$no._crop_operations)
Census_No._Crop_Operations$no._crop_operations <- as.numeric(Census_No._Crop_Operations$no._crop_operations)

# Include cropland acreage and operations data with practice data
Census_Practices <- Census_Practices %>%
  left_join(Census_Cropland_Acres, by = c("state", "county", "year")) %>%
  left_join(Census_No._Crop_Operations, by = c("state", "county", "year")) %>%
  # Clean census practice data
  # Remove unnecessary columns
  select(-program, -period, -week_ending, -ag_district, 
         -ag_district_code, -zip_code, -region, -watershed_code, 
         -watershed, -commodity, -domain, -domain_category, -cv_percent) %>%
  # Manage capitalization
  mutate(state = str_to_title(state),
         county = str_to_title(county),
         # Manage practice names for easier reading
         data_item = case_when(data_item == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES" ~ "reduced_tillage_acres",
                               data_item == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - AREA, MEASURED IN ACRES / OPERATION" ~ "reduced_tillage_acres/operation",
                               data_item == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - NUMBER OF OPERATIONS" ~ "reduced_tillage_no._operations",
                               data_item == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES" ~ "no_till_acres",
                               data_item == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - AREA, MEASURED IN ACRES / OPERATION" ~ "no_till_acres/operation",
                               data_item == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - NUMBER OF OPERATIONS" ~ "no_till_no._operations",
                               data_item == "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES" ~ "cover_crop_acres",
                               data_item == "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - AREA, MEASURED IN ACRES / OPERATION" ~ "cover_crop_acres/operation",
                               data_item == "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - NUMBER OF OPERATIONS" ~ "cover_crop_no._operations")) %>%
  rename(county_name = county,
         practice_and_unit = data_item) %>%
  # Pivot wider to make each practice their own column
  pivot_wider(names_from = practice_and_unit, values_from = value) %>%
  # Make practice acres/total cropland acres variables,repeat for number of operations
  mutate(pct_cropland_reduced_tillage = (reduced_tillage_acres / crop_acre_number * 100),
         pct_cropland_no_till = (no_till_acres / crop_acre_number * 100),
         pct_cropland_cover_crop = (cover_crop_acres / crop_acre_number * 100),
         pct_crop_operations_reduced_tillage = (reduced_tillage_no._operations / no._crop_operations * 100),
         pct_crop_operations_no_till = (no_till_no._operations / no._crop_operations * 100),
         pct_crop_operations_cover_crop = (cover_crop_no._operations / no._crop_operations * 100),
         # Make ansi columns numeric
         state_ansi = as.numeric(state_ansi),
         county_ansi = as.numeric(county_ansi))

rm(Census_No._Crop_Operations)
rm(Census_Cropland_Acres)


