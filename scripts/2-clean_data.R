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

## Partnerships for Climate-Smart Commodities
# Clean white space and capitalizations from practices 
Partnerships_for_Climate_Smart_Commodities$`Available Practices` <- 
  str_replace_all(Partnerships_for_Climate_Smart_Commodities$`Available Practices`, "(^|,|,)\\s*", "\\1")
Partnerships_for_Climate_Smart_Commodities$`Available Practices` <-
  str_to_title(Partnerships_for_Climate_Smart_Commodities$`Available Practices`)
# Rename column to match NASS
names(Partnerships_for_Climate_Smart_Commodities)[names(Partnerships_for_Climate_Smart_Commodities) == "States Covered Under Agreement"] <- "State"

# Clean white space and capitalizations from commodities
Partnerships_for_Climate_Smart_Commodities <- Partnerships_for_Climate_Smart_Commodities %>%
  mutate(Major_Commodities_Under_Agreement = gsub(",\\s*", ",", `Major Commodities Under Agreement`),
         Major_Commodities_Under_Agreement = str_to_title(Major_Commodities_Under_Agreement)) %>%
  select(-`Major Commodities Under Agreement`)

## PCSC data with HUP
# Remove unnecessary columns
PCSC_data_with_HUP_info <- PCSC_data_with_HUP_info %>%
  select(-`Project Summary`, -`States Covered Under Agreement`, -`Short Agreement Description`,
         -`Major Commodities Under Agreement`, -`Available Practices`, -`MMRV Highlights`,
         -`Marketing Highlights`, -`Federal Funding`, -`Non-Federal Match`)

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

## Procuder Numbers by State
Producer_Numbers_by_State$State <- str_to_title(Producer_Numbers_by_State$State) # Format capitalization



