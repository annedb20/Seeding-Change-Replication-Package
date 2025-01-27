source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 7: Total Dollars Obligated by Program

# Create dataset for state dollar totals for historical programs
historical_dollar_totals_per_state <- 
  Contract_Political_Download %>% 
  # Filter unnecessary information
  filter(state != "Total",
         geography_level == "State",
         county_name == "Total",
         historically_underserved == "Total",
         suppressed != "TRUE",
         obligation_fy == "Total",
         !program %in% c("AWEP", "AMA", "WHIP")) %>% 
# Due to lack of contract_status column, must filter for max of contract_count in order to filter for "Total" as contract_status
  group_by(state, program, obligation_fy) %>%
  filter(contract_count == max(contract_count)) %>%
  ungroup() %>%   
# Creater broader program values
  mutate(
    program_short = case_when(
      str_detect(program, "RCPP") ~ "RCPP",
      str_detect(program, "CSP") ~ "CSP",
      str_detect(program, "CStwP") ~ "CSP",
      str_detect(program, "EQIP") ~ "EQIP",
      TRUE ~ program)) %>% # Keep the original value if no match
  select(-program) %>%
  rename(program = program_short) %>%
  # Find totals dollars obligated for each state from historical programs
  group_by(state) %>%
  select(state, program, dollars_obligated, contract_count, treated_acres) %>% 
  mutate(sum_dollars_obligated = sum(dollars_obligated)) %>%
  ungroup() %>% 
  arrange(-sum_dollars_obligated, state, program) %>%
  # Add region variable (derived from USDA IRA Data Visualization Tool: https://publicdashboards.dl.usda.gov/t/FPAC_PUB/views/InflationReductionActDataVisualizationTool/IRAEndofYearReport?%3Aembed=y&%3AisGuestRedirectFromVizportal=y)
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
                                         "Northern Mariana Islands", "American Samoa") ~ 
                              "Is. Terr.")) %>%
  select(-contract_count, -treated_acres)

# Separate states into their own rows and clean names for merging with NASS data
PCSC_states_long <- separate_rows(PCSC_Projects, State, sep = ",")
PCSC_states_long <- PCSC_states_long %>% # Some data lost for Tribal and territories since not currently included by NASS
  mutate(State = case_when(State == "AL" ~ "Alabama", State == "AZ" ~ "Arizona", 
                           State == "AR" ~ "Arkansas", State == "AK" ~ "Alaska", 
                           State == "CA" ~ "California", State == "CO" ~ "Colorado",
                           State == "DE" ~ "Delaware", State == "CT" ~ "Connecticut",
                           State == "FL" ~ "Florida", State == "GA" ~ "Georgia",
                           State == "HI" ~ "Hawaii", State == "ID" ~ "Idaho",
                           State == "IL" ~ "Illinois", State == "IN" ~ "Indiana",
                           State == "IA" ~ "Iowa", State == "KS" ~ "Kansas",
                           State == "KY" ~ "Kentucky", State == "LA" ~ "Louisiana",
                           State == "ME" ~ "Maine", State == "MD" ~ "Maryland", 
                           State == "MA" ~ "Massachusetts", State == "MI" ~ "Michigan",
                           State == "MN" ~ "Minnesota", State == "MS" ~ "Mississippi",
                           State == "MO" ~ "Missouri", State == "MT" ~ "Montana",
                           State == "NE" ~ "Nebraska", State == "NH" ~ "New Hampshire",
                           State == "NM" ~ "New Mexico", State == "NV" ~ "Nevada",
                           State == "NJ" ~ "New Jersey", State == "NY" ~ "New York",
                           State == "NC" ~ "North Carolina", State == "ND" ~ "North Dakota",
                           State == "OH" ~ "Ohio", State == "OK" ~ "Oklahoma",
                           State == "OR" ~ "Oregon", State == "PA" ~ "Pennsylvania",
                           State == "RI" ~ "Rhode Island", State == "SC" ~ "South Carolina",
                           State == "SD" ~ "South Dakota", State == "TN" ~ "Tennessee",
                           State == "TX" ~ "Texas", State == "UT" ~ "Utah",
                           State == "VA" ~ "Virginia", State == "VT" ~ "Vermont",
                           State == "WA" ~ "Washington", State == "WV" ~ "West Virginia",
                           State == "WI" ~ "Wisconsin", State == "WY" ~ "Wyoming"))

# Merge datasets to allow for calculation
PCSC_producers_by_state <- merge(PCSC_states_long, Producer_Numbers_by_State, by = "State")

# Remove unnecessary columns
PCSC_producers_numbers <- PCSC_producers_by_state %>%
  select(-`Project Summary`, -`Short Agreement Description`, -`MMRV Highlights`, 
         -`Marketing Highlights`, -`Equity Highlights`, -Program, -Period, 
         -`Week Ending`, -`Geo Level`, -`State ANSI`, -`Ag District`, -`Ag District Code`,
         -County, -`County ANSI`, -`Zip Code`, -Region, -watershed_code, -Watershed,
         -Commodity, -Domain, -`Domain Category`, -`Data Item`, -`CV (%)`)

# Rename number of producers column
names(PCSC_producers_numbers)[names(PCSC_producers_numbers) == "Value"] <- "Number of Producers"

# Remove commas and make number of producers into numerical variable
PCSC_producers_numbers$`Number of Producers` <- gsub(",", "", PCSC_producers_numbers$`Number of Producers`)
PCSC_producers_numbers$`Number of Producers` <- as.numeric(PCSC_producers_numbers$`Number of Producers`)

# Sum dollars per state across projects
PCSC_producers_numbers_sum <- PCSC_producers_numbers %>%
  group_by(Applicant) %>%
  mutate(Project_Producer_Number = sum(`Number of Producers`)) %>% # Create variable of total producers per project
  ungroup() %>%
  group_by(Applicant, State) %>%
  mutate(Percent_State_Project = `Number of Producers`/Project_Producer_Number * 100, # Create variable of percentage of project producers which belong to each state
         Dollars_State_Project = `Federal Funding` * Percent_State_Project / 100, # Create variable which takes the percentage of federal dollars for each state and project
         Nonfed_Dollars_Project = `Non-Federal Match` * Percent_State_Project / 100) %>% # Create variable which takes the percentage of non-federal dollars for each state and project
  ungroup() %>%
  group_by(State) %>%
  summarize(Sum_Dollars_State = sum(Dollars_State_Project),
            Sum_Nonfed_Dollars_State = sum(Nonfed_Dollars_Project)) %>% # Add up project dollars for each state
  ungroup() %>%
  # Add region variable (derived from USDA IRA Data Visualization Tool: https://publicdashboards.dl.usda.gov/t/FPAC_PUB/views/InflationReductionActDataVisualizationTool/IRAEndofYearReport?%3Aembed=y&%3AisGuestRedirectFromVizportal=y)
  mutate(region = case_when(State %in% c("Alaska", "Hawaii", "Washington", 
                                         "Oregon", "California", "Idaho", 
                                         "Montana", "Wyoming", "Utah", "Nevada",
                                         "Arizona", "New Mexico", "Colorado") ~ "West",
                            State %in% c("Virginia", "Kentucky", "Tennessee",
                                         "North Carolina", "Arkansas", "Louisiana",
                                         "Florida", "Georgia", "Mississippi", 
                                         "Alabama", "South Carolina") ~ "Southeast", 
                            State %in% c("Maine", "New Hampshire", "Vermont", 
                                         "Massachusetts", "Connecticut", "Rhode Island", 
                                         "New York", "Pennsylvania", "New Jersey",
                                         "Delaware", "Maryland", "West Virginia", 
                                         "Ohio", "Michigan") ~ "Northeast",
                            State %in% c("Texas", "Oklahoma", "Kansas", "Missouri", 
                                         "Illinois", "Indiana", "Wisconsin", 
                                         "Minnesota", "Iowa", "Nebraska",
                                         "South Dakota", "North Dakota") ~ "Central",
                            State %in% c("U.S. Virgin Islands", "Guam", "Puerto Rico", 
                                         "Northern Mariana Islands", "American Samoa") ~ 
                              "Is. Terr."),
         # Add program variable for merging
         program = "PCSC") %>%
  # Rename variables to match historical totals' dataset
  rename(state = State,
         dollars_obligated = Sum_Dollars_State)

# Combine PCSC and historical datasets
All_Programs_Total_Dollars <- bind_rows(historical_dollar_totals_per_state, 
                                        PCSC_producers_numbers_sum)

# Edit totals to reflect additional PCSC dollars
All_Programs_Total_Dollars <- All_Programs_Total_Dollars %>%
  group_by(state) %>%
  mutate(sum_dollars_obligated = sum(dollars_obligated)) %>%
  ungroup()

# Establish sorting order for levels
sorting_order <- All_Programs_Total_Dollars %>%
  group_by(state) %>%
  summarise(Total = sum(dollars_obligated)) %>%
  arrange(Total) %>%
  pull(state)

# Sort out rows for which PCSC dollars are unknown (due to lack of NASS data)
All_Programs_Total_Dollars <- All_Programs_Total_Dollars %>%
  filter(region != "Is. Terr.")

# Create the bar chart
All_Programs_Total_Dollars_Plot <- All_Programs_Total_Dollars %>%
  mutate(state = factor(state, levels = sorting_order),
         region = factor(region, levels = c("Central", "Southeast", "West", 
                                            "Northeast")),
         program = factor(program, levels = c("PCSC", "RCPP", "CSP", "EQIP"))) %>%
  ggplot(aes(y = state, x = dollars_obligated / 1e6, fill = program)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Dollars in Millions", y = "",
       title = "Total Dollars Obligated by Program", fill = "Program") +
  facet_grid(region ~ ., scales = "free_y", space = "free") + # Separate graph by region
  theme_minimal(base_size = 18) + # Increase font size of all elements
  theme(axis.text.y = element_text(angle = 0, hjust = 1), legend.position = "bottom") +
  scale_x_continuous(labels = scales::label_comma(), breaks = c(0, 500, 1000)) 

All_Programs_Total_Dollars_Plot
