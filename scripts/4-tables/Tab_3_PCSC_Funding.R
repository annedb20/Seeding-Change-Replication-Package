source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")
 
# Create function to source other files while bypassing the data download 
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Fig_4_PCSC_Practices.R", start = 4, end = 62)
source2("Fig_7_Total_Dollars.R", start = 4, end = 183)

# Table 3: Estimated PCSC Funding

# Goal: Create new table with 7 columns: state, number of producers, number of projects, 
# estimated PCSC fed funds, sum of other programs' funds, estimated nonfed funds,
# and percentage of funds which are PCSC federal

# Pull out useful columns
PCSC_Table <- PCSC_producers_numbers %>%
  group_by(Applicant) %>%
  mutate(Project_Producer_Number = sum(`Number of Producers`)) %>% # Create variable of how many total producers a project might serve
  ungroup() %>%
  group_by(Applicant, State) %>%
  mutate(Percent_State_Project = `Number of Producers`/Project_Producer_Number * 100, # Create variable of percentage of project producers which belong to each state
         Dollars_State_Project = `Federal Funding` * Percent_State_Project / 100, # Create variable which takes the percentage of federal dollars for each state and project
         Nonfed_Dollars_Project = `Non-Federal Match` * Percent_State_Project / 100) %>% # Create variable which takes the percentage of non-federal dollars for each state and project
  ungroup() %>%
  group_by(State) %>%
  mutate(Sum_Dollars_State = sum(Dollars_State_Project),
         Sum_Nonfed_Dollars_State = sum(Nonfed_Dollars_Project)) %>% # Add up project dollars for each state
  ungroup() %>%
  select(-Applicant, -Major_Commodities_Under_Agreement, -Year,
         -Project_Producer_Number, -Percent_State_Project, -Dollars_State_Project,
         -Nonfed_Dollars_Project) %>%
  distinct() %>% # Remove duplicates created by removing other columns
  rename(Estimated_PCSC_Dollars = Sum_Dollars_State, # Create better names for table
         Number_of_Producers = 'Number of Producers',
         Estimated_PCSC_Nonfed_Match = Sum_Nonfed_Dollars_State)

# Create state frequency dataset by first cleaning the variable
PCSC_Projects$`State` <- 
  str_replace_all(PCSC_Projects$`State`, "(^|,|,)\\s*", "\\1")

# Split States_Covered_Under_Agreements into separate columns for each state
Partnerships_for_CS_Commodities_data_split <- PCSC_Projects %>%
  separate_rows(State, sep = ",") %>%
  mutate(State = trimws(State)) %>%
  pivot_wider(names_from = State,
              values_from = State,
              values_fn = length, 
              values_fill = list(State = 0))

# Group data by states and sum up counts
state_counts <- Partnerships_for_CS_Commodities_data_split %>%
  summarize(across(.cols = -Applicant, ~sum(. == 1, na.rm = TRUE))) %>%
  # Remove unnecessary columns
  select(-`Project Summary`, -`Available Practices`, 
         -`Short Agreement Description`, -Major_Commodities_Under_Agreement, 
         -`MMRV Highlights`, -`Marketing Highlights`, - `Equity Highlights`, 
         -`Federal Funding`, -`Non-Federal Match`, - `Primarily HUP`,
         - `Link to Project Website`, - `Actor Type`, - `Agreement found?`, 
         - `Agreement link`)

# Pivot longer and sort by descending percentage
percentage_states <- state_counts %>%
  pivot_longer(cols = everything(), names_to = "State", values_to = "Count") %>%
  mutate(Percentage = Count/total_applicants * 100) %>%
  arrange(desc(Percentage))

# Adjust this state frequency dataset for integration
state_project_numbers <- percentage_states %>%
  # Remove "states" for which we do not have NASS data
  filter(State != "Tribal", State != "PR", State != "GU", 
         State != "Navajo Nation", State != "DC", State != "CNMI") %>%
  # Full state names
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

# Merge with working table 
PCSC_Table <- merge(PCSC_Table, state_project_numbers, by = "State")

# Clean table
PCSC_Table <- PCSC_Table %>%
  select(-Percentage) %>%
  rename(Frequency_of_Project_Eligibility = Count)

# Extract sums from other programs for each state
totals_state_three_programs <- 
  Contract_Political_Download %>% 
  filter(#state != "Total",
    geography_level == "State",
    county_name == "Total",
    historically_underserved == "Total",
    suppressed != "TRUE",
    program != "AWEP",
    program != "WHIP",
    program != "AMA",
    obligation_fy != "Total",
  ) %>% 
  arrange(state, program, obligation_fy, dollars_obligated) %>% 
  group_by(state, program, obligation_fy) %>%
  filter(dollars_obligated == max(dollars_obligated)) %>%
  ungroup() %>%
  group_by(state) %>%
  summarize(sum_dollars_obligated = sum(dollars_obligated)) %>%
  rename(State = state)

# Merge with working table
PCSC_Table <- merge(PCSC_Table, totals_state_three_programs, by = "State")

# Clean and calculate percentage of total funds which are PCSC federal funds for each state
PCSC_Table <- PCSC_Table %>%
  rename(Sum_Older_Program_Dollars = sum_dollars_obligated) %>%
  group_by(State) %>%
  mutate(Percentage_Funds_PCSC = Estimated_PCSC_Dollars / 
           (Sum_Older_Program_Dollars + Estimated_PCSC_Dollars) * 100)

# Reorder columns
PCSC_Table <- PCSC_Table[, c("State", "Number_of_Producers", 
                             "Frequency_of_Project_Eligibility",
                             "Estimated_PCSC_Dollars", 
                             "Estimated_PCSC_Nonfed_Match",
                             "Sum_Older_Program_Dollars",
                             "Percentage_Funds_PCSC")]

# Adjust funds to millions for easier viewing
PCSC_Table <- PCSC_Table %>%
  mutate(Estimated_PCSC_Dollars = Estimated_PCSC_Dollars / 1e6,
         Sum_Older_Program_Dollars = Sum_Older_Program_Dollars / 1e6,
         Estimated_PCSC_Nonfed_Match = Estimated_PCSC_Nonfed_Match / 1e6) %>%
  rename(Sum_Older_Program_Dollars_Millions = Sum_Older_Program_Dollars,
         Estimated_PCSC_Dollars_Millions = Estimated_PCSC_Dollars,
         Estimated_PCSC_Nonfed_Match_Millions = Estimated_PCSC_Nonfed_Match) %>%
  # Sort by descending percentage PCSC 
  arrange(desc(Percentage_Funds_PCSC)) %>%
  distinct()

PCSC_Table_latex <- PCSC_Table %>%
  kable(caption = "State breakdown of funding across relevant programs, sorted by highest percentage of total (federal) funding coming from PCSC to least",
        format = "latex", booktabs = TRUE, escape = FALSE, digits = 2,  align = "lrrr")

print(PCSC_Table_latex)

# Tribal frequency of project eligibility added manually, extracted from percentage_states
