source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")
source("Fig_7_Total_Dollars.R")
source("Stat_6_MMRV_by_Partner_Type.R")

# Stat 7: Avg number of states supported by different types of PCSC partners
  
# Create dataset with PCSC actor type and individual rows for states
PCSC_states_long_with_actors <- merge(PCSC_states_long, PCSC_Partner_Types, by = "Applicant")

# Clean dataset
PCSC_states_long_with_actors <- PCSC_states_long_with_actors %>%
  select(-Notes, - `Agreement found?`, -`Agreement link`)

PCSC_states_long_with_actors$`Actor Type` <- gsub("Firm ", "Firm", PCSC_states_long_with_actors$`Actor Type`)
PCSC_states_long_with_actors <- PCSC_states_long_with_actors %>%
  rename(Actor.Type = `Actor Type`)

# Find total numbers of states supported by each applicant
PCSC_states_count_by_applicant <- PCSC_states_long_with_actors %>%
  group_by(Applicant, Actor.Type) %>%
  summarize(Unique.States.Supported = n_distinct(State))

# Find total number of states supported by each actor type
PCSC_states_aggregated_by_actor <- PCSC_states_count_by_applicant %>%
  group_by(Actor.Type) %>%
  summarize(Total.Unique.States.Supported = sum(Unique.States.Supported))

# Add in actor type frequency for weighting
weighted_PCSC_states_by_actor <- merge(PCSC_states_aggregated_by_actor, 
                                       freq_actors_df, by = "Actor.Type")

# Find averages
weighted_PCSC_states_by_actor <- weighted_PCSC_states_by_actor %>%
  mutate(average_number_of_states_supported = Total.Unique.States.Supported / Freq)

View(weighted_PCSC_states_by_actor)
