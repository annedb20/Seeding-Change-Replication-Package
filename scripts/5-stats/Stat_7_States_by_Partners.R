source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Create a function which runs select code without redownloading data
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Fig_7_Total_Dollars.R", start = 4, end = 184)
source2("Stat_6_MMRV_by_Partner_Type.R", start = 4, end = 83)

# Stat 7: Avg number of states supported by different types of PCSC partners

# Find total numbers of states supported by each applicant
PCSC_states_count_by_applicant <- PCSC_states_long %>%
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
