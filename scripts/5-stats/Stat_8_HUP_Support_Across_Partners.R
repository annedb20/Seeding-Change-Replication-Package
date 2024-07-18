source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")
source("Stat_6_MMRV_by_Partner_Type.R")

# Statistic 8: HUP support across partner types

# Merge actor data with HUP data for PCSC
PCSC_HUP_and_actors <- left_join(PCSC_data_with_HUP_info, PCSC_Partner_Types, by = "Applicant")
PCSC_HUP_by_actor <- PCSC_HUP_and_actors %>%
  filter(`Primarily HUP` == 1) %>%
  group_by(`Actor Type`) %>%
  summarize(count_primarily_HUP = n())

# Rename column for merging
freq_actors_df <- freq_actors_df %>%
  rename(`Actor Type` = `Actor.Type`)

# Add in actor frequencies for weighting
PCSC_HUP_by_actor_weighted <- inner_join(PCSC_HUP_by_actor, freq_actors_df, by = "Actor Type")
PCSC_HUP_by_actor_weighted <- PCSC_HUP_by_actor_weighted %>%
  mutate(weighted_primarily_HUP = count_primarily_HUP / Freq * 100)

View(PCSC_HUP_by_actor_weighted)
