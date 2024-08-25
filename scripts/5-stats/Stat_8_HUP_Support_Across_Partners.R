source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Create a function which runs select code without redownloading data
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Stat_6_MMRV_by_Partner_Type.R", start = 4, end = 82)

# Statistic 8: HUP support across partner types

# Count HUP support for different partner types
PCSC_HUP_by_actor <- PCSC_Projects %>%
  filter(`Primarily HUP` == 1) %>%
  group_by(Actor.Type) %>%
  summarize(count_primarily_HUP = n())

# Add in actor frequencies for weighting
PCSC_HUP_by_actor_weighted <- full_join(PCSC_HUP_by_actor, freq_actors_df, by = "Actor.Type")
PCSC_HUP_by_actor_weighted <- PCSC_HUP_by_actor_weighted %>%
  mutate(weighted_primarily_HUP = count_primarily_HUP / Freq * 100)

View(PCSC_HUP_by_actor_weighted)
