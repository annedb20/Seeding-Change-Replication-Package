source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")
source("Tab_3_PCSC_Funding.R")

# Statistic 5: Avg and median percentage increases in funding from PCSC

# Find percentage increase in funding from historical programs
PCSC_Table_with_increases_calculated <- PCSC_Table %>%
  mutate(Percent_increase_in_funding = Estimated_PCSC_Dollars_Millions
         / Sum_Older_Program_Dollars_Millions * 100)

# Calculate avg and median values for percentage increase
avg_increase <- mean(PCSC_Table_with_increases_calculated$Percent_increase_in_funding)
median_increase <- median(PCSC_Table_with_increases_calculated$Percent_increase_in_funding)

avg_increase
median_increase