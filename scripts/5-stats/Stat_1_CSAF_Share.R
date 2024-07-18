source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")
source("Tab_5_CSAF_Support.R")

# Statistic 1: CSAF Historical Share

# Find sum of dollars obligated to all practices
total_dollars_for_practices <- sum(practices_filtered$dollars_obligated, na.rm = TRUE)

# Find sum of dollars obligated to CSAF practices
CSAF_dollars_sum <- sum(CSAF$dollars_obligated, na.rm = TRUE)

# Calculate percentage of funds which are CSAF-specific
percent_historically_cs <- CSAF_dollars_sum / total_dollars_for_practices * 100

percent_historically_cs
