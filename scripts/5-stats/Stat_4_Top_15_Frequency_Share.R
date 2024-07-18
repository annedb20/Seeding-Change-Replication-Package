source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")
source("Fig_4_PCSC_Practices.R")

# Statistic 4: Frequency of top 15 PCSC practices

# Establish total counts of practices
total_practice_counts <- sum(frequency_practices$Count)

# Find frequency of top 15 practices
top_15_practices <- frequency_practices %>%
  slice(1:15)

top_15_practices_counts <- sum(top_15_practices$Count)

# Calculate percentage of practice counts which belong in top 15 practices
top_15_frequency_share <- top_15_practices_counts / total_practice_counts * 100

top_15_frequency_share
