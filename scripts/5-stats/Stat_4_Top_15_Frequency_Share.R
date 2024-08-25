source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Create a function which runs code snippets to avoid redundantly downloading data
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Fig_4_PCSC_Practices.R", start = 4, end = 59)

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
