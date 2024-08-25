source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

#Create a function which runs specified code while avoiding redundant data downloads
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Tab_3_PCSC_Funding.R", start = 12, end = 168)

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
