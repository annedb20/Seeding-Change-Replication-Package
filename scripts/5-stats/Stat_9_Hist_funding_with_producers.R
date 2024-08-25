source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Create a function which will run select code without redownloading data 
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Tab_3_PCSC_Funding.R", start = 4, end = 167)

# Statistic 9: Historical funding by state, viewed with producer numbers

# Filter data for relevant info
Hist_funding_by_state_with_producers <- PCSC_Table %>%
  select(State, Number_of_Producers, Sum_Older_Program_Dollars_Millions)

# Establish the sum of historical funds from this dataset
sum_historical_funds <- sum(
  Hist_funding_by_state_with_producers$Sum_Older_Program_Dollars_Millions)

# Calculate percentage of total historical funds which each state receives
Hist_funding_by_state_with_producers <- Hist_funding_by_state_with_producers %>%
  mutate(state_share_of_hist_funds = 
           Sum_Older_Program_Dollars_Millions / sum_historical_funds * 100) %>%
  arrange(desc(Number_of_Producers))

View(Hist_funding_by_state_with_producers) 
