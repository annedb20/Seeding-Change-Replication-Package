source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Stat 3: Top 10 practices' share of funds from historical programs, 2014-2023 ----

# Establish total dollar amount from Practice dataset
totals_practices <- Practice_Political_Download %>%
  # Filter for totals in relevant programs
  filter(geography_level == "National", practice_code == "Total", 
         obligation_fy == "Total", certification_fy == "Total",
         !program %in% c("WHIP", "AMA", "AWEP"))

# Sum total dollars obligated
total_dollars_for_practices <- sum(totals_practices$dollars_obligated)

# Filter for top 10 practices
total_dollars_top_10 <- Practice_Political_Download %>%
  filter(geography_level == "National", practice_code != "Total",
         obligation_fy == "Total", certification_fy == "Total",
         # Check only relevant programs
         !program %in% c("WHIP", "AMA", "AWEP"),
         # Remove Annual Payments and Minimum Payment Adjustments (not practice-specific)
         !practice_code %in% c("CROP", "AGLAND", "PCROP", "NIPF",
                               "PAST", "RANGE", "FARM", "MINPAY",
                               "E300EAP1", "E300EAP2")) %>%
  # Sum dollars obligated to practices across programs
  group_by(practice_code) %>%
  summarize(sum_dollars_to_practices = sum(dollars_obligated, na.rm = TRUE)) %>%
  ungroup() %>%
  # Take the top 10
  arrange(desc(sum_dollars_to_practices)) %>%
  slice(1:10)

# Sum dollars obligated to top 10 practices
top_10_practices_funds <- sum(total_dollars_top_10$sum_dollars_to_practices)

# Compare to total funds
top_10_share <- top_10_practices_funds / total_dollars_for_practices * 100

top_10_share