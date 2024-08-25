source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 8: Top 15 PCSC Crops by Frequency

# Split States_Covered_Under_Agreements into separate columns for each state
PCSC_data_split_by_crops <- PCSC_Projects %>%
  separate_rows(Major_Commodities_Under_Agreement, sep = ",") %>%
  mutate(Major_Commodities_Under_Agreement = trimws(Major_Commodities_Under_Agreement)) %>%
  pivot_wider(names_from = Major_Commodities_Under_Agreement, 
              values_from = Major_Commodities_Under_Agreement, 
              values_fn = length, values_fill = list(Major_Commodities_Under_Agreement = 0))

# Group data by states and sum up counts
crop_counts <- PCSC_data_split_by_crops %>%
  summarize(across(.cols = -Applicant, ~sum(. == 1, na.rm = TRUE))) %>%
  # Remove unnecessary columns
  select(-`Project Summary`, -`Available Practices`, 
         -`Short Agreement Description`, -State, 
         -`MMRV Highlights`, -`Marketing Highlights`, - `Equity Highlights`, 
         -`Federal Funding`, -`Non-Federal Match`, - `Primarily HUP`)

# Sort the result by descending frequency
frequency_crops <- crop_counts %>%
  pivot_longer(cols = everything(), names_to = "Crop", values_to = "Count") %>%
  arrange(desc(Count)) %>%
  slice(1:15)

# Create the bar graph
PCSC_frequency_crops_plot <- ggplot(frequency_crops, aes(
  x = Count, y = reorder(Crop, Count))) + geom_bar(stat = "identity", 
                                                   fill = "skyblue") + 
  theme_minimal(base_size = 16) +
  labs(title = "Top 15 PCSC Crops by Frequency",
       x = "Number of PCSC Projects for which Crop is Eligible", y = "")

PCSC_frequency_crops_plot
