########################################################################
# Author: Anne Bell Carroll, Elinor Benami (elinor@vt.edu)
# Date: # Sat Feb 22 14:26:43 2025 ------------------------------
# Purpose: Gen Figure on Top 15 PCSC Crops by Frequency
# Notes:
########################################################################

# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")
# source("scripts/ggsave_latex.R")

# Split States_Covered_Under_Agreements into separate columns for each state
PCSC_data_split_by_crops <- 
  PCSC_Projects %>%
  separate_rows(Major_Commodities_Under_Agreement, sep = ",") %>%
  mutate(Major_Commodities_Under_Agreement = trimws(Major_Commodities_Under_Agreement)) %>%
  pivot_wider(names_from = Major_Commodities_Under_Agreement, 
              values_from = Major_Commodities_Under_Agreement, 
              values_fn = length, values_fill = list(Major_Commodities_Under_Agreement = 0))

# Group data by states and sum up counts
crop_counts <- 
  PCSC_data_split_by_crops %>%
  summarize(across(.cols = -Applicant, ~sum(. == 1, na.rm = TRUE))) %>%
  # Remove unnecessary columns
  select(-`Project Summary`, -`Available Practices`, 
         -`Short Agreement Description`, -State, 
         -`MMRV Highlights`, -`Marketing Highlights`, - `Equity Highlights`, 
         -`Federal Funding`, -`Non-Federal Match`, - `Primarily HUP`)

# Sort the result by descending frequency
frequency_crops <- 
  crop_counts %>%
  pivot_longer(cols = everything(), names_to = "Crop", values_to = "Count") %>%
  arrange(desc(Count)) %>%
  slice(1:15)

# Create the bar graph
PCSC_frequency_crops_plot <- 
  frequency_crops %>% 
  ggplot(aes(x = Count, y = reorder(Crop, Count))) + 
  geom_bar(stat = "identity", fill = "#0c2458") + 
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0),
                     #breaks every 5 units+
                     breaks = seq(0, 45, by = 5)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(#title = "Top 15 Partnership for Climate Smart Commodities' Crops by Frequency",
       x = "\nNumber of PCSC projects for which a crop is eligible", y = "")+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 11, face = "bold"), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
        text = element_text(family = "sans") # Apply text size for x-axis title
  ) 

PCSC_frequency_crops_plot

ggsave.latex(PCSC_frequency_crops_plot, 
             filename = file_path("figs/pcsc_top_crops_v2.pdf"), 
             width = 6.5, height = 3.5, units = "in")
