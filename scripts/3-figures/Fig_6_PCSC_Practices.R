########################################################################
# Author: Anne Bell Carroll, Elinor Benami (elinor@vt.edu)
# Date: # Sat Feb 22 14:26:43 2025 ------------------------------
# Purpose: Gen Figure on  Top 15 PCSC Practices by Frequency
# Notes:
########################################################################

# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")
# source("scripts/ggsave_latex.R")

# Calculate total number of projects (labeled "Applicant" in the data)
total_applicants <- n_distinct(PCSC_Projects$Applicant)

# Split Available_Practices into separate columns for each practice
Partnerships_for_CS_Commodities_data_split <- 
  PCSC_Projects %>%
  rename(Available_Practices = 'Available Practices') %>% # Rename for easier manipulation
  separate_rows(Available_Practices, sep = ",") %>% # Create different rows
  pivot_wider(names_from = Available_Practices, values_from = Available_Practices, # Change into columns
              values_fn = length, values_fill = list(Available_Practices = 0))

# Add up instances of eligibility for each practice
practice_counts <-
  Partnerships_for_CS_Commodities_data_split %>%
  summarize(across(.cols = -Applicant, ~sum(. == 1, na.rm = TRUE))) %>%
  # Remove non-practice columns
  select(-`Project Summary`, 
         -`Short Agreement Description`, 
         -`MMRV Highlights`, -`Marketing Highlights`, - `Equity Highlights`, 
         -`Federal Funding`, -`Non-Federal Match`, 
         -Major_Commodities_Under_Agreement, -State) %>%
  # Manually combine duplicates with different wording or which resist cleaning
  mutate(`340 Cover Crop` = `340 Cover Crop` + `340 Cover Crop `,
         `390 Riparian Herbaceous Cover` = `390 Riparian Herbaceous Cover` + 
           `390 Riparian Herbaceous Cover `,
         `E590a Improving Nutrient Uptake Efficiency And Reducing Risk Of Nutrient Losses` = 
           `E590a Improving Nutrient Uptake Efficiency And Reducing Risk Of Nutrient Losses` + 
           `E590a Improving Fertilizer Uptake Efficiency And Reducing Risk Of Fertilizer Losses`,
         `590 Nutrient Management` = `590 Nutrient Management` + `590 Nutrient Management `,
         `Cema 223 Forest Management Assessment` = `Cema 223 Forest Management Assessment` 
         + `223 Forest Management Assessment`, 
         `Cema 218 Carbon Sequestration And Greenhouse Gas Mitigation Assessment` = 
           `Cema 218 Carbon Sequestration And Greenhouse Gas Mitigation Assessment` 
         + `218 Carbon Sequestration And Greenhouse Gas Mitigation Assessment`) %>%
  select(-`340 Cover Crop `, 
         -`390 Riparian Herbaceous Cover `,
         -`E590a Improving Fertilizer Uptake Efficiency And Reducing Risk Of Fertilizer Losses`,
         -`590 Nutrient Management `,
         -`223 Forest Management Assessment`,
         -`218 Carbon Sequestration And Greenhouse Gas Mitigation Assessment`)

# Remove practice codes 
colnames(practice_counts) <- gsub("\\d+", "", colnames(practice_counts))

# Sort in descending order of frequency
frequency_practices <- 
  practice_counts %>%
  pivot_longer(cols = everything(), names_to = "Practice", values_to = "Count") %>%
  arrange(desc(Count)) %>% 
  # remove the words "Residue and Tillage Management -" from the Practice List (but keep anything that comes after it )
  mutate(Practice = str_remove(Practice, "Residue And Tillage Management -")) %>% 
  # strip out leading white spaces before all practices
  mutate(Practice = str_trim(Practice)) 

# Graph the results with a stacked bar chart
PCSC_frequency_practices_plot <- 
  frequency_practices %>%
  filter(Practice != "Primarily HUP") %>% 
  slice(1:15) %>% # Take top 15
  ggplot(aes(x = Count, y = reorder(Practice, Count))) + 
  geom_bar(stat = "identity", fill = "#0c2458") + 
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0),
                     #breaks every 5 units+
                     breaks = seq(0, 95, by = 15)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(#title = "Top 15 PCSC Practices by Frequency",
       x = "\nNumber of PCSC projects for which a practice is eligible", y = "") +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 11, face = "bold"), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
        text = element_text(family = "sans") # Apply text size for x-axis title
  ) 

PCSC_frequency_practices_plot

ggsave.latex(PCSC_frequency_practices_plot, 
             filename = file_path("figs/PCSC_practices_v2.pdf"), 
             width = 6.5, height = 3.5, units = "in")
