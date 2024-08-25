source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 4: Top 15 PCSC Practices

# Calculate total number of projects (labeled "Applicant" in the data)
total_applicants <- n_distinct(PCSC_Projects$Applicant)

# Split Available_Practices into separate columns for each practice
Partnerships_for_CS_Commodities_data_split <- PCSC_Projects %>%
  rename(Available_Practices = 'Available Practices') %>% # Rename for easier manipulation
  separate_rows(Available_Practices, sep = ",") %>% # Create different rows
  pivot_wider(names_from = Available_Practices, values_from = Available_Practices, # Change into columns
              values_fn = length, values_fill = list(Available_Practices = 0))

# Add up instances of eligibility for each practice
practice_counts <- Partnerships_for_CS_Commodities_data_split %>%
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

# Sort in descending order of frequency
frequency_practices <- practice_counts %>%
  pivot_longer(cols = everything(), names_to = "Practice", values_to = "Count") %>%
  arrange(desc(Count))

# Graph the results with a stacked bar chart
PCSC_frequency_practices_plot <- frequency_practices %>%
  slice(1:15) %>% # Take top 15
  ggplot(aes(x = Count, y = reorder(Practice, Count))) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  theme_minimal(base_size = 16) +
  labs(title = "Top 15 PCSC Practices by Frequency",
       x = "Number of PCSC Projects for which a Practice is Eligible", y = "")

PCSC_frequency_practices_plot
