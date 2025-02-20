source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 6: Share of Dollars Dedicated to HUPs by Program and Contract Status

# Create new dataset representing HUP shares of dollars
hup_share_dollars <- Contract_Download_Table_HUP %>% 
  filter(contract_fiscal_year != "Total") %>%
  group_by(program_short, contract_status, hist._underserved) %>%
  # Generate a variable that is the sum of dollars_obligated for each program, contract status, and HUP status
  mutate(sum_dollars_obligated = sum(dollars_obligated, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(program_short, contract_status) %>% 
  # Find HUP share of dollars obligated to contracts of different statuses and programs
  mutate(all_dollars = sum(dollars_obligated, na.rm = TRUE),
         share_hist_underserved = ifelse(
           hist._underserved == 1, sum_dollars_obligated / all_dollars * 100, NA)) %>% 
  # Filter out irrelevant info
  filter(hist._underserved != "0", 
         !contract_status %in% c("Approved", "Eligible", "Preapproved", "Total"),
         !program_short %in% c("AWEP", "WHIP", "AMA")) %>%
  # Find total contracts for each program status
  mutate(sum_contract_counts = sum(contract_count, na.rm = TRUE)) %>%
  ungroup()

# Identify minimum year
min_year <- min(Contract_Download_Table_HUP$contract_fiscal_year)

# Create the bar chart
hup_share_dollars_plot <- ggplot(hup_share_dollars, aes(x = share_hist_underserved, 
                                                        y = program_short,
                                                        fill = factor(contract_status, 
                                                                      levels = c("Terminated", "Completed", "Cancelled", "Active")))) +
  geom_col(position = position_dodge(width = 0.75), color = "white", width = 0.7) +
  scale_fill_manual(values = c("Active" = "#a6dba0", 
                               "Cancelled" = "#f4a582", # Mutually agreeable
                               "Terminated" = "tomato", # Red: violation of terms
                               "Completed" = "#018571"),
                    guide = guide_legend(reverse = TRUE)) + 
  labs(fill = "Contract Status", 
       y = "", x = "Share of Dollars in a Given Status Associated with Historically Underserved Producers", 
       title = "Share of Dollars Dedicated to Historically Underserved Producers by Program and Contract Status",
       subtitle = paste0("Fiscal Year ", min_year, " - 2023"),
       caption = "Data Source: USDA NRCS Conservational Financial Assistance Data") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  # start the y axis at the 0 line
  scale_y_discrete(expand = c(0, 0)) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "blue") +
  geom_text(aes(label = sum_contract_counts), position = position_dodge(width = .75),
            hjust = -0.1, vjust = 0.5, show.legend = F, size = 5) + # Add labels, adjust size and position, and remove from legend
  theme_classic() + # Increase font size
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(),
        text = element_text(size = 20),
        plot.margin = margin(1, 1, 1, 0, "cm")) # Increase base font size for all text elements

hup_share_dollars_plot
