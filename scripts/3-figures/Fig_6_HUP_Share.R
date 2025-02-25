########################################################################
# Author: Anne Bell Carroll, Elinor Benami (elinor@vt.edu)
# Date: # Sat Feb 22 14:26:43 2025 ------------------------------
# Purpose: Gen Figure on Share of Dollars Dedicated to HUPs by Program and Contract Status
# Notes:
########################################################################

# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")

# Add script specific packages
library(tidylog)
library(scales)

# Create new dataset representing HUP shares of dollars
hup_share_dollars <- 
  Contract_Download_Table_HUP %>% 
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

# Whittle down hup file to only include relevant columns and that they are distinct
hup_share_dollars_distinct <-  hup_share_dollars %>% distinct(program_short, contract_status, share_hist_underserved, sum_contract_counts)
 
# Define a named vector with program abbreviations and their full names
program_names <- c(
  EQIP = "Environmental.\nQuality Incentives\nProgram (EQIP)",
  CSP = "Conservation\nStewardship\nProgram (CSP)",
  RCPP = "Regional\nConservation\nPartnership\nProgram (RCPP)"
)

# Create the bar chart ----- 
hup_share_dollars_plot <- 
  hup_share_dollars_distinct %>% 
  ggplot(aes(
    x = share_hist_underserved,
    y = factor(program_short, levels = rev(unique(hup_share_dollars$program_short))),  # Reverse the order here for y-axis
    fill = factor(contract_status, levels = c( "Terminated", "Cancelled", "Active", "Completed")
    )
  )) +
  geom_col(position = position_dodge(width = 0.75), color = "white", width = 0.7) +
  scale_fill_manual(values = c("Completed" = "#018571",
                               "Active" = "#a6dba0", 
                               "Cancelled" = "#f4a582", # Mutually agreeable
                               "Terminated" = "tomato" # Red: violation of terms
                               ), 
                    guide = guide_legend(reverse = FALSE)) + # Control legend order
  labs(fill = "Contract Status", 
       y = "", 
       x = "\n Share of funds linked to Historically Underserved Producers"
       # title = "Share of Dollars Dedicated to Historically Underserved Producers by Program and Contract Status",
       # subtitle = paste0("Fiscal Year ", min_year, " - 2023"),
       # caption = "Data Source: USDA NRCS Conservational Financial Assistance Data"
       ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 103), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  # start the y axis at the 0 line
  scale_y_discrete(expand = c(0, 0), labels = program_names) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "#5A5A5A", linewidth = 0.75) +
  geom_text(aes(label = scales::comma(sum_contract_counts)), 
            position = position_dodge(width = .75),
            hjust = -0.1, vjust = 0.5,
            show.legend = F, size = 3.5) + # Add labels, adjust size and position, and remove from legend
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, # center text
                                   vjust = 0.5, size = 10.5, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 12, face = "bold"), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11.5), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
        # legend opts
        legend.position = c(0.8, 0.75),  # Inset the Contract Status (x, y)
        # legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11.5, face = "bold"),
        legend.key.size = unit(0.5, "cm"),
        text = element_text(family = "sans"),
        legend.background = element_rect(colour = "gray", size = 0.5)
  ) 

hup_share_dollars_plot


ggsave.latex(hup_share_dollars_plot, filename = file_path("figs/hup_share_dollars_v2.pdf"), height = 3, width = 8.4, units = "in")
