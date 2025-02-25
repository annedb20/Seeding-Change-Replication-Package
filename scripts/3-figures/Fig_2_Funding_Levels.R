########################################################################
# Author: Anne Bell Carroll, Elinor Benami (elinor@vt.edu)
# Date: # Sat Feb 22 14:26:43 2025 ------------------------------
# Purpose:  Funding Levels for USDA Conservation Programs by Year
# Notes:
########################################################################

# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")

library(tidylog)


# Remove PCSC (not obligated in the same way)
Program_Funding_Levels <- 
  Program_Funding_Levels %>% 
  filter(program_name != "Partnerships for Climate-Smart Commodities (PCSC)")

# Find the maximum value of dollars_obligated (in the billions column)
max_billions <- 
  Program_Funding_Levels %>%
  group_by(year) %>%
  summarize(yearly_total = sum(billions, na.rm = TRUE)) %>%
  summarize(max_yearly_total = max(yearly_total, na.rm = TRUE)) %>%
  pull(max_yearly_total)


# Create a stacked bar chart showing funding levels in every year for each program. 
plot_funds <- 
  Program_Funding_Levels %>%
  ggplot(aes(year, billions, fill = program_name)) +
  geom_col(position = "stack") +
  # Add tick marks for every year
  scale_x_continuous(breaks = seq(min(Program_Funding_Levels$year),
                                  max(Program_Funding_Levels$year), by = 1),
                     # Start at origin for x and y axis
                     expand = c(0, 0)) +
  # Add tick marks for every numeric value on the y axis
  scale_y_continuous(breaks = seq(min(Program_Funding_Levels$billions, na.rm = TRUE),
                                  max_billions+1, by = 1)) +
  labs(#title = "Funding Levels for Select Major USDA Conservation Programs by Year",
       x = "",
       y = "Funding\n(Billions)",
       fill = "Program"#, 
       #caption = "Source: RCA Data Viewer, USDA NRCS"
       ) +
  # Add dashed line for historical obligations
  geom_segment(aes(x = 2005, xend = 2023, y = max_billions * 1.1, yend = max_billions * 1.1), linetype = "dashed") +
  annotate("text", x = (2005 + 2023) / 2, y = max_billions + 1,
                    label = "Historical Obligations", size = 4, hjust = 0.5) +
  # Add dashed line for projected outlays
  geom_segment(aes(x = 2024, xend = 2033, y = max_billions * 1.1, yend = max_billions * 1.1), linetype = "dashed",
               color = "gray15") +
  annotate("text", x = ((2024 + 2033) / 2), y = max_billions + 1.25,
                    label = "\n Projected Outlays", size = 4, hjust = 0.5, color = "gray15") +
  # Adjust the legend to have more rows
  scale_fill_discrete(guide = guide_legend(nrow = 2))  +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 12), 
        legend.title = element_text(size = 12, face="bold", hjust = 0.5),
        legend.title.align = 0.5,    # Center the title relative to legend box
        legend.box = "horizontal",  # Layout legend items horizontally
        legend.key.width = unit(1.5, "lines"),  # Adjust key width, optional
        legend.key.height = unit(1, "lines"),  # Adjust key height, optional
        legend.margin = margin(t = -15, b = 5, l = 0, r = 0),  # Reduce margin around the legend to decrease the gap
        legend.text = element_text(size = 10),   # Larger text size for legend labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11), 
        # axis.title.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
        text = element_text(family = "sans"), # Apply text size for x-axis title
        panel.background = element_blank(), # Remove panel background
        axis.line = element_line(colour = "grey") # Add axis line
  ) 

plot_funds

ggsave.latex(All_Programs_Total_Dollars_Plot, 
             filename = file_path("figs/funding_levels_v2.pdf"), 
             height = 7.2, width = 7.5, units = "in")

