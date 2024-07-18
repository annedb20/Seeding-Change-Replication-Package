source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 2: Funding Levels for USDA Conservation Programs by Year

# Find the maximum value of dollars_obligated (in the billions column)
max_billions <- Program_Funding_Levels %>%
  group_by(year) %>%
  summarize(yearly_total = sum(billions, na.rm = TRUE)) %>%
  summarize(max_yearly_total = max(yearly_total, na.rm = TRUE)) %>%
  pull(max_yearly_total)

# Create function to clean chart
clean_chart_clutter_explore <- 
  theme(panel.background = element_blank(), # Remove panel background
        axis.line = element_line(colour = "grey"), # Add axis line
        axis.title.y = element_text(angle = 0, vjust = 0.5), # Rotate y axis so don't have to crank head
        legend.position = "bottom") 

# Create a stacked bar chart showing funding levels in every year for each program. 
plot_funds <- Program_Funding_Levels %>%
  ggplot(aes(year, billions, fill = program_name)) +
  geom_col(position = "stack") +
  # Add tick marks for every year
  scale_x_continuous(breaks = seq(min(Program_Funding_Levels$year),
                                  max(Program_Funding_Levels$year), by = 1),
                     # Start at origin for x and y axis
                     expand = c(0, 0)) +
  # Add tick marks for every numeric value on the y axis
  scale_y_continuous(breaks = seq(min(Program_Funding_Levels$billions, na.rm = TRUE),
                                  max_billions, by = 1)) +
  labs(title = "Funding Levels for Select Major USDA Conservation Programs by Year",
       x = "",
       y = "Funding\n(Billions)",
       fill = "Program Name", 
       caption = "Source: RCA Data Viewer, USDA NRCS") +
  clean_chart_clutter_explore +
  # Rotate x label 90 degrees
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 12, hjust = 1),
        legend.position = "bottom") +
  # Add dashed line for historical obligations
  geom_segment(aes(x = 2005, xend = 2023, y = max_billions * 1.05, yend = max_billions * 1.05), linetype = "dashed") +
  ggplot2::annotate("text", x = (2005 + 2023) / 2, y = max_billions,
                    label = "Historical Obligations", size = 4, hjust = 0.5) +
  # Add dashed line for projected outlays
  geom_segment(aes(x = 2024, xend = 2033, y = max_billions * 1.05, yend = max_billions * 1.05), linetype = "dashed",
               color = "dimgray") +
  ggplot2::annotate("text", x = ((2024 + 2033) / 2), y = max_billions * 1.04,
                    label = "\n Projected Outlays", size = 4, hjust = 0.5, color = "dimgray") +
  # Adjust the legend to have more rows
  scale_fill_discrete(guide = guide_legend(nrow = 3)) 

plot_funds
