# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")

# Figure 1: IRA vs Farmbill Outlays

# Create stacked lines plot
program_funding_plot <- ggplot(data = Program_Funding_and_Outlays, 
                               aes(x = Year, 
                                   y = Outlays_Billions, 
                                   fill = Program)) +
  geom_area() + # Stack programs
  labs(y = "Dollars in Billions",
       x = "Fiscal Year") +
  # Set gradient colors for both categories
  scale_fill_manual(values = c("RCPP IRA" = "#e5f5f9", 
                               "CSP IRA" = "#99d8c9",
                               "EQIP IRA" = "#2ca25f",
                               "RCPP Farm Bill" = "#efedf5",
                               "CSP Farm Bill" = "#bcbddc",
                               "EQIP Farm Bill" = "#756bb1"),
                    labels = c("Regional Conservation Partnership Program IRA", 
                                "Conservation Stewardship Program IRA",
                                "Environmental Quality Incentives Program IRA",
                                "Regional Conservation Partnership Program Farm Bill",
                                "Conservation Stewardship Program Farm Bill",
                                "Environmental Quality Incentives Program Farm Bill")) +
  # Add text labelling IRA and Farm Bill categories
  annotate("text", x = 2029, y = 1, label = "Farm Bill Funding", hjust = 0.5,
           vjust = 1, color = "white", size = 7, angle = 0) +
  annotate("text", x = 2029, y = 4, label = "Inflation Reduction Act Funding", hjust = 0.5, 
           vjust = 1, color = "white", size = 7, angle = 0) +
  # Year labels starting from 2023 and every two years thereafter
  scale_x_continuous(breaks = seq(min(Program_Funding_and_Outlays$Year), 
                                  max(Program_Funding_and_Outlays$Year), by = 1),
                     labels = function(x) ifelse((x - 2023) %% 2 == 0, x, "")) + 
  theme_light(base_size = 20) + # Size up fonts
  theme(legend.position = "bottom") + # Move legend to bottom
  guides(fill = guide_legend(ncol = 2)) # Adjust legend column number to 2

program_funding_plot
