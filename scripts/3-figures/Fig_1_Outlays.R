# source("packages.R")
# source("1-load_data.R")
# source("2-clean_data.R")


# Figure 1: IRA vs Farmbill Outlays

# Create stacked lines plot
program_funding_plot <- 
  Program_Funding_and_Outlays %>% 
  ggplot(aes(x = Year,
             y = Outlays_Billions,
             fill = Program)) +
  geom_area(alpha = 0.95) +  # Add some light transparency
  labs(y = "Dollars \n(Billions)",
       x = "") +
  # Set gradient colors for both categories
  scale_fill_manual(values = c("RCPP IRA" = "#e5f5f9",
                               "CSP IRA" = "#99d8c9",
                               "EQIP IRA" = "#2ca25f",
                               "RCPP Farm Bill" = "#efedf5",
                               "CSP Farm Bill" = "#bcbddc",
                               "EQIP Farm Bill" = "#756bb1"),
                    labels = c("IRA: Regional Cons. Partnership Program", 
                                "IRA: Cons. Stewardship Program",
                                "IRA: Enviro. Quality Incentives Program",
                                "Farm Bill: Regional Cons. Partnership Program",
                                "Farm Bill: Cons. Stewardship Program",
                                "Farm Bill: Enviro. Quality Incentives Program")) +
  # Add text labelling IRA and Farm Bill categories
  annotate("text", x = 2028.75, y = 1.5, label = "Farm Bill\nFunding", hjust = 0.5,
           vjust = 1, color = "white", size = 5, angle = 0, fontface="bold" ) +
  annotate("text", x = 2028.75, y = 4.25, label = "Inflation Reduction \nAct Funding", hjust = 0.5,
           vjust = 1, color = "white", size = 5, angle = 0, fontface="bold") +
  # Year labels starting from 2023 and every two years thereafter
  scale_x_continuous(breaks = seq(min(Program_Funding_and_Outlays$Year), 
                                  max(Program_Funding_and_Outlays$Year), by = 1),
                     labels = function(x) ifelse((x - 2023) %% 2 == 0, x, ""),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = 1:7) + # Set expand to zero for the y-axis
  theme_light(base_size = 14) + 
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 0, vjust = 0.5, size = 14), # Larger text size for axis labels
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 14, face = "bold"), # Slightly smaller text size for title
        legend.title = element_text(size = 12, face="bold"),
        legend.text = element_text(size = 11),   # Larger text size for legend labels
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 14), # Apply larger text size for x-axis labels
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 14, face = "bold"),
        text = element_text(family = "sans") # Apply text size for x-axis title
        ) + 
  guides(fill = guide_legend(ncol = 2)) # Adjust legend column number to 2

program_funding_plot

ggsave.latex(program_funding_plot, filename = file_path("figs/outlays_v2.pdf"))
