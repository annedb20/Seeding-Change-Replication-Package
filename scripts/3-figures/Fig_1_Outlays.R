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
                    labels = c("IRA: Regional Cons. Partnership", 
                                "IRA: Cons. Stewardship",
                                "IRA: Enviro. Quality Incentives",
                                "Farm Bill: Regional Cons. Partnership",
                                "Farm Bill: Cons. Stewardship",
                                "Farm Bill: Enviro. Quality Incentives")) +
  # Add text labelling IRA and Farm Bill categories
  annotate("text", x = 2028.75, y = 1.5, label = "Farm Bill\nFunding", hjust = 0.5,
           vjust = 1, color = "white", size = 4, angle = 0, fontface="bold" ) +
  annotate("text", x = 2028.75, y = 4.35, label = "Inflation Reduction \nAct Funding", hjust = 0.5,
           vjust = 1, color = "white", size = 4, angle = 0, fontface="bold") +
  # Year labels starting from 2023 and every two years thereafter
  scale_x_continuous(breaks = seq(min(Program_Funding_and_Outlays$Year), 
                                  max(Program_Funding_and_Outlays$Year), by = 1),
                     labels = function(x) ifelse((x - 2023) %% 2 == 0, x, ""),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = 1:7) + # Set expand to zero for the y-axis
  theme_light(base_size = 14) + 
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
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
        text = element_text(family = "sans") # Apply text size for x-axis title
        ) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 2))

program_funding_plot

ggsave.latex(program_funding_plot, filename = file_path("figs/outlays_v2.pdf"), height = 4, width = 6.7, units = "in")
