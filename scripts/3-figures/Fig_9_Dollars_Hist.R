source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 9: Dollars Obligated by Program and Fiscal Year (2014-2023)

# Generate a dataset that only shows the total annual values (dollars obligated, contracts) by program and state
totals_state <- 
  Contract_Political_Download %>% 
  filter(geography_level == "State",
         county_name == "Total",
         historically_underserved == "Total",
         suppressed != "TRUE"
  ) %>% 
  arrange(state, program, obligation_fy, dollars_obligated) %>% 
  group_by(state, program, obligation_fy) %>%
  # When using this dataset, taking the maximum dollars_obligated essentially filters for "total" contract statuses
  filter(dollars_obligated == max(dollars_obligated)) %>%
  ungroup()  

# Generate a stacked bar chart that shows the total dollars obligated by fiscal year, facetted by state
dollars_year_state_program <- 
  totals_state %>% 
  filter(dollars_obligated != "Total", 
         obligation_fy != "Total") %>% 
  mutate(dollars_obligated = dollars_obligated / 1e6) %>% # Divide by millions
  # Ensure obligation_fy is treated as a factor and ordered if not already
  mutate(obligation_fy = factor(obligation_fy, levels = unique(obligation_fy))) %>%
  ggplot(aes(x = obligation_fy, y = dollars_obligated, fill = program)) +
  geom_col(position = "stack") +
  # Facet_wrap by state should have a free y axis scale
  facet_wrap(~ state, scales = "free_y") +
  labs(title = "Dollars Obligated by Program, Fiscal Year, and State",
       subtitle = "Note y-axis scale varies by state",
       x = "Fiscal Year", 
       y = "Dollars Obligated, in Millions",
       fill = "Program") +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 1, x, "")) +
  theme(axis.text.x = element_text(angle = 90,  hjust = 1, vjust = 0.5),
        legend.position = "bottom")

dollars_year_state_program