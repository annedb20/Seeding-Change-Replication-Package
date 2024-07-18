source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Figure 5: Historical Share of Support for CSAF Practices

# Filter information from relevant programs
CSAF <- Practice_Political_Download %>%
  filter(obligation_fy != "Total",
         certification_fy == "Total",
         geography_level == "National",
         practice_code != "Total",
         !program %in% c("WHIP","AMA", "AWEP"))

# Find yearly dollar totals before filtering for CSAF practices
CSAF <- CSAF %>%
  group_by(obligation_fy) %>%
  mutate(total_dollars_ob_fy = sum(dollars_obligated, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(practice_code %in% c(327, "E327A", "E327B", 328, "E328A", "E328B",
                              "E328E", "E328F", "E328N", "E328O", 329, "E329A", 
                              "E329B", "E329C", "E329D", "E329E", 332, 336, 340,
                              "E340A", "E340B", "E340C", "E340D", "E340F", 
                              "E340G", "E340H", "E340I", "E340J", 345, "E345A",
                              "E345B", "E345C", "E345D", "E345E", 386, "E386A",
                              "E386B", "E386C", "E386D", "E386E", 393, "E393A",
                              412, "E412A", 484, "E484A", "E484B", "E484C", 
                              "E484D", 585, 601, 603, 590, "E590A", "E590B",
                              "E590C", "E590D", 317, 313, 366, 367, 592, 632,
                              314, "E314A", 315, "E315A", 338, "E338A", 512, 
                              "E512A", "E512B", "E512C", "E512D", "E512I",
                              "E512J", "E512L", "E512M", 528, "E528A", "E528F",
                              "E528G", "E528G", "E528H", "E528I", "E528J",
                              "E528L", "E528M", "E528N", "E528O", "E528P",
                              "E528R", "E528S", "E528T", "E528U", 550, "E550A",
                              "E550B", 311, 342, 379, 380, 381, "E381A", 383,
                              "E383A", 384, "E384A", 390, "E390A", "E390B", 391,
                              "E391A", "E391B", "E391C", 420, "E420A", "E420B",
                              422, 612, "E612B", "E612C", "E612G", 643, "E643D",
                              666, "E666A", "E66D", "E666E", "E666F", "E666H",
                              "E666I", "E666J", "E666K", "E666L", "E666P",
                              "E666R", "E666S", 453, 543, 372, "E372A", "E372B",
                              374, 430, 441, 442, 533, "E533C", "E533D", 672,
                              670, 657, 449, "E449B", "B000FST5", "B000CPL25")) %>%
  group_by(obligation_fy) %>%
  mutate(CS_dollars_ob_fy = sum(dollars_obligated, na.rm = TRUE)) %>%
  mutate(percent_CS = CS_dollars_ob_fy / total_dollars_ob_fy * 100) %>% # Find yearly percentage of CSAF funding
  ungroup()

# Summarize yearly changes
CSAF_yrs <- CSAF %>%
  summarize(obligation_fy = unique(obligation_fy),
            total_dollars_ob_fy = unique(total_dollars_ob_fy),
            CS_dollars_ob_fy = unique(CS_dollars_ob_fy),
            percent_CS = unique(percent_CS))
CSAF_yrs$obligation_fy <- as.numeric(CSAF_yrs$obligation_fy)
CSAF_yrs <- CSAF_yrs %>%
  arrange(obligation_fy)

# Adjust dollars to billions
CSAF_yrs <- CSAF_yrs %>%
  mutate(total_dollars_ob_fy_bil = total_dollars_ob_fy/1e9,
         CS_dollars_ob_fy_bil = CS_dollars_ob_fy/1e9) %>%
  select(-total_dollars_ob_fy, -CS_dollars_ob_fy) %>%
  mutate(Other = total_dollars_ob_fy_bil - CS_dollars_ob_fy_bil) %>%
  select(-total_dollars_ob_fy_bil) %>%
  rename(CSAF = CS_dollars_ob_fy_bil)

# Pivot longer
CSAF_yrs <- CSAF_yrs %>%
  pivot_longer(cols = c("Other", "CSAF"),
               names_to = "type", values_to = "dollars")

# Create stacked bar chart
CS_graph <- ggplot(CSAF_yrs, aes(x = factor(obligation_fy), y = dollars, 
                                 fill = factor(type, levels = c("Other",
                                                                "CSAF")))) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = subset(CSAF_yrs, type == "CSAF"), 
            aes(label = sprintf("%.2f", percent_CS)), # Label with percentage CSAF, limiting to two digits after decimal
            vjust = -0.5, color = "black") +
  scale_x_discrete() + # Show each unique year on x-axis
  labs(x = "Year", y = "Dollars in Billions", fill = "Practices Supported", 
       title = "Historical Share of Support for CSAF Practices") + 
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

CS_graph
