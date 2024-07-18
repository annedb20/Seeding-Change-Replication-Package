source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Table 5: Historical Support for CSAF Practices

# Create new dataset with practice-specific information for all years on a national level
practices_filtered <- Practice_Political_Download %>%
  filter(obligation_fy != "Total", 
         certification_fy == "Total",
         geography_level == "National",
         practice_code != "Total",
         !program %in% c("WHIP","AMA", "AWEP")) # Exclude unnecessary programs

# Edit dataset to only include CSAF practices (derived from https://www.nrcs.usda.gov/sites/default/files/2023-10/NRCS-CSAF-Mitigation-Activities-List.pdf)
CSAF <- practices_filtered %>%
  group_by(obligation_fy) %>%
  mutate(total_dollars_ob_fy = sum(dollars_obligated, na.rm = TRUE)) %>% # Calculate total $ for each year
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
  group_by(obligation_fy) %>% # Separate for percentages in different years
  mutate(CS_dollars_ob_fy = sum(dollars_obligated, na.rm = TRUE)) %>% # Find sum of funds supporting CSAF practices
  mutate(percent_CS = CS_dollars_ob_fy / total_dollars_ob_fy * 100) %>% # Find percentage of funds each year supporting CSAF practices
  ungroup()

# Create table summarizing yearly changes
CSAF_yrs <- CSAF %>%
  summarize(obligation_fy = unique(obligation_fy),
            total_dollars_ob_fy = unique(total_dollars_ob_fy),
            CS_dollars_ob_fy = unique(CS_dollars_ob_fy),
            percent_CS = unique(percent_CS))

# Arrange in ascending years
CSAF_yrs$obligation_fy <- as.numeric(CSAF_yrs$obligation_fy)
CSAF_yrs <- CSAF_yrs %>%
  arrange(obligation_fy) %>%
  # Adjust to millions
  mutate(total_dollars_ob_fy = (total_dollars_ob_fy / 1e6),
         CS_dollars_ob_fy = (CS_dollars_ob_fy / 1e6)) %>%
  # Latex format
  kable(caption = "Historical Support for CSAF Practices across RCPP, CSP, and EQIP (2014-2023)",
        format = "latex", digits = 1, booktabs = TRUE, escape = FALSE, align = "lrrr")

print(CSAF_yrs)

