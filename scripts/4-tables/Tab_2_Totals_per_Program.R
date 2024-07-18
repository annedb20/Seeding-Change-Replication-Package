source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Table 2: Total Dollars Obligated, Acres Treated, and Contracts Offered (2014-2023)

# Filter for totals in relevant programs
totals_program <- Contract_Political_Download %>%
  filter(geography_level == "National", obligation_fy == "Total",
         fips_code == "Total", historically_underserved == "Total",
         !program %in% c("AWEP", "AMA", "WHIP")) %>%
  group_by(program) %>%
  # Due to lack of contract_status column, must filter for max of dollars_obligated to filter for "Total" as contract_status
  filter(dollars_obligated == max(dollars_obligated, na.rm = TRUE)) %>%
  ungroup() %>%
  select(program, dollars_obligated, contract_count, treated_acres) %>%
  arrange(-dollars_obligated, program) %>%
  # Add formatting for output
  mutate(across(where(is.numeric), ~scales::comma(.))) %>%  # Add commas to numeric values
  rename_with(.fn = ~sapply(.x, function(name) {
    name %>% 
      gsub("_", " ", .) %>%  # Replace underscores with spaces
      tools::toTitleCase()  # Convert to title case
  }),
  .cols = everything()) %>% 
  kable(caption = "Total Dollars Obligated, Acres Treated, and Contracts Offered From 2014-2023 By Program",
        format = "latex", booktabs = TRUE, escape = FALSE, digits = 0,  align = "lrrr") 

print(totals_program)
