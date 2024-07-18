source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Statistic 2: Avg Value of Contracts per Program

# Create dataset focused on programs of interest
contract_values <- Contract_Political_Download %>%
  filter(historically_underserved == "Total", state == "Total", 
         county_name == "Total", !program %in% c("AWEP", "WHIP", "AMA"),
         obligation_fy == "Total", suppressed != "TRUE") %>%
  mutate(program = case_when(program %in% c("CSP", "CSP-GCI") ~ "CSP",
                             program %in% c("RCPP-EQIP", "RCPP-CSP") ~ "RCPP",
                             program == "EQIP" ~ "EQIP")) %>%
  arrange(state, program, obligation_fy, dollars_obligated) %>% 
  group_by(state, program, obligation_fy) %>%
  filter(dollars_obligated == max(dollars_obligated)) %>% # Take Total for contract status
  ungroup() %>%
  summarize(program = program,
            avg_contract_value = dollars_obligated / contract_count) # Divide dollars among contracts

View(contract_values)