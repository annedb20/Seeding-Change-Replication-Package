source("packages.R")

# Download the data from the dataverse

# CSV Files
# Contract_Demographic_Download
Contract_Demographic_Download <- get_dataframe_by_name(
  filename = "Contract_Demographic_Download.csv",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_csv
)

# Contract_Download_Table_HUP
Contract_Download_Table_HUP <- get_dataframe_by_name(
  filename = "Contract_Download_Table_HUP.csv",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_csv
)

# Contract_Political_Download
Contract_Political_Download <- get_dataframe_by_name(
  filename = "Contract_Political_Download.csv",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_csv
)

# Practice_Political_Download
Practice_Political_Download <- get_dataframe_by_name(
  filename = "Practice_Political_Download.csv",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_csv
)

# TAB files
# Partnership_for_Climate-Smart_Commodities_Projects
PCSC_Projects <- get_dataframe_by_name(
  filename = "Partnership_for_Climate-Smart_Commodities_Projects.tab",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_delim
)

# Producer_Numbers_by_State
Producer_Numbers_by_State <- get_dataframe_by_name(
  filename = "Producer_Numbers_by_State.tab",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_delim
)

# Program_Funding_and_Outlays
Program_Funding_and_Outlays <- get_dataframe_by_name(
  filename = "Program_Funding_and_Outlays.tab",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_delim
)

# XLSX File
# Program_Funding_Levels
Program_Funding_Levels <- get_dataframe_by_name(
  filename = "Program_Funding_Levels",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = function(x) {read_xlsx(path = x, sheet = 1, skip = 1, n_max=7)}
)
