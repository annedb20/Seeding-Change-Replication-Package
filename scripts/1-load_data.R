########################################################################
# Author: Anne Bell Carroll (annedb20@vt.edu), Elinor Benami (elinor@vt.edu)
# Date: # Sat Feb 27 12:50:43 2025 ------------------------------
# Purpose: Load data from the dataverse
# Notes:
########################################################################

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

# Census_No._Operations
Census_No._Crop_Operations <- get_dataframe_by_name(
  filename = "Census_No._Operations.tab",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_delim
  )

#	Census_Cropland_Acres
Census_Cropland_Acres <- 	get_dataframe_by_name(
  filename = "Census_Total_Cropland_Acres.tab",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_delim
  )

# Census_Cover_Crop
Census_Cover_Crop <- get_dataframe_by_name(
  filename = "Census_Cover_Crop.tab",
  dataset = "https://doi.org/10.7910/DVN/MMXD4Y",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_delim
  )

# Census_Tillage
Census_Tillage <- get_dataframe_by_name(
  filename = "Census_Reduced_and_No_Till.tab",
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
