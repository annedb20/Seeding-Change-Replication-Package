source("packages.R")
source("1-load_data.R")
source("2-clean_data.R")

# Stat 6: PCSC Monitoring by Org Type ----

# Merge with PCSC dataset and clean
PCSC_with_actors <- left_join(Partnerships_for_Climate_Smart_Commodities, PCSC_Partner_Types, by = "Applicant")

PCSC_with_actors <- PCSC_with_actors %>%
  select(-Notes, - `Agreement found?`, -`Agreement link`)

PCSC_with_actors$`Actor Type` <- gsub("Firm ", "Firm", PCSC_with_actors$`Actor Type`)

# Calculate frequency of different types of actors
freq_actors <- table(PCSC_with_actors$`Actor Type`) 

freq_actors_df <- data.frame(freq_actors) %>%
  rename(Actor.Type = Var1)

# Create sets of phrases to search for
soil_sampling <- c("soil sampling", "soil sample", "Soil sampling", "Soil Sampling",
                   "soil and plan sampling", "soil quality assessment", "soil analysis",
                   "soil health analysis", "soil's physical properties", 
                   "soil health testing", "core sampling for carbon")
remote_sensing <- c("remote sens", "Remote sens", "remote-sens", "remotely sens",
                    "satellite", "Satellite", "drone", "Drone")
life_cycle_assessment <- c("LCA", "life cycle assessment", "Life Cycle Assessment",
                           "Life cycle assessment")
artificial_intelligence <- c("AI", "artificial intelligence", "Artifical Intelligence",
                             "machine learning", "Machine learning", "Machine Learning")
Fieldprint <- c("Fieldprint", "FieldPrint")
field_level <- c("In-field", "in-field", "field-level", "field level", "Field level",
                 "Field-level", "Field measurements", "field measurement", 
                 "field-scale", "field scale", "in field")

# Compile list of sets of phrases to search for
phrases_lists <- list(soil_sampling, remote_sensing, life_cycle_assessment, 
                      artificial_intelligence, Fieldprint, field_level)

# Create function to count occurrences of phrases
count_phrases <- function(PCSC_with_actors, phrases_lists) {
  for (i in seq_along(phrases_lists)) {
    phrases <- phrases_lists[[i]]
    column_name <- paste0("Phrase.Count.", i)
    PCSC_with_actors[[column_name]] <- sapply(PCSC_with_actors$`MMRV Highlights`, function(text) {
      as.integer(any(sapply(phrases, (function(phrase) str_count(text, fixed(phrase)))))) 
    })
  }
  return(PCSC_with_actors)
}

# Run function against data
PCSC_with_actors_counts <- count_phrases(PCSC_with_actors, phrases_lists)

# Create function to aggregate by actor type
aggregate_phrase_counts <- function(PCSC_with_actors_counts) {
  PCSC_with_actors_counts %>%
    group_by(Actor.Type) %>%
    summarize(across(starts_with("Phrase.Count"), sum, .names = "Total.{.col}"))
}

# Run function against data
aggregated_counts <- aggregate_phrase_counts(PCSC_with_actors_counts)

# Rename variables for easier viewing
aggregated_counts <- aggregated_counts %>%
  rename(Soil_sampling_counts = Total.Phrase.Count.1,
         Remote_sensing_counts = Total.Phrase.Count.2,
         LCA_counts = Total.Phrase.Count.3,
         AI_counts = Total.Phrase.Count.4,
         Fieldprint_counts = Total.Phrase.Count.5,
         field_level_counts = Total.Phrase.Count.6) 

# Add in actor type frequency to allow weighted values
weighted_counts <- merge(aggregated_counts, freq_actors_df, by = "Actor.Type")

# Find percentage of actors who plan to practice certain MMRV activities
weighted_counts <- weighted_counts %>%
  mutate(Soil_sampling_weighted = Soil_sampling_counts/Freq * 100,
         Remote_sensing_weighted = Remote_sensing_counts/Freq * 100,
         LCA_weighted = LCA_counts/Freq * 100,
         AI_weighted = AI_counts/Freq * 100,
         Fieldprint_weighted = Fieldprint_counts/Freq * 100,
         field_level_weighted = field_level_counts/Freq * 100)

View(weighted_counts)