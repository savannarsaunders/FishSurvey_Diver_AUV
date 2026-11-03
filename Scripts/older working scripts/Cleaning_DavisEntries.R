## Fixing Davis Strobel (DS) entries of fish counts
# 9/29/2026 Savanna Saunders
#He counted every fish in the frame including the ones in the previous frame --> fish individual counted multiple times 
# goal: subtract each species count is most previous frame to get the number of new fish in that frame
## Steps:
# Compares each row to the previous row.
# Searches for the same species across all 6 species columns.
# Subtracts the previous count from the current count.
# If the result is 0 or negative, removes that species from the current row.
# Moves Species, Count, Behaviors, ColorPhase, ScientificName, CommonName, and TrophicGroup together.
# Shifts remaining species to the left so there are no gaps.
# Leaves the first row unchanged.

library(tidyverse)
library(dplyr)

davis <- read_csv('Data/davis_count - Sheet1.csv') |> 
  select("AnalysisDate":"ColorPhase6")


# STEP 1: Define the columns

species_cols  <- paste0("Species", 1:6)
count_cols    <- paste0("Count", 1:6)
behavior_cols <- paste0("Behaviors", 1:6)
color_cols    <- paste0("ColorPhase", 1:6)


# STEP 2: Make a new copy of your original dataset

df_new <- davis


# STEP 3: Subtract previous-row counts
# ONLY RUN ONCE OR WILL KEEP CHANGING NUMBERS
for (i in 2:nrow(df_new)) {
  
  # Information from the previous row
  previous_species <- as.character(unlist(df_new[i - 1, species_cols]))
  previous_counts  <- as.numeric(unlist(df_new[i - 1, count_cols]))
  
  # Look at each species position in the current row
  for (j in 1:6) {
    
    current_species <- as.character(df_new[[species_cols[j]]][i])
    current_count   <- as.numeric(df_new[[count_cols[j]]][i])
    
    # Skip if there is no species or count
    if (is.na(current_species) || current_species == "" ||
        is.na(current_count)) {
      next
    }
    
    # Find the same species ANYWHERE in the previous row
    match_position <- which(previous_species == current_species)
    
    # If species was present in previous row
    if (length(match_position) > 0) {
      
      previous_count <- previous_counts[match_position[1]]
      
      if (!is.na(previous_count)) {
        
        # Calculate difference
        new_count <- current_count - previous_count
        
        # If difference is zero or negative,
        # remove the species from the current row
        if (new_count <= 0) {
          
          df_new[[species_cols[j]]][i] <- NA
          df_new[[count_cols[j]]][i] <- NA
          df_new[[behavior_cols[j]]][i] <- NA
          df_new[[color_cols[j]]][i] <- NA
          
        } else {
          
          # Positive difference: replace count
          df_new[[count_cols[j]]][i] <- new_count
        }
      }
    }
  }
}


# STEP 4: Shift all species information to the left
# ONLY RUN ONCE OR WILL KEEP CHANGING NUMBERS
for (i in 1:nrow(df_new)) {
  
  # Get all 7 pieces of information
  species    <- as.character(unlist(df_new[i, species_cols]))
  counts     <- as.numeric(unlist(df_new[i, count_cols]))
  behaviors  <- as.character(unlist(df_new[i, behavior_cols]))
  colors     <- as.character(unlist(df_new[i, color_cols]))
  
  # Find species that are still present
  keep <- !is.na(species) & species != ""
  
  # Keep the entire species package together
  species_new    <- species[keep]
  counts_new     <- counts[keep]
  behaviors_new  <- behaviors[keep]
  colors_new     <- colors[keep]
  
  # Clear all six positions
  for (j in 1:6) {
    
    df_new[[species_cols[j]]][i] <- NA
    df_new[[count_cols[j]]][i] <- NA
    df_new[[behavior_cols[j]]][i] <- NA
    df_new[[color_cols[j]]][i] <- NA
  }
  
  # Put the remaining species packages back at the beginning
  if (length(species_new) > 0) {
    
    for (j in 1:length(species_new)) {
      
      df_new[[species_cols[j]]][i] <- species_new[j]
      df_new[[count_cols[j]]][i] <- counts_new[j]
      df_new[[behavior_cols[j]]][i] <- behaviors_new[j]
      df_new[[color_cols[j]]][i] <- colors_new[j]
    }
  }
}


# STEP 5: Look at your finished dataset and save

View(df_new)

davis_clean <- df_new
write.csv(davis_clean, "Data/davis_clean.csv", row.names = FALSE)

