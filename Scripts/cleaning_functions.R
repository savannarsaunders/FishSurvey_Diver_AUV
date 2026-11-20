## Cleaning FishCountData Excel File
# All are functions

# Key:
#   
#   C = Diver Physical Clipboard
#   CQ = Diver Clipboard Quick Survey (~3min)
#   D = Diver GoPro Video
#   DC = Diver GoPro Video Attached to Clipboard
#   DQ = Diver Quick Survey Video (~3min)
#   R = Rangerbot GoPro Video
#   
#   OC = Open Circut Diver
#   RB = Rangerbot AUV


# Libraries ---------------------------------------------------------------
# library(tidyverse)
# library(readxl)
# library(janitor)
# library(dplyr)
# library(stringr)

# Metadata ----------------------------------------------------------------
metafish <- function(metadata_excel) {
  metadata_fish <- read_xlsx({{metadata_excel}}, sheet = "FishMetadata") |> 
    select("SppCode":"TrophicLevel")
  
  ## finds all instances where the common name appears twice bc there are two different species
  # dup_cn <- metadata_fish[duplicated(metadata_fish$CommonName) | duplicated(metadata_fish$CommonName, fromLast = TRUE), ] 
  # 
  # dup_spc <- metadata_fish[duplicated(metadata_fish$SppCode) | duplicated(metadata_fish$SppCode, fromLast = TRUE), ]
  
  # Multiple common names and species codes are used more than once, therefore I combined them where necessary so when joining to actual data, it was a one to one relationship (did not add more rows with multiple matches)
  ## Many to many adds rows bc there are multiple occurances of some of the common names due to different species genus names > Fix: collapase all occurances where this is true and make the sppcode & scientific name the family name, keep the common name similar
  condensed_metadata_fish <- metadata_fish |> #common name 
    group_by(CommonName) |> 
    summarise(
      ScientificName = str_flatten(unique(ScientificName), collapse = " / "),
      SppCode = str_flatten(unique(SppCode), collapse = "/"),
      Family = str_flatten(unique(Family), collapse = "/"), 
      TrophicLevel = str_flatten(unique(TrophicLevel), collapse = "/"),
      .groups = "drop")
  
  condensed_metadata_fish <- condensed_metadata_fish |> #species code
    group_by(SppCode) %>%
    summarise(
      ScientificName = str_flatten(unique(ScientificName), collapse = " / "),
      CommonName = str_flatten(unique(CommonName), collapse = "/"),
      Family = str_flatten(unique(Family), collapse = "/"), 
      TrophicLevel = str_flatten(unique(TrophicLevel), collapse = "/"),
      .groups = "drop")
  
  
  lookup <- condensed_metadata_fish |>
    select(everything()) |>
    pivot_longer(
      cols = c(SppCode, CommonName),
      names_to = "match_type",
      values_to = "species_lookup") |>  
    # Adds back in the common name columns
    left_join(condensed_metadata_fish |> 
                select(ScientificName, CommonName), by = join_by(ScientificName == ScientificName), na_matches = "never") |> 
    select(-match_type)
}

# metadata <- metafish("Data/Fish Count Data.xlsx")

# Videos ------------------------------------------------------------------
  VideoDataCleaning_time <- function(FishCountExcel, metafish) {
    videos <- read_xlsx({{FishCountExcel}}, sheet = "FishCountVideo") |>  #all videos diver and rangerbot
      select(Observer, Transect:ColorPhase8) |> 
      filter(if_any(everything(), ~ !is.na(.))) |>  #removes rows where each column is NA
 #new column with all seconds
      mutate(
        TimeStamp_sec = period_to_seconds(ms(TimeStamp))) |> 
# new column total duration in sec of the total video (do not add this column)
      group_by(Observer, Transect, SurveyDate, SurveyLocation,C_D_R) |>
      mutate(
        TotalDuration_sec = max(TimeStamp_sec, na.rm = TRUE) -
          min(TimeStamp_sec, na.rm = TRUE)) |> 
      mutate(
        across(
          matches("^(Species|ColorPhase|Behaviors)[1-8]$"),
          as.character
        )
      ) |> 
      mutate(
        across(
          matches("^(Count)[1-8]$"),
          as.numeric
        )
      )
    
    videos$TotalDuration_min <- videos$TotalDuration_sec/60
    
    videos_lng <- videos |>
      pivot_longer(
        cols = matches("^(Species|Count|Behaviors|ColorPhase)[1-8]$"),
        names_to = c(".value", "SpeciesNumber"),
        names_pattern = "(Species|Count|Behaviors|ColorPhase)([1-8])") |> 
      filter(!is.na(Count), Count != "") |> # Removes rows where the count is NA (keeps all 0)
      select(-SpeciesNumber) #not needed - this row was just the order number of species for entries
    
    clean_videos_lng <- videos_lng |> ## Makes all the names/entries the same format
  # Color Phase
      mutate(
        ColorPhase = case_when(
          str_detect(ColorPhase, regex("^(TERMINAL|adult|term|terinal|T)", ignore_case = TRUE)) ~ "terminal",
          str_detect(ColorPhase, regex("^(initial|I|intiial|iniital|intial)", ignore_case = TRUE)) ~ "initial",
          str_detect(ColorPhase, regex("^(juv|juvenile)", ignore_case = TRUE)) ~ "juvenile",
          TRUE ~ ColorPhase)) |> 
  # Behavior
      mutate(# Identify schooling first
        Schooling = case_when(
          str_detect(Behaviors, regex("schooling", ignore_case = TRUE)) ~ TRUE, TRUE ~ NA),
        # Remove schooling from the behavior string
        Behaviors_clean = Behaviors |> 
          str_remove_all(regex("schooling", ignore_case = TRUE))  |> 
          str_replace_all("[,/;]", " ")  |> 
          str_squish(),
        # Clean Behaviors
        Behaviors = case_when(
          # Lateral movers
          str_detect(Behaviors_clean, regex("^(lm|LM|lateral movers)", ignore_case = TRUE)) ~ "lateral movers",
          # Local reactives
          str_detect(Behaviors_clean, regex("^(lr|local reactives)", ignore_case = TRUE)) ~ "local reactives",
          # Benthic pursuers
          str_detect(Behaviors_clean, regex("^(bp|benthic pursuers)", ignore_case = TRUE)) ~ "benthic pursuers",
          # Pelagic pursuers
          str_detect(Behaviors_clean, regex("^(pp|pelagic pursuers)", ignore_case = TRUE)) ~ "pelagic pursuers",
          # Descenders
          str_detect(Behaviors_clean, regex("^(descender|descenders|decender|decenders|descenderd)", ignore_case = TRUE)) ~ "descenders",
          # Unfazed
          str_detect(Behaviors_clean, regex("^unfazed",ignore_case = TRUE)) ~ "unfazed",
          # Benthic
          str_detect(Behaviors_clean, regex("^(benthic|bennthic|bethic|betnhic|bnethic|bentthic|bnthic)", ignore_case = TRUE)) ~ "benthic",
          # Pelagic
          str_detect(Behaviors_clean, regex("^(pelagic|pelagis)", ignore_case = TRUE)) ~ "pelagic",
          TRUE ~ Behaviors_clean))|> 
      select(-Behaviors_clean, -TotalDuration_sec, -TimeStamp_sec) |> 
    ##removes any rows where species = NA --> takes out all time points where count = 0s
      filter(!is.na(Species))
    
    ##Joining fish metadata
    clean_videos_lng_sp <- clean_videos_lng |> 
      mutate(species_lookup = str_to_lower(str_squish(Species))) |> 
      left_join({{metafish}} |> 
                  mutate(
                    species_lookup = str_to_lower(str_squish(species_lookup))),
                by = "species_lookup")
    }
  
# videos <- VideoDataCleaning_time("Data/Fish Count Data.xlsx", metadata)
  
  
  # Clipboard Entries -------------------------------------------------------
  ClipboardDataCleaning <- function(FishCountExcel, videoDF_min) {
    clipboard_entries <- read_xlsx({{FishCountExcel}}, sheet = "OCClipboard") |> 
      drop_na(DateEntered, Digitizer,SurveyLocation,SurveyDate, SurveyTime, C_D_R,Transect,Observer,ScientificName,CommonName,TrophicGroup,Species) |> 
      select(SurveyLocation, SurveyDate, C_D_R:Observer, ScientificName:TrophicGroup, '0-5':SppTotal)|> 
      mutate(Transect = as.numeric(Transect)) |> 
      mutate( ## Change DC to C bc in the videos DC indicates a gopro on the clipboard. C means a physical clipboard count
        C_D_R = case_when(
          str_detect(C_D_R, "DC") ~ "C",
          TRUE ~ C_D_R)) |> 
      rename(TrophicLevel = TrophicGroup) |> 
    ##Adding in total duration in min from the videos from the exact same transect surveys
        left_join({{videoDF_min}} |> 
            filter(C_D_R == "D" | C_D_R == "DQ") |> 
            select(Observer, Transect, SurveyDate, SurveyLocation, TotalDuration_min) |> 
            distinct(Observer, Transect, SurveyDate, SurveyLocation, .keep_all = TRUE),
          by = c("Observer","Transect","SurveyDate","SurveyLocation"),
          relationship = "many-to-one") |> 
        select(-C_D_R.y) |> 
        rename("C_D_R" = "C_D_R.x")
  }

  # clip <- ClipboardDataCleaning("Data/Fish Count Data.xlsx", videos)
