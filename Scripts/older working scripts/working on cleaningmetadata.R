# Finished result ---------------------------------------------------------

metadata_fish <- read_xlsx("Data/Fish Count Data.xlsx", sheet = "FishMetadata") |> 
  select("SppCode":"TrophicLevel")

## finds all instances where the common name appears twice bc there are two different species
# dup_cn <- metadata_fish[duplicated(metadata_fish$CommonName) | duplicated(metadata_fish$CommonName, fromLast = TRUE), ] 
# 
# dup_spc <- metadata_fish[duplicated(metadata_fish$SppCode) | duplicated(metadata_fish$SppCode, fromLast = TRUE), ]

# Multiple common names and species codes are used more than once, therefore I combined them where necessary so when joining to actual data, it was a one to one relationship (did not add more rows with multiple matches)
ccondensed_metadata_fish <- metadata_fish |> #common name 
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

# Working Area ------------------------------------------------------------


metadata_fish <- read_xlsx("Data/Fish Count Data.xlsx", sheet = "FishMetadata") |> 
  select("SppCode":"TrophicLevel") 

## finds all instances where the common name appears twice bc there are two different species
dup_cn <- metadata_fish[duplicated(metadata_fish$CommonName) | duplicated(metadata_fish$CommonName, fromLast = TRUE), ] 

dup_spc <- metadata_fish[duplicated(metadata_fish$SppCode) | duplicated(metadata_fish$SppCode, fromLast = TRUE), ] 

dups <- bind_rows(dup_cn, dup_spc) |> 
  distinct()
  


# modfam <- dups |> 
#   mutate(
#     ScientificName = case_when(
#       # HAST
#       SppCode == "HAST" ~ "Haemulidae",
#       
#       # STSP
#       CommonName == "damselfish species" ~ "Pomacentridae",
#       
#       # SCSP & SPSP Parrotfish Species
#       CommonName == "parrotfish species" ~ "Scaridae",
#       
#       
#       TRUE ~ ScientificName
#     ),
#     
#     SppCode = case_when(
#       # CHAT
#       ScientificName == "Chriodorus atherinoides" ~ "Hemiramphidae",
#       ScientificName == "Chilomycterus antillarum" ~ "Diodontidae",
#       
#       # HACA
#       ScientificName == "Halichoeres caudalis" ~ "Labridae",
#       ScientificName == "Haemulon carbonarium" ~ "Haemulidae",
#       
#       # HAST
#       SppCode == "HAST" ~ "Haemulidae",
#       
#       # HOSP
#       ScientificName == "Holacanthus sp." ~ "Pomacanthidae",
#       ScientificName == "Holocentridae sp." ~ "Holocentridae",
#       
#       # OGSP
#       ScientificName == "Ogcocephalus sp." ~ "Ogcocephalidae",
#       ScientificName == "Opistognathus sp." ~ "Opistognathidae",
#       
#       # SCSP
#       ScientificName == "Sciaenidae sp." ~ "Sciaenidae",
#       
#       # SCSP & SPSP Parrotfish Species
#       CommonName == "parrotfish species" ~ "Scaridae",
#       
#       # SPSP
#       ScientificName == "Sparidae sp." ~ "Sparidae",
#       ScientificName == "Scorpaena sp." ~ "Scorpaenidae",
#       
#       # STSP
#       CommonName == "damselfish species" ~ "Pomacentridae",
#       
#       TRUE ~ SppCode
#     ),
#     
#     CommonName = case_when(
#       # Dashback goby
#       SppCode == "MISI" ~ "Gobiidae",
#       SppCode == "SGDA" ~ "Syngnathidae",
#       
#       # HAST
#       ScientificName == "Haemulon steindachneri" ~ "Haemulidae",
#       ScientificName == "Haemulon striatum" ~ "Haemulidae",
#       
#       TRUE ~ CommonName),
#     
#     TrophicLevel = case_when(
#       # STSP
#       CommonName == "damselfish species" ~ "unkown",
#       
#       TRUE ~ TrophicLevel)
#   ) |> 
#   distinct()


condensed_metadata_fish <- metadata_fish %>%
  group_by(CommonName) %>%
  summarise(
    # Keeps and combines unique scientific names (e.g., "Pomacentridae sp. / Pomacentridae")
    ScientificName = str_flatten(unique(ScientificName), collapse = " / "),
    
    # Collapses unique codes. If duplicates exist (like STSP), it keeps just one.
    # If they are different (like MISI and SGDA), it turns into "MISI/SGDA"
    SppCode = str_flatten(unique(SppCode), collapse = "/"),
    
    # Cleans up the family if there was a typo in the original data
    Family = str_flatten(unique(Family), collapse = "/"), 
    
    # Combines conflicting feeding guilds into "inv/herb"
    TrophicLevel = str_flatten(unique(TrophicLevel), collapse = "/"),
    
    .groups = "drop"
  )

condensed_metadata_fish <- condensed_metadata_fish %>%
  group_by(SppCode) %>%
  summarise(
    # Keeps and combines unique scientific names (e.g., "Pomacentridae sp. / Pomacentridae")
    ScientificName = str_flatten(unique(ScientificName), collapse = " / "),
    CommonName = str_flatten(unique(CommonName), collapse = "/"),
    
    # Cleans up the family if there was a typo in the original data
    Family = str_flatten(unique(Family), collapse = "/"), 
    
    # Combines conflicting feeding guilds into "inv/herb"
    TrophicLevel = str_flatten(unique(TrophicLevel), collapse = "/"),
    
    .groups = "drop")


lookup <- condensed_metadata_fish |>
  select(everything()) |>
  pivot_longer(
    cols = c(SppCode, CommonName),
    names_to = "match_type",
    values_to = "species_lookup") |>  

  left_join(condensed_metadata_fish |> 
              select(ScientificName, CommonName), by = join_by(ScientificName == ScientificName), na_matches = "never") |> 
  select(-match_type)



## collapses them down to just family name and one common name for each duplicate - does not allow for more rows to be added when using fish meta data and common names to match in other dfs



## testing move to function after working
videos <- read_xlsx("Data/Fish Count Data.xlsx", sheet = "FishCountVideo") |>  #all videos diver and rangerbot
  # filter(SurveyLocation != "GKT") |> # Dont need bc moved to DataNOTusing tab
  select(Observer, Transect:ColorPhase8) |> 
  filter(if_any(everything(), ~ !is.na(.))) #removes rows where each column is NA

videos <- videos |> #new column with all seconds
  mutate(
    TimeStamp_sec = period_to_seconds(ms(TimeStamp))) 

videos <- videos |> # new column total duration in sec of the total video (do not add this column)
  group_by(Observer, Transect, SurveyDate, SurveyLocation,C_D_R) |>
  mutate(
    TotalDuration_sec = max(TimeStamp_sec, na.rm = TRUE) -
      min(TimeStamp_sec, na.rm = TRUE))

videos$TotalDuration_min <- videos$TotalDuration_sec/60

videos <- videos |>
  pivot_longer(
    cols = matches("^(Species|Count|Behaviors|ColorPhase)[1-8]$"),
    names_to = c(".value", "SpeciesNumber"),
    names_pattern = "(Species|Count|Behaviors|ColorPhase)([1-8])") |> 
  filter(!is.na(Count), Count != "") |> # Removes rows where the count is NA (keeps all 0)
  select(-SpeciesNumber) #not needed - this row was just the order number of species for entries


clean_videos_lng <- videos |> ## Makes all the names/entries the same format
  mutate(
    ColorPhase = case_when(
      str_detect(ColorPhase, regex("^(TERMINAL|adult|term|terinal|T)", ignore_case = TRUE)) ~ "terminal",
      str_detect(ColorPhase, regex("^(initial|I|intiial|iniital|intial)", ignore_case = TRUE)) ~ "initial",
      str_detect(ColorPhase, regex("^(juv|juvenile)", ignore_case = TRUE)) ~ "juvenile",
      TRUE ~ ColorPhase)) |> 
  mutate(
    # Identify schooling first
    Schooling = case_when(
      str_detect(Behaviors, regex("schooling", ignore_case = TRUE)) ~ TRUE,
      TRUE ~ NA
    ),
    
    # Remove schooling from the behavior string
    Behaviors_clean = Behaviors %>%
      str_remove_all(regex("schooling", ignore_case = TRUE)) %>%
      str_replace_all("[,/;]", " ") %>%
      str_squish(),
    
    # Clean Behaviors
    Behaviors = case_when(
      
      # Lateral movers
      str_detect(
        Behaviors_clean,
        regex("^(lm|LM|lateral movers)",
              ignore_case = TRUE)
      ) ~ "lateral movers",
      
      # Local reactives
      str_detect(
        Behaviors_clean,
        regex("^(lr|local reactives)",
              ignore_case = TRUE)
      ) ~ "local reactives",
      
      # Benthic pursuers
      str_detect(
        Behaviors_clean,
        regex("^(bp|benthic pursuers)",
              ignore_case = TRUE)
      ) ~ "benthic pursuers",
      
      # Pelagic pursuers
      str_detect(
        Behaviors_clean,
        regex("^(pp|pelagic pursuers)",
              ignore_case = TRUE)
      ) ~ "pelagic pursuers",
      
      # Descenders
      str_detect(
        Behaviors_clean,
        regex("^(descender|descenders|decender|decenders|descenderd)",
              ignore_case = TRUE)
      ) ~ "descenders",
      
      # Unfazed
      str_detect(
        Behaviors_clean,
        regex("^unfazed",
              ignore_case = TRUE)
      ) ~ "unfazed",
      
      # Benthic
      str_detect(
        Behaviors_clean,
        regex("^(benthic|bennthic|bethic|betnhic|bnethic|bentthic|bnthic)",
              ignore_case = TRUE)
      ) ~ "benthic",
      
      # Pelagic
      str_detect(
        Behaviors_clean,
        regex("^(pelagic|pelagis)",
              ignore_case = TRUE)
      ) ~ "pelagic",
      
      TRUE ~ Behaviors_clean))  |> 
  select(-Behaviors_clean, -TotalDuration_sec, -TimeStamp_sec)

##removes any rows where species = NA takes out all time points where count = 0s
nclean_videos_lng_n <- clean_videos_lng |> 
  filter(!is.na(Species))

##Joining fish metadata
## Many to many adds rows bc there are multiple occurances of some of the common names due to different species genus names > Fix: collapase all occurances where this is true and make the sppcode & scientific name the family name, keep the common name similar

spclean_videos_lng_sp <- nclean_videos_lng_n |> 
  left_join(lookup, by = join_by(Species == species_lookup))  

