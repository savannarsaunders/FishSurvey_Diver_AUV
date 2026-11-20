library(tidyverse)


video_count <- read_csv("Data/Pclean_video_counts.csv") |> 
  select(-Species)

fish_per_df <- video_count |>  # Not by species
  group_by(SurveyDate, SurveyLocation, Observer, D_DC_R, Transect, TotalDuration_min, TrophicLevel) |>
  summarise(
    TotalCount = sum(TotalSppCount, na.rm = TRUE),
    .groups = "drop") |> 
  drop_na()

##Removing sites with no comparison data
fish_per_df <- fish_per_df |> 
  filter(SurveyLocation != "CKR") |> 
  filter(SurveyLocation != "FLC" & SurveyDate != as.Date("2026-09-25")) 

ggplot(fish_per_df, aes(x = interaction(D_DC_R, Transect), y = TotalCount, fill = TrophicLevel))+
  geom_bar( stat = "identity",position = "stack") +
  facet_wrap(~ SurveyLocation + SurveyDate,scales = "free_x",
             nrow = 1,
             labeller = labeller(
               SurveyLocation = c(
                 BIT = "Buck Island",
                 BPT = "Black Point",
                 FLC = "Flat Cay",
                 CKR = "Coculus Rock")))+
  labs(
    y = "Fish Abundance (Video)",
    x = "Diver (D) vs Rangerbot (R) per transect")+
  theme(
    axis.text.x = element_text(angle = 30)
  )

## adding in Clipboard Written surveys
clipboard <- read_csv("Data/Pclipboard_entries.csv") |> 
  rename(TrophicLevel = TrophicGroup)

clipboard$TotalDuration_min <- 15

clipboard <- clipboard |> 
  left_join(metadata_fish |> 
              select(Family, ScientificName), 
            by = join_by(ScientificName == ScientificName), relationship = "many-to-many", na_matches = "never")
              
clipboard_count <- clipboard |> 
  group_by(SurveyDate, SurveyLocation, Observer, D_DC_R, Transect, TotalDuration_min, ScientificName, Family, TrophicLevel, CommonName) |>
  summarize(
    TotalSppCount = sum(SppTotal, na.rm = TRUE),
    .groups = "drop"
  ) 

video_clipboard <- bind_rows(video_count, clipboard_count)

trophic <- video_clipboard |>  # Not by species
  group_by(SurveyDate, SurveyLocation, Observer, D_DC_R, Transect, TotalDuration_min, TrophicLevel) |>
  summarise(
    TotalTrophicCount = sum(TotalSppCount, na.rm = TRUE),
    .groups = "drop") |> 
  drop_na()

## Graph comparing C, D, R fish abundance
trophic_comparisons <- function(sites, dates) {
  ggplot(trophic |> filter(SurveyLocation %in% {{sites}}, SurveyDate %in% {{dates}} ), aes(x = interaction(D_DC_R, Transect), y = TotalTrophicCount, fill = TrophicLevel))+ #change fill depending on goal
    geom_bar( stat = "identity",position = "stack") +
    facet_wrap(~ SurveyLocation + SurveyDate, scales = "free_x",
               ncol = 5,
               labeller = labeller(
                 SurveyLocation = c(
                   BIT = "Buck Island",
                   BPT = "Black Point",
                   FLC = "Flat Cay",
                   CKR = "Coculus Rock")))+
    labs(
      y = "Fish Abundance",
      x = "Diver Clipboard (C), Diver Video (D), Rangerbot Video (R) per Transect"
    ) + 
    scale_y_continuous(expand = c(0,0))+
    theme(
      axis.text.x = element_text(angle = 30)
    )
}

##all
trophic_comparisons(unique(trophic$SurveyLocation), unique(trophic$SurveyDate))
ggsave("Outputs/Figures/All_comparisons.png", plot = last_plot(), dpi = 300) 

## ones with C, D, and R comparisons
trophic_comparisons(c("BIT", "BPT", "FLC"), c("2026-05-13", "2026-08-25", "2026-08-17"))
ggsave("Outputs/Figures/full_C_D_R_comparisons.png", plot = last_plot(), dpi = 300) 

## BPT with DQ = quick 3 min diver transect
trophic_comparisons(c("BPT"), c( "2026-08-17"))
ggsave("Outputs/Figures/BPT_8_17.png", plot = last_plot(), dpi = 300) 

trophic_comparisons(c("BIT"), c( "2026-05-13"))



