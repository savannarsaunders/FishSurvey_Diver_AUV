
# Removing Time & Combine C, D, R > Abundance -----------------------------------------

abundance_df <- function(clipboard_df, video_df, ...) {
  clipboard <- {{clipboard_df}} |> ## Gets rid of size bins and sums total count based on species & transect
    group_by(SurveyDate, SurveyLocation, Observer, C_D_R, Transect, TotalDuration_min, ScientificName, Family, TrophicLevel, CommonName) |>
    summarize(
      TotalSppCount = sum(SppTotal, na.rm = TRUE),
      .groups = "drop") |> 
    filter(C_D_R != "CQ") # Removes clipbord entries from 3 min surveys (different method)
  
  video <- {{video_df}}|> ## Groups each species entry per transect and sums the number of present fish per species -> behavior etc. is dropped
    group_by(SurveyDate, SurveyLocation, Observer, C_D_R, Transect, TotalDuration_min, ScientificName, Family, TrophicLevel, CommonName) |>
    summarise(
      TotalSppCount = sum(Count, na.rm = TRUE),
      .groups = "drop") |> 
    filter(C_D_R != "DC" & C_D_R != "DQ") # Removes video data from Diver 3 min surveys and video on clipboard (different methods)
  
  # combined the clipboard and video count data
  # should not have any NAs in any columns except for Behavior, 
  all_data <- bind_rows(video, clipboard) 
  
  # creation od unique identifyer
  all_data <- all_data |> 
    group_by(SurveyLocation) |>
    mutate(
      TransectID = paste0(
        dense_rank(interaction(SurveyDate, Transect, drop = TRUE)),"_",SurveyLocation)) |>
    ungroup() |>
    arrange(SurveyLocation, parse_number(TransectID))
  
  ## only gives this data frame
  # Site (factor), transect ID (unique identifier), CDR (factor), aggregate to species/trophic richness (number), total abundance per transect id/trophic level (number)
  abundance <- all_data |> 
    group_by(...) |>
    summarize(
      Richness = n_distinct(ScientificName), 
      #if for total abundance then it counts species richness within each transect ID
      #if for trophic level abundance then it counts the species richness within each trophic group per transect ID
      total_abundance = sum(TotalSppCount))  |> 
    arrange(parse_number(TransectID)) |> 
    left_join(
      SiteMetadata |> 
        select(Code, Location), 
      by = c("SurveyLocation" = "Code"))
} 

#check
# abud <- abundance_df(clipboard, video, TransectID, SurveyLocation, C_D_R)
# abud_t <- abundance_df(clipboard, video, TransectID, SurveyLocation, C_D_R, TrophicLevel)


# Graphs ------------------------------------------------------------------


abund_graphs_site_loop <- function(data, yval, numrow, RichnessType, AbundType) {
  for(site in unique({{data}}$Location)){
    abundance_site <- {{data}} |> 
      filter(Location == site)
    
    by_site <- ggplot(abundance_site, aes(C_D_R, {{yval}}, fill = C_D_R)) + 
      facet_wrap(~TransectID, nrow = {{numrow}}, labeller = as_labeller(function(repNum) {
        paste("Replication", sub("_.*", "", repNum))})) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Richness),
                position = position_stack(vjust = 0.5),
                color = "black",
                size = 4)+
      scale_y_continuous(breaks = scales::breaks_width(50))+
      scale_fill_discrete(
        labels = c(
          "C" = "Clipboard",
          "D" = "Diver Video",
          "R" = "Rangerbot Video"), name  = "Survey Method")+
      labs(
        x = "Survey Method",
        y = "Total Abundance",
        title = paste(site),
        caption = paste("Total abundance separated by replication at", {{site}}, ".", "Black values above bars represent the", {{RichnessType}}, "observed."))+
      theme_minimal()+
      theme(
        axis.text.x = element_text(size = 12),
        strip.text = element_text( size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 20)
      )
      
    print(by_site)
    
    site_ac <- unique(abundance_site$SurveyLocation)
    
    ggsave(
      paste0("Outputs/Figures/Updated/SppAbundance/", site_ac, "_", {{AbundType}}, ".png"),plot = last_plot(),
      height = 8, width = 11)
  }}

# Check
# abund_graphs_site_loop(comp_abundance, 2, "species richness", "totalabundance")

trophic_abund_graphs_site_loop <- function(data, yval, numrow, RichnessType, AbundType) {
  for(site in unique({{data}}$Location)){
    abundance_site <- {{data}} |> 
      filter(Location == site)
    
    by_site <- ggplot(abundance_site, aes(C_D_R, {{yval}}, fill = TrophicLevel)) + 
      facet_wrap(~TransectID, nrow = {{numrow}}, labeller = as_labeller(function(repNum) {
        paste("Replication", sub("_.*", "", repNum))}))+
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = Richness),
                position = position_dodge(width = 0.9),
                vjust = 0,
                color = "black",
                size = 4)+
      scale_fill_discrete(
        labels = c(
          "herb" = "Herbivore",
          "inv" = "Invertivores",
          "pisc" = "Piscivores",
          "plank" = "Planktivore"), name  = "Trophic Level")+
      scale_x_discrete(
        labels = c(
          "C" = "Clipboard",
          "D" = "Diver",
          "R" = "Rangerbot"))+
      scale_y_continuous(breaks = scales::breaks_width(50))+
      labs(
        x = "Survey Method",
        y = "Total Trophic Abundance",
        title = site,
        caption = paste("Total abundance per trophic level separated by replication at", {{site}}, ".", "Black values above bars represent the", {{RichnessType}}, "observed."))+
      theme_minimal()+
      theme(
        axis.text.x = element_text(size = 12),
        strip.text = element_text( size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 20)
      )
    
    print(by_site)
    
    site_ac <- unique(abundance_site$SurveyLocation)
    
    ggsave(
      paste0("Outputs/Figures/Updated/Trophic/", site_ac, "_", {{AbundType}}, ".png"),plot = last_plot(),
      height = 8, width = 11)
  }}

# Check
# trophic_abund_graphs_site_loop(comp_trophic_abundance, 2, "trophic richness", "trophic abundance")

