
# Removing Time & Combine C, D, R > Abundance -----------------------------------------

abundance_df <- function(clipboard_df, video_df, ...) {
  clipboards <- clipboard |> ## Gets rid of size bins and sums total count based on species & transect
    group_by(SurveyDate, SurveyLocation, Observer, C_D_R, Transect, Visability, TotalDuration_min, ScientificName, Family, TrophicLevel, CommonName) |>
    summarize(
      TotalSppCount = sum(SppTotal, na.rm = TRUE),
      .groups = "drop") |> 
    filter(C_D_R != "CQ") # Removes clipboard entries from 3 min surveys (different method)
  
  videos <- video|> ## Groups each species entry per transect and sums the number of present fish per species -> behavior etc. is dropped
    group_by(SurveyDate, SurveyLocation, Observer, C_D_R, Transect, Visability, TotalDuration_min, ScientificName, Family, TrophicLevel, CommonName) |>
    summarise(
      TotalSppCount = sum(Count, na.rm = TRUE),
      .groups = "drop") |> 
    filter(C_D_R != "DC" & C_D_R != "DQ") # Removes video data from Diver 3 min surveys and video on clipboard (different methods)
  
  # combined the clipboard and video count data
  # should not have any NAs in any columns except for Behavior, 
  all_data <- bind_rows(videos, clipboards) 
  
  # creation of unique identifier (TransectID, eg. 1_BPT, 5_FLC) 
    # Every new survey location restarts at 1
    # Numbering follows chronological date order and then numerical transect order - increase for every new date or new transect #
  all_data <- all_data |>
    group_by(SurveyLocation) |>
    mutate(
      TransectID = paste0(
        dense_rank(
          paste(SurveyDate, Transect, sep = "_")
        ),
        "_", SurveyLocation
      )
    ) |>
    ungroup()
  
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
      by = c("SurveyLocation" = "Code")) |> 
  ungroup()
} 

#check
# abud <- abundance_df(clipboard, video, TransectID, SurveyLocation, C_D_R)
# abud_t <- abundance_df(clipboard, video, TransectID, SurveyLocation, C_D_R, TrophicLevel)


# Graphs ------------------------------------------------------------------


abund_graphs_site_loop <- function(data, yval, numrow, RichnessType, AbundType) {
  for(site in unique({{data}}$Location)){
    abundance_site <- {{data}} |> 
      filter(Location == site)
    
    by_site <- ggplot(abundance_site, aes(C_D_R, {{yval}}, fill = Visability)) + 
      facet_wrap(~TransectID, nrow = {{numrow}}, labeller = as_labeller(function(repNum) {
        paste("Replication", sub("_.*", "", repNum))})) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Richness),
                position = position_stack(vjust = 0.5),
                color = "black",
                size = 4)+
      scale_y_continuous(breaks = scales::breaks_width(50))+
      scale_fill_manual(
        values = c(
          "Low" = "#CC79A7",
          "Moderate" = "#F0E442",
          "High" = "#009E73"))+
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


# Relative Abundance ------------------------------------------------------
##only does ones with full C, D, R comparison match
relative_abund <- function(decFactor, ylab) {
  rel_abundance <- abundance_df(clipboard, video, TransectID, SurveyLocation, SurveyDate, C_D_R, Visability, TotalDuration_min, CommonName, ScientificName, Family, TrophicLevel) |> 
    group_by(TransectID, SurveyLocation, SurveyDate, C_D_R, Visability, {{decFactor}}) |>
    summarise(
      total_abundance = sum(total_abundance, na.rm = TRUE),
      .groups = "drop") |> 
    group_by(TransectID, SurveyLocation, SurveyDate, C_D_R, Visability) %>%
    mutate(
      relative_abundance = 
        (total_abundance / sum(total_abundance)) * 100) 
  
  rel_comp_abundance <- rel_abundance |> 
    group_by(TransectID) |> 
    filter(all(c("C", "D", "R") %in% C_D_R)) |> 
    ungroup()
  
  summary_relativeabundance <- rel_comp_abundance |> 
    group_by(C_D_R) |> 
    summarise(
      mean_abund = mean(relative_abundance),
      SEM_abund = sd(relative_abundance) / sqrt(length(relative_abundance)),
      CI_low = mean_abund - 1.96*SEM_abund,
      CI_high = mean_abund + 1.96*SEM_abund)
  
  rel_graph <- ggplot(summary_relativeabundance, aes(x = C_D_R, y = mean_abund, fill = C_D_R)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = mean_abund - SEM_abund,
                      ymax = mean_abund + SEM_abund), width = 0.2,
                  position = position_dodge(.9))+
    scale_x_discrete(
      labels = c(
        "C" = "Clipboard",
        "D" = "Diver",
        "R" = "Rangerbot"))+
    labs(
      x = "Survey Method",
      y = paste("Relative Fish", {{ylab}}, "Abundance (%)"))+
    theme_minimal()+
    theme(
      axis.text.x = element_text(size = 12),
      strip.text = element_text( size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 15),
      plot.caption = element_text(size = 10),
      plot.title = element_text(size = 20)
    )
  print(rel_graph)
}
