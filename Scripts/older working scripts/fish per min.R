## Fish per min

fish_per_min_comp <- function() {
  ggplot(trophic |> filter(SurveyLocation %in% {{sites}}, SurveyDate %in% {{dates}} ), aes(x = interaction(D_DC_R, Transect), y = TotalTrophicCount, fill = TrophicLevel))+
    ggplot(combined, aes(x = `D/R`, y = total_counts, fill = `D/R`)) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~SurveyLocation, labeller = labeller(SurveyLocation = c(
      BIT = "Buck Island, St. Thomas, USVI",
      BPT = "Black Point, St. Thomas, USVI")))+
    labs(
      x = "Diver vs. AUV",
      y = "Total Fish Count", 
      caption = str_wrap("Figure 1. Comparison of total fish counts recorded during one transect by a diver and AUV at Buck Island and Black Point, St. Thomas, USVI.", width = 100)) +
    scale_fill_manual(values = c(
      "D" = "#2c695c",
      "R" = "#f4e5b5"))+
    scale_x_discrete(labels = c(
      "D" = "Diver",
      "R" = "AUV")) +
    scale_y_continuous(breaks = seq(0, max(combined$total_counts), by = 50))+
    theme_minimal() +
    theme(legend.position = "none")+
    theme(
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      strip.text = element_text(size = 22),
      plot.caption = element_text(size = 15)
    )
}