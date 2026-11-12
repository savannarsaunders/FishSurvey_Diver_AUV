#Total Abundance of fish


# Libraries & Functions ---------------------------------------------------
library(MASS)
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(performance)
library(dplyr)
library(lme4)

source("Scripts/cleaning_functions.R")
source("Scripts/Functions.R")

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::group_by)

# Data Import -------------------------------------------------------------

fishmeta <- metafish("Data/Fish Count Data_11_12_26.xlsx")
video <- VideoDataCleaning_time("Data/Fish Count Data_11_12_26.xlsx", fishmeta)
clipboard <- ClipboardDataCleaning("Data/Fish Count Data_11_12_26.xlsx", video)
SiteMetadata <- read_xlsx("Data/Fish Count Data_11_12_26.xlsx", sheet = "SiteMetadata") |> 
  filter(Code != "GKT")



# Removing Time & Combine C, D, R > Abundance -----------------------------------------

species_abundance <- abundance_df(clipboard, video, TransectID, SurveyLocation, C_D_R, TotalDuration_min) |> 
  mutate(
    abund_per_min = total_abundance/TotalDuration_min
  )

## CSV for GIS richness map
write.csv(species_abundance, "Outputs/DataFrames/species_abundance.csv", row.names = FALSE)
write.csv(SiteMetadata, "Outputs/DataFrames/SiteMetadata.csv", row.names = FALSE)

# Site (factor), transect ID (unique identifier), CDR (factor), aggregate to species richness (number), total abundance (number)


# abundace by transect ID
# abundance_transectID_plot <- function(data) {
#   ggplot({{data}}, aes(TransectID, total_abundance)) + 
#     facet_wrap(~C_D_R, ncol = 1) +
#     geom_bar(stat = "identity") +
#     theme_minimal()+
#     theme(
#       axis.text.x = element_text(angle = 30))
# }

# abundance_transectID_plot(species_abundance)

# Selecting only transects that have a full C_D_R comparison
comp_abundance <- species_abundance |> 
  group_by(TransectID) |> 
  filter(n() == 3) |> 
  ungroup()


# abundance_transectID_plot(comp_abundance)

# Graphs ------------------------------------------------------------------
abundance_method_plot <- function(data) {
  ggplot({{data}}, aes(C_D_R, total_abundance, fill = C_D_R)) + 
    facet_wrap(~TransectID, ncol = 4) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Richness),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 4
    )+
    theme_minimal()+
    theme(
      axis.text.x = element_text(angle = 30))
}

abundance_method_plot(comp_abundance)

##Loop to make an abundance graph for each site separated by replication including species richness per each observation - only comparisons with all C_D_R
abund_graphs_site_loop(comp_abundance, total_abundance, 2, "trophic richness", "totalabundance")


# Time --------------------------------------------------------------------

abund_graphs_site_loop(comp_abundance, abund_per_min, 2, "trophic richness", "totalabundance")

# Regression ------------------------------------------------------

# Check mean vs variance (should be similar for Poisson)
mean(comp_abundance$total_abundance)
var(comp_abundance$total_abundance)
var(comp_abundance$total_abundance) / mean(comp_abundance$total_abundance) # Should be close to 1

# Visualize the distribution
ggplot(comp_abundance, aes(x = total_abundance)) +
  geom_histogram(bins = 8, fill = "lightblue", color = "black") +
  labs(title = "Distribution of fish",
       x = "Number of fish", y = "Frequency")

pois_simple <- glm(total_abundance ~ C_D_R, data = comp_abundance, family = "poisson")

#check assumptions & overdispursion
check_model(pois_simple)
check_overdispersion(pois_simple) 

summary(pois_simple)

# Over dispursed data and right skewed

## Negative Binomial Regression Across Methods only
neg_bino <- glm.nb(total_abundance ~ C_D_R, data = comp_abundance)

check_model(neg_bino)
check_overdispersion(neg_bino)

summary(neg_bino)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino) - logLik(pois_simple)), df = 1, lower.tail = FALSE)
# 'log Lik.' 0 (df=4) 
# since p-value is 0 it means that the negative binomial is more appropriate for this data

# Including site as cofactor as effect Negative Binomial

ggplot(comp_abundance, aes(x = C_D_R, y = total_abundance)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black") +
  theme_bw()

ggplot(comp_abundance, aes(x = SurveyLocation, y = total_abundance)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  geom_violin(aes(fill = SurveyLocation), alpha = 0.2) +
  theme_bw()

neg_bino_site <- glm.nb(total_abundance ~ C_D_R + SurveyLocation, data = comp_abundance) # Used + instead of * because my main predictor does not change depending of survey location

check_model(neg_bino_site)
check_overdispersion(neg_bino_site)

summary(neg_bino_site)

##Check to see if glm neg binomial is right fit 
pois_site <- glm(total_abundance ~ C_D_R * SurveyLocation, data = comp_abundance, family = "poisson")
summary(pois_site)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_site) - logLik(pois_site)), df = 1, lower.tail = FALSE)
# 'log Lik.' 7.783978e-217 (df=8)
# since p-value is close to 0 it means that the negative binomial is more appropriate for this data

# Video Comparison Only ---------------------------------------------------

video_comp_abundance <- comp_abundance |> 
  filter(C_D_R != "C")
  
# Check mean vs variance
mean(video_comp_abundance$total_abundance)
var(video_comp_abundance$total_abundance)
var(video_comp_abundance$total_abundance) / mean(video_comp_abundance$total_abundance)

# Visualize the distribution
ggplot(video_comp_abundance, aes(x = total_abundance)) +
  geom_histogram(bins = 8, fill = "lightblue", color = "black") +
  labs(title = "Distribution of fish",
       x = "Number of fish", y = "Frequency")

# Overdispursed 

# only method as predictor
neg_bino_v <- glm.nb(total_abundance ~ C_D_R, data = video_comp_abundance)

summary(neg_bino_v)

check_model(neg_bino_v)
check_overdispersion(neg_bino_v)

# Including site as cofactor Negative Binomial

# Visualizing site differences 
ggplot(video_comp_abundance, aes(x = SurveyLocation, y = total_abundance)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  geom_violin(aes(fill = SurveyLocation), alpha = 0.2) +
  theme_bw()

# neg binomial
neg_bino_site_v <- glm.nb(total_abundance ~ C_D_R + SurveyLocation, data = video_comp_abundance) # Used + instead of * because my main predictor does not change depending of survey location

check_model(neg_bino_site_v)
check_overdispersion(neg_bino_site_v)

summary(neg_bino_site_v)

##Check to see if glm neg binomial is right fit 
pois_site_v <- glm(total_abundance ~ C_D_R * SurveyLocation, data = video_comp_abundance, family = "poisson")
summary(pois_site)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_site_v) - logLik(pois_site_v)), df = 1, lower.tail = FALSE)
# 'log Lik.' 2.668443e-123 (df=5) 
# since p-value is close to 0 it means that the negative binomial is more appropiate for this data



# Mixed model C, D, R -----------------------------------------------------

abund_mixed <- glmer(
  total_abundance ~ C_D_R + (C_D_R | SurveyLocation), 
  comp_abundance, 
  family = poisson(link = "log")
)
check_model(abund_mixed)
check_overdispersion(abund_mixed)
summary(abund_mixed)
