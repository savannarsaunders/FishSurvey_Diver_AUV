# Trophic Group Abundance

# Libraries & Functions ---------------------------------------------------

library(MASS)
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(performance)
library(dplyr)

source("Scripts/cleaning_functions.R")
source("Scripts/Functions.R")

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::group_by)



# Data Import -------------------------------------------------------------

fishmeta <- metafish("Data/Fish Count Data_11_2.xlsx")
video <- VideoDataCleaning_time("Data/Fish Count Data_11_2.xlsx", fishmeta)
clipboard <- ClipboardDataCleaning("Data/Fish Count Data_11_2.xlsx", video)
SiteMetadata <- read_xlsx("Data/Fish Count Data_11_2.xlsx", sheet = "SiteMetadata")



# Removing Time & Combine C, D, R > Tophic Abundance -----------------------------------------

trophic_abundance <- abundance_df(clipboard, video, TransectID, SurveyLocation, C_D_R, TrophicLevel, TotalDuration_min) |> 
  mutate(
    abund_per_min = abund_per_min/TotalDuration_min
  )

# Site (factor), transect ID (unique identifier), CDR (factor), aggregate to trophic group richness (number), total abundance per trophic group (number)

# Selecting only transects that have a full C_D_R comparison
comp_trophic_abundance <- trophic_abundance |> 
  group_by(TransectID) |> 
  filter(all(c("C", "D", "R") %in% C_D_R)) |> 
  ungroup()

# Graphs ------------------------------------------------------------------
trophic_abund_graphs_site_loop(comp_trophic_abundance, abund_per_min, 2, "trophic richness", "trophic abundance")

# Regression ------------------------------------------------------

# Check mean vs variance (should be similar for Poisson)
mean(comp_trophic_abundance$abund_per_min)
var(comp_trophic_abundance$abund_per_min)
var(comp_trophic_abundance$abund_per_min) / mean(comp_trophic_abundance$abund_per_min) # Should be close to 1

# Visualize the distribution
ggplot(comp_trophic_abundance, aes(x = abund_per_min)) +
  geom_histogram(bins = 8, fill = "darkblue", color = "black") +
  labs(title = "Distribution of fish",
       x = "Number of fish", y = "Frequency")

pois_simple_t <- glm(abund_per_min ~ C_D_R*TrophicLevel, data = comp_trophic_abundance, family = "poisson")

#check assumptions & overdispursion
check_model(pois_simple_t)
check_overdispersion(pois_simple_t) 

summary(pois_simple_t)

# Over dispursed data and right skewed

## Negative Binomial Regression Across Methods only
neg_bino_t <- glm.nb(abund_per_min ~ C_D_R*TrophicLevel, data = comp_trophic_abundance)

check_model(neg_bino_t)
check_overdispersion(neg_bino_t)

summary(neg_bino_t)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_t) - logLik(pois_simple_t)), df = 1, lower.tail = FALSE)
# 'log Lik.' 0 (df=16) 
# since p-value is 0 it means that the negative binomial is more appropriate for this data

# Including site as cofactor as effect Negative Binomial

ggplot(comp_trophic_abundance, aes(x = C_D_R, y = abund_per_min)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black") +
  theme_bw()

ggplot(comp_trophic_abundance, aes(x = SurveyLocation, y = abund_per_min)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  geom_violin(aes(fill = SurveyLocation), alpha = 0.2) +
  theme_bw()

neg_bino_site <- glm.nb(abund_per_min ~ C_D_R + SurveyLocation, data = comp_trophic_abundance) # Used + instead of * because my main predictor does not change depending of survey location

check_model(neg_bino_site)
check_overdispersion(neg_bino_site)

summary(neg_bino_site)

##Check to see if glm neg binomial is right fit 
pois_site <- glm(abund_per_min ~ C_D_R * SurveyLocation, data = comp_trophic_abundance, family = "poisson")
summary(pois_site)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_site) - logLik(pois_site)), df = 1, lower.tail = FALSE)
# 'log Lik.' 1.70112e-173 (df=10) 
# since p-value is close to 0 it means that the negative binomial is more appropriate for this data

# Video Comparison Only ---------------------------------------------------

video_comp_trophic_abundance <- comp_trophic_abundance |> 
  filter(C_D_R != "C")

# Check mean vs variance
mean(video_comp_trophic_abundance$abund_per_min)
var(video_comp_trophic_abundance$abund_per_min)
var(video_comp_trophic_abundance$abund_per_min) / mean(video_comp_trophic_abundance$abund_per_min)

# Visualize the distribution
ggplot(video_comp_trophic_abundance, aes(x = abund_per_min)) +
  geom_histogram(bins = 8, fill = "lightblue", color = "black") +
  labs(title = "Distribution of fish",
       x = "Number of fish", y = "Frequency")

# Overdispursed 

# only method as predictor
neg_bino_v <- glm.nb(abund_per_min ~ C_D_R, data = video_comp_trophic_abundance)

summary(neg_bino_v)

check_model(neg_bino_v)
check_overdispersion(neg_bino_v)

# Including site as cofactor Negative Binomial

# Visualizing site differences 
ggplot(video_comp_trophic_abundance, aes(x = SurveyLocation, y = abund_per_min)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  geom_violin(aes(fill = SurveyLocation), alpha = 0.2) +
  theme_bw()

# neg binomial
neg_bino_site_v <- glm.nb(abund_per_min ~ C_D_R + SurveyLocation, data = video_comp_trophic_abundance) # Used + instead of * because my main predictor does not change depending of survey location

check_model(neg_bino_site_v)
check_overdispersion(neg_bino_site_v)

summary(neg_bino_site_v)

##Check to see if glm neg binomial is right fit 
pois_site_v <- glm(abund_per_min ~ C_D_R * SurveyLocation, data = video_comp_trophic_abundance, family = "poisson")
summary(pois_site)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_site_v) - logLik(pois_site_v)), df = 1, lower.tail = FALSE)
# 'log Lik.' 2.668443e-123 (df=5) 
# since p-value is close to 0 it means that the negative binomial is more appropiate for this data




# With TIMEEE -------------------------------------------------------------

# Regression ------------------------------------------------------

# Check mean vs variance (should be similar for Poisson)
mean(comp_trophic_abundance$abund_per_min)
var(comp_trophic_abundance$abund_per_min)
var(comp_trophic_abundance$abund_per_min) / mean(comp_trophic_abundance$abund_per_min) # Should be close to 1

# Visualize the distribution
ggplot(comp_trophic_abundance, aes(x = abund_per_min)) +
  geom_histogram(bins = 8, fill = "darkblue", color = "black") +
  labs(title = "Distribution of fish",
       x = "Number of fish", y = "Frequency")

pois_simple_t <- glm(abund_per_min ~ C_D_R*TrophicLevel, data = comp_trophic_abundance, family = "poisson")

#check assumptions & overdispursion
check_model(pois_simple_t)
check_overdispersion(pois_simple_t) 

summary(pois_simple_t)

# Over dispursed data and right skewed

## Negative Binomial Regression Across Methods only
neg_bino_t <- glm.nb(abund_per_min ~ C_D_R*TrophicLevel, data = comp_trophic_abundance)

check_model(neg_bino_t)
check_overdispersion(neg_bino_t)

summary(neg_bino_t)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_t) - logLik(pois_simple_t)), df = 1, lower.tail = FALSE)
# 'log Lik.' 0 (df=16) 
# since p-value is 0 it means that the negative binomial is more appropriate for this data

# Including site as cofactor as effect Negative Binomial

ggplot(comp_trophic_abundance, aes(x = C_D_R, y = abund_per_min)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black") +
  theme_bw()

ggplot(comp_trophic_abundance, aes(x = SurveyLocation, y = abund_per_min)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  geom_violin(aes(fill = SurveyLocation), alpha = 0.2) +
  theme_bw()

neg_bino_site <- glm.nb(abund_per_min ~ C_D_R + SurveyLocation, data = comp_trophic_abundance) # Used + instead of * because my main predictor does not change depending of survey location

check_model(neg_bino_site)
check_overdispersion(neg_bino_site)

summary(neg_bino_site)

##Check to see if glm neg binomial is right fit 
pois_site <- glm(abund_per_min ~ C_D_R * SurveyLocation, data = comp_trophic_abundance, family = "poisson")
summary(pois_site)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_site) - logLik(pois_site)), df = 1, lower.tail = FALSE)
# 'log Lik.' 1.70112e-173 (df=10) 
# since p-value is close to 0 it means that the negative binomial is more appropriate for this data

# Video Comparison Only ---------------------------------------------------

video_comp_trophic_abundance <- comp_trophic_abundance |> 
  filter(C_D_R != "C")

# Check mean vs variance
mean(video_comp_trophic_abundance$abund_per_min)
var(video_comp_trophic_abundance$abund_per_min)
var(video_comp_trophic_abundance$abund_per_min) / mean(video_comp_trophic_abundance$abund_per_min)

# Visualize the distribution
ggplot(video_comp_trophic_abundance, aes(x = abund_per_min)) +
  geom_histogram(bins = 8, fill = "lightblue", color = "black") +
  labs(title = "Distribution of fish",
       x = "Number of fish", y = "Frequency")

# Overdispursed 

# only method as predictor
neg_bino_v <- glm.nb(abund_per_min ~ C_D_R, data = video_comp_trophic_abundance)

summary(neg_bino_v)

check_model(neg_bino_v)
check_overdispersion(neg_bino_v)

# Including site as cofactor Negative Binomial

# Visualizing site differences 
ggplot(video_comp_trophic_abundance, aes(x = SurveyLocation, y = abund_per_min)) +
  geom_jitter(aes(color = SurveyLocation), width = 0.2, alpha = 0.6) +
  geom_violin(aes(fill = SurveyLocation), alpha = 0.2) +
  theme_bw()

# neg binomial
neg_bino_site_v <- glm.nb(abund_per_min ~ C_D_R + SurveyLocation, data = video_comp_trophic_abundance) # Used + instead of * because my main predictor does not change depending of survey location

check_model(neg_bino_site_v)
check_overdispersion(neg_bino_site_v)

summary(neg_bino_site_v)

##Check to see if glm neg binomial is right fit 
pois_site_v <- glm(abund_per_min ~ C_D_R * SurveyLocation, data = video_comp_trophic_abundance, family = "poisson")
summary(pois_site)

#seeing which fits the data better
pchisq(2 * (logLik(neg_bino_site_v) - logLik(pois_site_v)), df = 1, lower.tail = FALSE)
# 'log Lik.' 2.668443e-123 (df=5) 
# since p-value is close to 0 it means that the negative binomial is more appropiate for this data



