## Hurdle Model


# Notes -------------------------------------------------------------------
# Two Parts
# 1.  Models the probability of seeing any fish vs. zero fish (presence/absence via logistic regression).
# 2. Count Part (Truncated): Models the abundance (>0) using a zero-truncated distribution like truncated Negative Binomial (dist = "negbin") to handle overdispersion common in fish catches, or truncated Poisson (dist = "poisson").

# Libraries ---------------------------------------------------------------
library(pscl)
library(tidyverse)


# Questions ---------------------------------------------------------------

# Should i combine the clipboard surveys and the video surveys for the model?
  # I am just worried about the set up of the data frame in regards to the videos being 4 second intervals vs the clipboard is just present


# Data --------------------------------------------------------------------

PAclean_video_counts <- read_csv("Data/PAclean_video_counts.csv")
# Prelim ------------------------------------------------------------------

plot(table(PAclean_video_counts$Count))

#Sourcing a function
