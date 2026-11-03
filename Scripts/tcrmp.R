## TCRMP Fish Survey Data Visualization 

library(tidyverse)
library(readxl)
library(lubridate)


tcrmps <- read_csv("/Users/savannasaunders/Desktop/Thesis/TCRMP Fish Surveys/Fish.csv")

glimpse(tcrmp)

tcrmp$Year <- as_date(tcrmp$SampleYear)

tcrmp$Year<- y(tcrmp$SampleYear)


tcrmp_clean <- tcrmps %>% 
  select(Location, SampleYear, ScientificName, TrophicGroup, SppTotal, `Dive Type`) %>% 
  filter(SampleYear %in% c(2022, 2023, 2024, 2025),
    Location %in% c("Brewers Bay", "Flat Cay", "Black Point"), TrophicGroup == "herb"
  )

ggplot(tcrmp_clean, aes(x = `Dive Type`, y = SppTotal)) +
  geom_point()

ggplot(tcrmp_clean, aes(x = `Dive Type`, y = SppTotal, fill = ScientificName)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ SampleYear)

ggplot(tcrmp_clean, aes(x = `Dive Type`, y = SppTotal, fill = ScientificName)) +
  geom_boxplot() +
  facet_wrap(~ SampleYear)
