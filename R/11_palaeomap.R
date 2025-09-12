#Bethany Allen   12th Sept 2025
#Code to plot a palaeomap of localities

#setwd("#####")

#Load packages
library(palaeoverse)
library(tidyverse)

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")
glimpse(fossils)

#Create midpoint age key
time_key <- select(GTS2020, interval_name, mid_ma)

#Generate a column of midpoint ages
fossils <- left_join(fossils, time_key,
                  by = join_by("stage_bin" == "interval_name"))

#Palaeorotate points
fossils <- palaeorotate(occdf = as.data.frame(fossils),
                     age = "mid_ma",
                     model = "MERDITH2021",
                     uncertainty = FALSE,
                     round = 2)

#Remove points which cannot be rotated
fossils <- filter(fossils, !is.na(p_lat))

#Plot points
ggplot(fossils, aes(x = p_lng, y = p_lat, group = stage_bin, col = stage_bin)) +
  geom_point() +
  xlim(-180, 180) +
  ylim(-90, 90)
