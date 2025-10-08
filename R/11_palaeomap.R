#Bethany Allen   12th Sept 2025
#Code to plot a palaeomap of localities

#setwd("#####")

#Load packages
library(palaeoverse)
library(tidyverse)
library(viridis)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

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

#Add column to hold geographic realm information
fossils$realm <- NA
#Allocate points to geographic realms
for (i in (1:nrow(fossils))) {
  if (fossils$p_lng[i] < -100 && fossils$p_lat[i] < -50) {
    fossils$realm[i] <- "A"
  } else if (-100 < fossils$p_lng[i] &
             fossils$p_lng[i] < -15 &
             -60 < fossils$p_lat[i] &
             fossils$p_lat[i] < 60) {
    fossils$realm[i] <- "B"
  } else if (fossils$p_lng[i] > 140 &
             fossils$p_lat[i] < -25) {
    fossils$realm[i] <- "C"
  } else if (60 < fossils$p_lng[i] &
             fossils$p_lng[i] < 120 &
             -10 < fossils$p_lat[i] &
             fossils$p_lat[i] < 30) {
    fossils$realm[i] <- "D"
  } else if (120 < fossils$p_lng[i] &
             20 < fossils$p_lat[i] &
             fossils$p_lat[i] < 60) {
    fossils$realm[i] <- "D"
  } else if (0 < fossils$p_lng[i] &
             fossils$p_lng[i] < 150 &
             -75 < fossils$p_lat[i] &
             fossils$p_lat[i] < -25) {
    fossils$realm[i] <- "E"
    } else if (55 < fossils$p_lng[i] &
               fossils$p_lng[i] < 75 &
               -25 < fossils$p_lat[i] &
               fossils$p_lat[i] < -20) {
      fossils$realm[i] <- "E"
    } else if (0 < fossils$p_lng[i] &
         fossils$p_lng[i] < 75 &
         -25 < fossils$p_lat[i] &
         fossils$p_lat[i] < 30) {
      fossils$realm[i] <- "F"
    } else if (20 < fossils$p_lng[i] &
               fossils$p_lng[i] < 80 &
               25 < fossils$p_lat[i] &
               fossils$p_lat[i] < 50) {
      fossils$realm[i] <- "F"
    } else if (50 < fossils$p_lng[i] &
               40 < fossils$p_lat[i]) {
      fossils$realm[i] <- "F"
    } else {fossils$realm[i] <- "G"}
}

#Plot points
ggplot(fossils, aes(x = p_lng, y = p_lat, group = realm, col = realm)) +
  geom_point() +
  scale_colour_viridis(discrete = T)+
  xlim(-180, 180) +
  ylim(-90, 90) +
  theme_classic()

