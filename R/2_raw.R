#Bethany Allen   19th September 2024
#Code to calculate raw diversity through time

#setwd("#####")

#Load packages
library(tidyverse)
library(palaeoverse)
library(deeptime)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")
glimpse(fossils)

#Count unique species per stage bin
species_counts <- fossils %>%
                  distinct(accepted_name, stage_bin, .keep_all = T) %>%
                  count(stage_bin)

#Count unique genera per stage bin
genus_counts <- fossils %>%
                distinct(genus, stage_bin, .keep_all = T) %>%
                count(stage_bin)

#Bind
counts <- data.frame(stage = species_counts$stage_bin,
                     genera = genus_counts$n,
                     species = species_counts$n)
counts <- arrange(counts, factor(stage, levels = stages))

#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
counts <- left_join(counts, midpoint_data, by = join_by(stage == interval_name))

#Pivot data
counts <- pivot_longer(counts, cols = c(genera, species), names_to = "level",
                     values_to = "raw")

#Save
write_csv(counts, "data/counts.csv")

#Plot species
ggplot(counts, aes(x = mid_ma, y = raw_species)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Raw species count") +
  coord_geo(xlim = c(max(counts$max_ma), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Plot genera
ggplot(counts, aes(x = mid_ma, y = raw_genera)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Raw genus count") +
  coord_geo(xlim = c(max(counts$max_ma), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())
