#Bethany Allen   19th September 2024
#Code to calculate raw diversity through time

#setwd("#####")

#Load packages
library(tidyverse)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")
glimpse(fossils)

#Count unique species per stage bin
species_counts <- fossils %>%
                  filter(identified_rank == "species") %>%
                  distinct(accepted_name, stage_bin, .keep_all = T) %>%
                  count(stage_bin)

#Count formations per stage bin
formation_counts <- fossils %>%
                    filter(!is.na(formation)) %>%
                    distinct(formation, stage_bin, .keep_all = T) %>%
                    count(stage_bin)

#Bind
counts <- data.frame(stage = species_counts$stage_bin,
                     formations = formation_counts$n,
                     raw_species = species_counts$n)
counts <- arrange(counts, factor(stage, levels = stages))

#Save
write_csv(counts, "data/raw_counts.csv")
