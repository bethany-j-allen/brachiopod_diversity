#Bethany Allen   30th January 2026
#Code to add the DeepDive results to the overall results table

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
counts <- read_csv("data/counts.csv")

#Read in DeepDive results
DeepDive_genera <- read_csv("data/Empirical_predictions_genera.csv")
DeepDive_species <- read_csv("data/Empirical_predictions_species.csv")

#Summarise DeepDive results
DD_median_genera <- apply(DeepDive_genera, 2, median)
DD_max_genera <- apply(DeepDive_genera, 2, max)
DD_min_genera <- apply(DeepDive_genera, 2, min)
DD_median_species <- apply(DeepDive_species, 2, median)
DD_max_species <- apply(DeepDive_species, 2, max)
DD_min_species <- apply(DeepDive_species, 2, min)

#Create data frame for results
gen_summary <- data.frame(stage = rev(stages), level = "genera",
                          DD_max = DD_max_genera[2:14],
                          DD_median = DD_median_genera[2:14],
                          DD_min = DD_min_genera[2:14],
                          row.names = NULL)
sp_summary <- data.frame(stage = rev(stages), level = "species",
                         DD_max = DD_max_species[2:14],
                         DD_median = DD_median_species[2:14],
                         DD_min = DD_min_species[2:14],
                         row.names = NULL)
summary <- rbind(gen_summary, sp_summary)

#Add to counts table
counts <- left_join(counts, summary, by = c("stage", "level"))

#Update counts table file
write_csv(counts, "data/counts.csv")
