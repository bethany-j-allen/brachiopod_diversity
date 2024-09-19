#Bethany Allen   19th September 2024
#Code to calculate rarefied diversity through time

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

#Get (pooled) collection counts per stage
collection_counts <- fossils %>%
                     filter(identified_rank == "species") %>%
                     distinct(collection_no_pooled, stage_bin,
                              .keep_all = T) %>%
                     count(stage_bin)

#Identify smallest number of collections
to_sample <- min(collection_counts$n)

#Create data frame to store sampled richness values, starting with stages
subsampled_richness <- data.frame()

#Sample that number of collections for each stage, and count species
for (j in 1:length(stages)) {
  stage_filter <- fossils %>% filter(identified_rank == "species") %>%
                    filter(stage_bin == stages[j])
  for (i in 1:100) {
    unique_collections <- unique(stage_filter$collection_no_pooled)
    coll_sample <- sample(unique_collections, size = to_sample,
                          replace = FALSE)
    sampled_collections <- filter(stage_filter,
                                  collection_no_pooled %in% coll_sample)
    species_count <- length(unique(sampled_collections$accepted_name))
    subsampled_richness[i,j] <- species_count
  }
}

#Label stages
colnames(subsampled_richness) <- stages

#Summarise data
medians <- apply(subsampled_richness, 2, median)
maxes <- apply(subsampled_richness, 2, max)
mins <- apply(subsampled_richness, 2, min)
summary <- data.frame(max = maxes, median = medians, min = mins)
