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

#Get (pooled) collection counts per stage
collection_counts <- fossils %>%
                     filter(identified_rank == "species") %>%
                     distinct(collection_no_pooled, stage_bin,
                              .keep_all = T) %>%
                     count(stage_bin)

#Identify smallest number of collections
to_sample <- min(collection_counts$n)

#Create data frame to store sampled richness values, starting with stages
subsampled_richness <- data.frame(stages)

#Sample that number of collections for each stage, and count species
for (i in 1:100) {
  stage_filter <- fossils %>% filter(identified_rank == "species") %>%
                  filter(stage_bin == "Asselian")
  unique_collections <- unique(stage_filter$collection_no_pooled)
  coll_sample <- sample(unique_collections, size = to_sample,
                        replace = FALSE)
  sampled_collections <- filter(stage_filter,
                                collection_no_pooled %in% coll_sample)
  species_count <- length(unique(sampled_collections$accepted_name))
  subsampled_richness[i,2] <- species_count
}
