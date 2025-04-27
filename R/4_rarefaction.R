#Bethany Allen   19th September 2024
#Code to calculate rarefied diversity through time

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

#Get (pooled) collection counts per stage
collection_counts <- fossils %>%
                     distinct(collection_no_pooled, stage_bin,
                              .keep_all = T) %>%
                     count(stage_bin)

#Identify smallest number of collections
to_sample <- min(collection_counts$n)

#Create data frame to store sampled richness values, starting with stages
gen_subsampled_richness <- data.frame()
sp_subsampled_richness <- data.frame()

#Sample that number of collections for each stage
for (j in 1:length(stages)) {
  stage_filter <- filter(fossils, stage_bin == stages[j])
  for (i in 1:100) {
    unique_collections <- unique(stage_filter$collection_no_pooled)
    coll_sample <- sample(unique_collections, size = to_sample,
                          replace = FALSE)
    sampled_collections <- filter(stage_filter,
                                  collection_no_pooled %in% coll_sample)
    #Count distinct genera
    genus_count <- length(unique(sampled_collections$genus))
    gen_subsampled_richness[i,j] <- genus_count
    #Count distinct species
    species_count <- length(unique(sampled_collections$accepted_name))
    sp_subsampled_richness[i,j] <- species_count
  }
}

#Label stages
colnames(gen_subsampled_richness) <- stages
colnames(sp_subsampled_richness) <- stages

#Summarise data
gen_medians <- apply(gen_subsampled_richness, 2, median)
sp_medians <- apply(sp_subsampled_richness, 2, median)
gen_maxes <- apply(gen_subsampled_richness, 2, max)
sp_maxes <- apply(sp_subsampled_richness, 2, max)
gen_mins <- apply(gen_subsampled_richness, 2, min)
sp_mins <- apply(sp_subsampled_richness, 2, min)
summary <- data.frame(stage = stages, rf_gen_max = gen_maxes,
                      rf_gen_median = gen_medians, rf_gen_min = gen_mins,
                      rf_sp_max = sp_maxes, rf_sp_median = sp_medians,
                      rf_sp_min = sp_mins)

#Save to counts table
counts <- read_csv("data/counts.csv")
counts <- left_join(counts, summary, by = join_by(stage))
write_csv(counts, "data/counts.csv")


#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
summary <- left_join(summary, midpoint_data, by = join_by(stage == interval_name))

#Plot genera
ggplot(summary, aes(x = mid_ma, y = rf_gen_median)) +
  geom_ribbon(aes(ymax = rf_gen_max, ymin = rf_gen_min), alpha = 0.5) +
  geom_line(linewidth = 2) +
  scale_x_reverse() +
  labs(x = "Ma", y = "Rarefied generic diversity") +
  coord_geo(xlim = c(max(summary$max_ma), min(summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Plot species
ggplot(summary, aes(x = mid_ma, y = rf_sp_median)) +
  geom_ribbon(aes(ymax = rf_sp_max, ymin = rf_sp_min), alpha = 0.5) +
  geom_line(linewidth = 2) +
  scale_x_reverse() +
  labs(x = "Ma", y = "Rarefied species diversity") +
  coord_geo(xlim = c(max(summary$max_ma), min(summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())
