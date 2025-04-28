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

#Genera
#Convert to ranges
gen_ranges <- tax_range_time(fossils, name = "genus", min_ma = "min_ma",
                         max_ma = "max_ma")

#Convert into pseudo-occurrences
gen_pseud_occs <- tax_expand_time(gen_ranges, min_ma = "min_ma",
                                  max_ma = "max_ma")

#Remove column duplications
gen_pseud_occs <- gen_pseud_occs[,-c(3,4)]

#Count the number of times each stage appears
gen_richness <- group_by(gen_pseud_occs, interval_name) %>% count()

#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
gen_summary <- left_join(gen_richness, midpoint_data,
                         by = join_by(interval_name))

#Species
#Convert to ranges
sp_ranges <- tax_range_time(fossils, name = "accepted_name",
                            min_ma = "min_ma", max_ma = "max_ma")

#Convert into pseudo-occurrences
sp_pseud_occs <- tax_expand_time(sp_ranges, min_ma = "min_ma",
                                 max_ma = "max_ma")

#Remove column duplications
sp_pseud_occs <- sp_pseud_occs[,-c(3,4)]

#Count the number of times each stage appears
sp_richness <- group_by(sp_pseud_occs, interval_name) %>% count()

#Add stage midpoints
#midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
sp_summary <- left_join(sp_richness, midpoint_data, by = join_by(interval_name))


#Save to counts table
counts <- read_csv("data/counts.csv")
colnames(gen_richness) <- c("stage", "rt")
gen_richness$level <- "genera"
colnames(sp_richness) <- c("stage", "rt")
sp_richness$level <- "species"
gen_richness <- rbind(gen_richness, sp_richness)
test <- left_join(counts, gen_richness, by = c("stage", "level"))
write_csv(counts, "data/counts.csv")


#Plot genera
ggplot(gen_summary, aes(x = mid_ma, y = n)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Generic diversity") +
  coord_geo(xlim = c(max(gen_summary$max_ma), min(gen_summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Plot species
ggplot(sp_summary, aes(x = mid_ma, y = n)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Species diversity") +
  coord_geo(xlim = c(max(sp_summary$max_ma), min(sp_summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())
