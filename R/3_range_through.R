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

#Convert to ranges
ranges <- tax_range_time(fossils, name = "genus", min_ma = "min_ma",
                         max_ma = "max_ma")

#Convert into pseudo-occurrences
pseud_occs <- tax_expand_time(ranges, min_ma = "min_ma", max_ma = "max_ma")

#Remove column duplications
pseud_occs <- pseud_occs[,-c(3,4)]

#Count the number of times each stage appears
richness <- group_by(pseud_occs, interval_name) %>% count()

#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
summary <- left_join(richness, midpoint_data, by = join_by(interval_name))

#Plot
ggplot(summary, aes(x = mid_ma, y = n)) +
  geom_line(linewidth = 1) + scale_x_reverse() +
  labs(x = "Ma", y = "Generic diversity") +
  coord_geo(xlim = c(max(summary$max_ma), min(summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title=element_blank())
