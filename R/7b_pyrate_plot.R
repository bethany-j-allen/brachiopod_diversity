#Bethany Allen   13th November 2024
#Code to summarise and plot PyRate results

#setwd("#####")

#Load packages
library(tidyverse)
library(coda)
library(palaeoverse)
library(deeptime)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
log <- read_tsv("pyrate/genera_logs/rep_3_BDS_mcmc.log")

#Burn in
log <- slice_tail(log, prop = 0.9)

#Extract median origination and extinction times from rep
start_times <- select(log, ends_with("TS"))
start_vector <- apply(start_times, 2, median)
end_times <- select(log, ends_with("TE"))
end_vector <- apply(end_times, 2, median)

#Collate into dataframe
ranges <- data.frame(genus = names(start_vector),
                      start = start_vector,
                      end = end_vector)

#Convert into pseudo-occurrences
pseud_occs <- tax_expand_time(ranges,
                        max_ma = "start",
                        min_ma = "end")

#Count the number of times each stage appears
richness <- group_by(pseud_occs, interval_name) %>% count()

#Add stage midpoints
richness <- filter(richness, interval_name %in% stages)
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
richness <- left_join(richness, midpoint_data, by = join_by(interval_name))

#Plot
ggplot(richness, aes(x = mid_ma, y = n)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Generic diversity") +
  #coord_geo(xlim = c(max(counts$max_ma), min(counts$min_ma)),
  #          pos = as.list(rep("bottom", 2)),
  #          dat = list("stages", "periods"),
  #          height = list(unit(4, "lines"), unit(2, "line")),
  #          rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title=element_blank())
