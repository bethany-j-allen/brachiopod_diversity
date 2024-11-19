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

#Create table to store results
collate <- data.frame(stages = stages, test = NA)

for (i in 1:50) {
  print(i)
  
  #Read in dataset
  log <- read_tsv(paste0("pyrate/genera_logs/rep_", i, "_BDS_mcmc.log"),
                         show_col_types = FALSE)

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
  
  #Leftjoin to summary table
  collate <- left_join(collate, richness, by = join_by(stages == interval_name))
}

#Find max and min values
summary <- data.frame(stages = stages, max_val = NA, min_val = NA, mid_val = NA)

for (j in 1:length(stages)) {
  summary$max_val[j] <- max(collate[j, 3:52])
  summary$min_val[j] <- min(collate[j, 3:52])
  summary$mid_val[j] <- median(as.numeric(collate[j, 3:52]))
}

#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
summary <- left_join(summary, midpoint_data, by = join_by(stages == interval_name))

#Plot
ggplot(summary, aes(x = mid_ma, y = mid_val)) +
  geom_ribbon(aes(ymax = max_val, ymin = min_val), alpha = 0.5) +
  geom_line(linewidth = 1) + scale_x_reverse() +
  labs(x = "Ma", y = "Generic diversity") +
  coord_geo(xlim = c(max(summary$max_ma), min(summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title=element_blank())
