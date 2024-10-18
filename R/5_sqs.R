#Bethany Allen   19th September 2024
#Code to calculate SQS diversity through time

#setwd("#####")

#Load packages
library(tidyverse)
library(iNEXT)
library(palaeoverse)
library(deeptime)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")
glimpse(fossils)

#Generate a table of sample sizes
raw_counts <- group_by(fossils, stage_bin) %>% count()

#Generate list of frequencies by stage, with total number at start
#Achieved by using and trimming the 'count' function in dplyr,
# across a loop of stage names
stage_freq <- list()

for (k in 1:length(stages)) {
  one_stage <- fossils %>% filter(stage_bin == stages[k])
  spec_list <- count(one_stage, accepted_name) %>% arrange(desc(n)) %>%
    add_row(n = length(unique(one_stage$collection_no_pooled)), .before = 1) %>%
    select(n)
  spec_list <- unlist(spec_list, use.names = F)
  stage_freq[[k]] <- spec_list
}
names(stage_freq) <- stages
glimpse(stage_freq)

#Estimate D using estimateD in iNEXT, quorum of 0.8
estD <- estimateD(stage_freq, q = 0, datatype = "incidence_freq",
                  base = "coverage", level = 0.8)

#Add sample size in additional column (from first value in lists)
estD$reference_t <- unlist(lapply(stage_freq, '[[', 1))

#Remove values when t is more than two times the sample size
estD[which(estD$t >= 2 * estD$reference_t),
     c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)

View(estD)

#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
estD <- left_join(estD, midpoint_data, by = join_by(Assemblage == interval_name))

#Plot
ggplot(estD, aes(x = mid_ma, y = qD)) +
  geom_ribbon(aes(ymax = qD.UCL, ymin = qD.LCL), alpha = 0.5) +
  geom_line(linewidth = 2) +
  scale_x_reverse() +
  labs(x = "Ma", y = "SQS diversity") +
  coord_geo(xlim = c(max(estD$max_ma), min(estD$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title=element_blank())
      