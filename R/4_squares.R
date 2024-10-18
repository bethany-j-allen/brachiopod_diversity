#Bethany Allen   19th September 2024
#Code to calculate squares diversity through time

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

#Generate list of frequencies by stage
#Achieved by using and trimming the 'count' function in dplyr,
# across a loop of stage names
stage_freq <- list()

for (k in 1:length(stages)) {
  spec_list <- fossils %>% filter(stage_bin == stages[k]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  spec_list <- unlist(spec_list, use.names = F)
  stage_freq[[k]] <- spec_list
}
names(stage_freq) <- stages
glimpse(stage_freq)

#Estimate diversity using squares method (Alroy, 2018)
squares_list <- vector("numeric", length = 0)

for(i in 1:length(stage_freq)) {
  freq_list <- stage_freq[[i]]
  if(is.na(freq_list[1])){freq_list <- 0}
  if(freq_list[1] == 0){squares <- 0} else {
    sp_count <- length(freq_list)
    sing_count <- sum(freq_list == 1)
    ind_count <- sum(freq_list)
    sum_nsq <- sum(freq_list^2)
    squares <- sp_count + (((sing_count^2)*sum_nsq)/((ind_count^2) - (sing_count*sp_count)))
    if(squares == Inf){squares <- length(freq_list)}
  }
  squares_list <- append(squares_list, squares)
}

#Label squares estimates with stages
summary <- data.frame(stage = stages, squares = squares_list)

#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
summary<- left_join(summary, midpoint_data, by = join_by(stage == interval_name))

#Plot
ggplot(summary, aes(x = mid_ma, y = squares)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Squares diversity") +
  coord_geo(xlim = c(max(counts$max_ma), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title=element_blank())
