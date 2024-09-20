#Bethany Allen   19th September 2024
#Code to calculate squares diversity through time

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
summary <- data.frame(stages, squares = squares_list)
