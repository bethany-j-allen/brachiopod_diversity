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
sp_stage_freq <- list(); gen_stage_freq <- list()

#Species
for (j in 1:length(stages)) {
  spec_list <- fossils %>% filter(stage_bin == stages[j]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  spec_list <- unlist(spec_list, use.names = F)
  sp_stage_freq[[j]] <- spec_list
}
names(sp_stage_freq) <- stages
glimpse(sp_stage_freq)

#Genera
for (k in 1:length(stages)) {
  gen_list <- fossils %>% filter(stage_bin == stages[k]) %>%
    count(., genus) %>% arrange(desc(n)) %>% select(n)
  gen_list <- unlist(gen_list, use.names = F)
  gen_stage_freq[[k]] <- gen_list
}
names(gen_stage_freq) <- stages
glimpse(gen_stage_freq)

#Estimate diversity using squares method (Alroy, 2018)
sp_squares_list <- vector("numeric", length = 0)
gen_squares_list <- vector("numeric", length = 0)

#Species
for(i in 1:length(sp_stage_freq)) {
  freq_list <- sp_stage_freq[[i]]
  if(is.na(freq_list[1])){freq_list <- 0}
  if(freq_list[1] == 0){squares <- 0} else {
    sp_count <- length(freq_list)
    sing_count <- sum(freq_list == 1)
    ind_count <- sum(freq_list)
    sum_nsq <- sum(freq_list^2)
    squares <- sp_count + (((sing_count^2)*sum_nsq)/((ind_count^2) - (sing_count*sp_count)))
    if(squares == Inf){squares <- length(freq_list)}
  }
  sp_squares_list <- append(sp_squares_list, squares)
}

#Genera
for(l in 1:length(gen_stage_freq)) {
  freq_list <- gen_stage_freq[[l]]
  if(is.na(freq_list[1])){freq_list <- 0}
  if(freq_list[1] == 0){squares <- 0} else {
    gen_count <- length(freq_list)
    sing_count <- sum(freq_list == 1)
    ind_count <- sum(freq_list)
    sum_nsq <- sum(freq_list^2)
    squares <- gen_count + (((sing_count^2)*sum_nsq)/((ind_count^2) - (sing_count*gen_count)))
    if(squares == Inf){squares <- length(freq_list)}
  }
  gen_squares_list <- append(gen_squares_list, squares)
}

#Label squares estimates with stages
summary <- data.frame(stage = stages, gen_squares = gen_squares_list,
                      sp_squares = sp_squares_list)

#Save to counts table
counts <- read_csv("data/counts.csv")
counts <- left_join(counts, summary, by = join_by(stage))
write_csv(counts, "data/counts.csv")


#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
summary<- left_join(summary, midpoint_data, by = join_by(stage == interval_name))

#Plot genera
ggplot(summary, aes(x = mid_ma, y = gen_squares)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Squares generic diversity") +
  coord_geo(xlim = c(max(counts$max_ma), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Plot species
ggplot(summary, aes(x = mid_ma, y = sp_squares)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Ma", y = "Squares species diversity") +
  coord_geo(xlim = c(max(counts$max_ma), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())
