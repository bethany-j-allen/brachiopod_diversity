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

#Generate list of frequencies by stage, with total number at start
#Achieved by using and trimming the 'count' function in dplyr,
# across a loop of stage names
sp_stage_freq <- list(); gen_stage_freq <- list()

#Species
for (j in 1:length(stages)) {
  one_stage <- fossils %>% filter(stage_bin == stages[j])
  spec_list <- count(one_stage, accepted_name) %>% arrange(desc(n)) %>%
    add_row(n = length(unique(one_stage$collection_no_pooled)), .before = 1) %>%
    select(n)
  spec_list <- unlist(spec_list, use.names = F)
  sp_stage_freq[[j]] <- spec_list
}
names(sp_stage_freq) <- stages
glimpse(sp_stage_freq)

#Genera
for (k in 1:length(stages)) {
  one_stage <- fossils %>% filter(stage_bin == stages[k])
  gen_list <- count(one_stage, genus) %>% arrange(desc(n)) %>%
    add_row(n = length(unique(one_stage$collection_no_pooled)), .before = 1) %>%
    select(n)
  gen_list <- unlist(gen_list, use.names = F)
  gen_stage_freq[[k]] <- gen_list
}
names(gen_stage_freq) <- stages
glimpse(gen_stage_freq)

#Estimate D using estimateD in iNEXT, quorum of 0.8
sp_estD <- estimateD(sp_stage_freq, q = 0, datatype = "incidence_freq",
                  base = "coverage", level = 0.8)
gen_estD <- estimateD(gen_stage_freq, q = 0, datatype = "incidence_freq",
                     base = "coverage", level = 0.8)

#Add sample size in additional column (from first value in lists)
sp_estD$reference_t <- unlist(lapply(sp_stage_freq, '[[', 1))
gen_estD$reference_t <- unlist(lapply(gen_stage_freq, '[[', 1))

#Remove values when t is more than two times the sample size
sp_estD[which(sp_estD$t >= 2 * sp_estD$reference_t),
     c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)
gen_estD[which(gen_estD$t >= 2 * gen_estD$reference_t),
        c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)

#Save to counts table
counts <- read_csv("data/counts.csv")
summary <- data.frame(stage = gen_estD$Assemblage,
                       gen_qD = gen_estD$qD,
                       gen_qD_LCL = gen_estD$qD.LCL,
                       gen_qD_UCL = gen_estD$qD.UCL,
                       sp_qD = sp_estD$qD,
                       sp_qD_LCL = sp_estD$qD.LCL,
                       sp_qD_UCL = sp_estD$qD.UCL)
counts <- left_join(counts, summary, by = join_by(stage))
write_csv(counts, "data/counts.csv")


#Add stage midpoints
midpoint_data <- select(GTS2020, interval_name, max_ma, mid_ma, min_ma)
summary <- left_join(summary, midpoint_data, by = join_by(stage == interval_name))

#Plot genera
ggplot(summary, aes(x = mid_ma, y = gen_qD)) +
  geom_ribbon(aes(ymax = gen_qD_UCL, ymin = gen_qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) +
  scale_x_reverse() +
  labs(x = "Ma", y = "SQS generic diversity") +
  coord_geo(xlim = c(max(summary$max_ma), min(summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Plot species
ggplot(summary, aes(x = mid_ma, y = sp_qD)) +
  geom_ribbon(aes(ymax = sp_qD_UCL, ymin = sp_qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) +
  scale_x_reverse() +
  labs(x = "Ma", y = "SQS species diversity") +
  coord_geo(xlim = c(max(summary$max_ma), min(summary$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())
