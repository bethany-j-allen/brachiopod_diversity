#Bethany Allen   27th April 2025
#Code to plot graph with all diversity metrics

#setwd("#####")

#Load packages
library(tidyverse)
library(deeptime)
library(gridExtra)
library(ggpubr)
library(ggthemes)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
counts <- read_csv("data/counts.csv")

#Raw
#Plot species
raw_sp <- ggplot(counts, aes(x = mid_ma, y = raw_species)) +
          geom_line(linewidth = 2) + scale_x_reverse() +
          labs(x = NULL, y = "Raw species count") +
          theme_classic() + theme(legend.title = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank())

#Plot genera
raw_gen <- ggplot(counts, aes(x = mid_ma, y = raw_genera)) +
           geom_line(linewidth = 2) + scale_x_reverse() +
           labs(x = NULL, y = "Raw genus count") +
           theme_classic() + theme(legend.title = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.ticks.x = element_blank())

#Range-through
#Plot species
rt_sp <- ggplot(counts, aes(x = mid_ma, y = rt_species)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through species count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Plot genera
rt_gen <- ggplot(counts, aes(x = mid_ma, y = rt_genera)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through genus count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Rarefied
#Plot species
rf_sp <- ggplot(counts, aes(x = mid_ma, y = rf_sp_median)) +
  geom_ribbon(aes(ymax = rf_sp_max, ymin = rf_sp_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied species diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Plot genera
rf_gen <- ggplot(counts, aes(x = mid_ma, y = rf_gen_median)) +
  geom_ribbon(aes(ymax = rf_gen_max, ymin = rf_gen_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied generic diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Squares
#Plot species
sq_sp <- ggplot(counts, aes(x = mid_ma, y = sp_squares)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Squares species diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Plot genera
sq_gen <- ggplot(counts, aes(x = mid_ma, y = gen_squares)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Squares genus diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#SQS
#Plot species
sqs_sp <- ggplot(counts, aes(x = mid_ma, y = sp_qD)) +
  geom_ribbon(aes(ymax = sp_qD_UCL, ymin = sp_qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "SQS species diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Plot genera
sqs_gen <- ggplot(counts, aes(x = mid_ma, y = gen_qD)) +
  geom_ribbon(aes(ymax = gen_qD_UCL, ymin = gen_qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "SQS generic diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Create time axis
axis <- ggplot(counts, aes(x = mid_ma)) +
  scale_x_reverse() + labs(x = NULL) +
  coord_geo(xlim = c(max(counts$max_ma + 4.8), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Create and arrange composite plots
sp_plot <- ggarrange(raw_sp, rt_sp, rf_sp, sq_sp, sqs_sp, axis,
                    labels = c("A", "B", "C", "D", "E", NULL),
                    ncol = 1, nrow = 6)
gen_plot <- ggarrange(raw_gen, rt_gen, rf_gen, sq_gen, sqs_gen, axis,
                     labels = c("A", "B", "C", "D", "E", NULL),
                     ncol = 1, nrow = 6)

#Save
ggsave(file = "Species_figure.pdf", plot = sp_plot,
       width = 25, height = 40, units = "cm")
ggsave(file = "Genus_figure.pdf", plot = gen_plot,
       width = 25, height = 40, units = "cm")
