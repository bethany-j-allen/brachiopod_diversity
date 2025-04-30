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

#Raw species plot
raw_sp <- ggplot(counts, aes(x = mid_ma, y = raw, group = level, col = level)) +
          geom_line(linewidth = 2) + scale_x_reverse() +
          labs(x = NULL, y = "Raw count") +
          scale_colour_manual(values = c("grey", "black")) +
          theme_classic() + theme(legend.title = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank())

#Raw genus plot
raw_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = raw)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Raw count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Range-through species plot
rt_sp <- ggplot(counts, aes(x = mid_ma, y = rt, group = level, col = level)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through count") +
  scale_colour_manual(values = c("grey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Range-through genus plot
rt_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = rt)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Rarefied species plot
rf_sp <- ggplot(counts, aes(x = mid_ma, y = rf_median, group = level,
                            col = level, fill = level)) +
  geom_ribbon(aes(ymax = rf_max, ymin = rf_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied diversity") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_fill_manual(values = c("grey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Rarefied genus plot
rf_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma,
                                                        y = rf_median)) +
  geom_ribbon(aes(ymax = rf_max, ymin = rf_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Squares species plot
squares_sp <- ggplot(counts, aes(x = mid_ma, y = squares, group = level,
                              col = level)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Squares diversity") +
  scale_colour_manual(values = c("grey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Squares genus plot
squares_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma,
                                                             y = squares)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Squares diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#SQS species plot
sqs_sp <- ggplot(counts, aes(x = mid_ma, y = qD, group = level, col = level,
                          fill = level)) +
  geom_ribbon(aes(ymax = qD_UCL, ymin = qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "SQS diversity") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_fill_manual(values = c("grey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#SQS genus plot
sqs_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = qD)) +
  geom_ribbon(aes(ymax = qD_UCL, ymin = qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "SQS diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Residual modelling genus plot
rm_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma,
                                                        y = RM_resid)) +
  geom_ribbon(aes(ymax = RM_sdupper, ymin = RM_sdlower), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Model detrended diversity") +
  geom_hline(yintercept = 0) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Residual modelling species plot
rm_sp <- ggplot(filter(counts, level == "species"), aes(x = mid_ma,
                                                        y = RM_resid)) +
  geom_ribbon(aes(ymax = RM_sdupper, ymin = RM_sdlower), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Model detrended diversity") +
  geom_hline(yintercept = 0) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Create time axis
axis_sp <- ggplot(counts, aes(x = mid_ma)) +
  scale_x_reverse() + labs(x = NULL) +
  coord_geo(xlim = c(max(counts$max_ma + 3.6), min(counts$min_ma - 8)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

axis_gen <- ggplot(counts, aes(x = mid_ma)) +
  scale_x_reverse() + labs(x = NULL) +
  coord_geo(xlim = c(max(counts$max_ma + 3.4), min(counts$min_ma)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Create and arrange composite plots
species_plot <- ggarrange(raw_sp, rt_sp, rf_sp, squares_sp, sqs_sp, rm_sp,
                          axis_sp,
                  labels = c("A", "B", "C", "D", "E", NULL),
                  ncol = 1, nrow = 7)

genus_plot <- ggarrange(raw_gen, rt_gen, rf_gen, squares_gen, sqs_gen, rm_gen,
                        axis_gen,
                          labels = c("A", "B", "C", "D", "E", NULL),
                          ncol = 1, nrow = 7)

#Save
ggsave(file = "Species_figure.pdf", plot = species_plot, width = 25,
       height = 40, units = "cm")

ggsave(file = "Genus_figure.pdf", plot = genus_plot, width = 25,
       height = 40, units = "cm")
