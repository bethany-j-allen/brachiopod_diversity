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

#Raw plot
raw <- ggplot(counts, aes(x = mid_ma, y = raw, group = level, col = level)) +
          geom_line(linewidth = 2) + scale_x_reverse() +
          labs(x = NULL, y = "Raw count") +
          theme_classic() + theme(legend.title = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank())

#Range-through plot
rt <- ggplot(counts, aes(x = mid_ma, y = rt, group = level, col = level)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Rarefied plot
rf <- ggplot(counts, aes(x = mid_ma, y = rf_median, group = level,
                            col = level, fill = level)) +
  geom_ribbon(aes(ymax = rf_max, ymin = rf_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Squares plot
squares <- ggplot(counts, aes(x = mid_ma, y = squares, group = level,
                              col = level)) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Squares diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#SQS plot
sqs <- ggplot(counts, aes(x = mid_ma, y = qD, group = level, col = level,
                          fill = level)) +
  geom_ribbon(aes(ymax = qD_UCL, ymin = qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "SQS diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Create time axis
axis <- ggplot(counts, aes(x = mid_ma)) +
  scale_x_reverse() + labs(x = NULL) +
  coord_geo(xlim = c(max(counts$max_ma + 3.6), min(counts$min_ma - 8)),
            pos = as.list(rep("bottom", 2)),
            dat = list("stages", "periods"),
            height = list(unit(4, "lines"), unit(2, "line")),
            rot = list(90, 0), size = list(2.5, 5), abbrv = FALSE) +
  theme_classic() + theme(legend.title = element_blank())

#Create and arrange composite plots
main_plot <- ggarrange(raw, rt, rf, squares, sqs, axis,
                  labels = c("A", "B", "C", "D", "E", NULL),
                  ncol = 1, nrow = 6)

#Save
ggsave(file = "Figure.pdf", plot = main_plot, width = 25, height = 40, units = "cm")
