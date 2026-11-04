#Bethany Allen   27th April 2025
#Code to plot graph with all diversity metrics

#setwd("#####")

#Load packages
library(tidyverse)
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
          annotate(geom = "rect",
                   xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
                   xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
                   ymin = -Inf, ymax = Inf,
                   colour = "grey", alpha = 0.2, linewidth = 0) +
          geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
          geom_line(linewidth = 2) + scale_x_reverse() +
          labs(x = NULL, y = "Raw count") +
          scale_colour_manual(values = c("darkgrey", "black")) +
          theme_classic() + theme(legend.title = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank())

#Raw genus plot
raw_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = raw)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Raw count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Range-through species plot
rt_sp <- ggplot(counts, aes(x = mid_ma, y = rt, group = level, col = level)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through count") +
  scale_colour_manual(values = c("darkgrey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Range-through genus plot
rt_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = rt)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Range-through count") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Rarefied species plot
rf_sp <- ggplot(counts, aes(x = mid_ma, y = rf_median, group = level,
                            col = level, fill = level)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = rf_max, ymin = rf_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied diversity") +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Rarefied genus plot
rf_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma,
                                                        y = rf_median)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = rf_max, ymin = rf_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Rarefied diversity") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#SQS species plot
sqs_sp <- ggplot(counts, aes(x = mid_ma, y = qD, group = level, col = level,
                          fill = level)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = qD_UCL, ymin = qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Coverage-based diversity estimate") +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#SQS genus plot
sqs_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = qD)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = qD_UCL, ymin = qD_LCL), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Coverage-based diversity estimate") +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#Residual modelling species plot
rm_sp <- ggplot(counts, aes(x = mid_ma, y = RM_resid, group = level,
                            col = level)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = RM_sdupper, ymin = RM_sdlower), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Model detrended diversity") +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  geom_hline(yintercept = 0) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())


#Residual modelling genus plot
rm_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma,
                                                         y = RM_resid)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = RM_sdupper, ymin = RM_sdlower), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Model detrended diversity") +
  geom_hline(yintercept = 0) +
  theme_classic() + theme(legend.title = element_blank(),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())

#DeepDive species plot
DD_sp <- ggplot(counts, aes(x = mid_ma, y = DD_median, group = level, col = level,
                             fill = level)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = DD_max, ymin = DD_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Millions of years (Ma)", y = "DeepDive diversity estimate") +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  theme_classic()

#DeepDive genus plot
DD_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = DD_median)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = DD_max, ymin = DD_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = "Millions of years (Ma)", y = "DeepDive diversity estimate") +
  theme_classic()

#Create and arrange composite plots
species_plot <- ggarrange(raw_sp, rt_sp, rf_sp, sqs_sp, rm_sp, DD_sp,
                  labels = c("A", "B", "C", "D", "E", "F"),
                  ncol = 1, nrow = 6,
                  heights = c(1, 1, 1, 1, 1, 1))

genus_plot <- ggarrange(raw_gen, rt_gen, rf_gen, sqs_gen, rm_gen, DD_gen,
                  labels = c("A", "B", "C", "D", "E", "F"),
                  ncol = 1, nrow = 6,
                  heights = c(1, 1, 1, 1, 1, 1))

#Save
ggsave(file = "Species_figure.pdf", plot = species_plot, width = 25,
       height = 50, units = "cm")

ggsave(file = "Genus_figure.pdf", plot = genus_plot, width = 25,
       height = 50, units = "cm")
