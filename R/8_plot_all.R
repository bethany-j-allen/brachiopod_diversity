#Bethany Allen   27th April 2025
#Code to plot graph with all diversity metrics
#Plotting elements are annotated at first use

#setwd("#####")

#Load packages
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(ggthemes)
library(stringr)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
counts <- read_csv("data/counts.csv")

#Raw species plot
#Read count data, plot age on x-axis, value on y-axis, colour by genus/species
raw_sp <- ggplot(counts, aes(x = mid_ma, y = raw, group = level, col = level)) +
          #Add grey polygons representing geological stages behind
          annotate(geom = "rect",
                   xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
                   xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
                   ymin = -Inf, ymax = Inf,
                   colour = "grey", alpha = 0.2, linewidth = 0) +
          #Add dashed line corresponding to the P-T boundary
          geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
          #Add the plot lines, and reverse the x-axis (time moves left to right)
          geom_line(linewidth = 2) + scale_x_reverse() +
          #Set the y-axis label, and remove x-axis label
          labs(x = NULL, y = "Raw count") +
          #Choose black and grey for the two line types
          scale_colour_manual(values = c("darkgrey", "black")) +
          #Plot classic graph style, set font size, remove x-axis labelling
          theme_classic(base_size = 20) + theme(legend.title = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank())

#Raw genus plot
#Here the data are filtered to only include the genus-level estimates
raw_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = raw)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = "Raw count") +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  #The y-axis label is too long so is wrapped over multiple lines
  labs(x = NULL, y = str_wrap("Range-through count", width = 15)) +
  scale_colour_manual(values = c("darkgrey", "black")) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  labs(x = NULL, y = str_wrap("Range-through count", width = 15)) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  #Estimates with an uncertainty range a represented with a line over a ribbon
  geom_ribbon(aes(ymax = rf_max, ymin = rf_min), alpha = 0.5) +
  geom_line(linewidth = 2) + scale_x_reverse() +
  labs(x = NULL, y = str_wrap("Rarefied diversity", width = 15)) +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  labs(x = NULL, y = str_wrap("Rarefied diversity", width = 15)) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  labs(x = NULL, y = str_wrap("Coverage-based diversity est.", width = 15)) +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  labs(x = NULL, y = str_wrap("Coverage-based diversity est.", width = 15)) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  labs(x = NULL, y = str_wrap("Model detrended diversity", width = 15)) +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  #Here an x-axis is added to delineate positive versus negative deviation
  geom_hline(yintercept = 0) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  labs(x = NULL, y = str_wrap("Model detrended diversity", width = 15)) +
  geom_hline(yintercept = 0) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank(),
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
  geom_line(linewidth = 2) +
  #This plot provides the overall x-axis, tick placement is specified
  scale_x_reverse(breaks = c(290, 280, 270, 260, 250, 240)) +
  labs(x = "Millions of years (Ma)",
       y = str_wrap("DeepDive diversity est.", width = 15)) +
  scale_colour_manual(values = c("darkgrey", "black")) +
  scale_fill_manual(values = c("darkgrey", "black")) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank())

#DeepDive genus plot
DD_gen <- ggplot(filter(counts, level == "genera"), aes(x = mid_ma, y = DD_median)) +
  annotate(geom = "rect",
           xmin = c(293.52, 283.5, 268.8, 259.1, 251.9, 247.2),
           xmax = c(290.1, 272.95, 265.1, 254.14, 251.2, 242),
           ymin = -Inf, ymax = Inf,
           colour = "grey", alpha = 0.2, linewidth = 0) +
  geom_vline(xintercept = 251.902, linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(ymax = DD_max, ymin = DD_min), alpha = 0.5) +
  geom_line(linewidth = 2) +
  scale_x_reverse(breaks = c(290, 280, 270, 260, 250, 240)) +
  labs(x = "Millions of years (Ma)",
       y = str_wrap("DeepDive diversity est.", width = 15)) +
  theme_classic(base_size = 20) + theme(legend.title = element_blank())

#Create and arrange composite plots
#Provide all of the input (sub)plots
species_plot <- ggarrange(raw_sp, rt_sp, rf_sp, sqs_sp, rm_sp, DD_sp,
                  #Add letters automatically (in capital), set font size
                  labels = "AUTO", font.label = list(size = 20), label.x = 0.9,
                  #Specify letter placement, stack plots vertically
                  #Align horizontally and vertically so graph size is uniform
                  label.y = 0.9, ncol = 1, nrow = 6, align = "hv")

genus_plot <- ggarrange(raw_gen, rt_gen, rf_gen, sqs_gen, rm_gen, DD_gen,
                  labels = "AUTO", font.label = list(size = 20), label.x = 0.93,
                  label.y = 0.9, ncol = 1, nrow = 6, align = "hv")

#Note that the formatting on these figures may look horrible in R - they are
# configured to fit the output PDF scaling

#Save as PDFs
ggsave(file = "Species_figure.pdf", plot = species_plot, width = 25,
       height = 50, units = "cm")

ggsave(file = "Genus_figure.pdf", plot = genus_plot, width = 25,
       height = 50, units = "cm")
