#Alexander Dunhill   19th September 2024
#Code to calculate residual diversity through time

#setwd("#####")

#Load packages
library(earth)
library(nlme)
library(paleoTS)
library(plotrix)
library(tidyverse)

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")
glimpse(fossils)

#Count formations per stage bin
formation_counts <- fossils %>%
  filter(!is.na(formation)) %>%
  distinct(formation, stage_bin, .keep_all = T) %>%
  count(stage_bin)
colnames(formation_counts) <- c("stage", "formations")

#Source code from Graeme Lloyd
source("R/Lloyd_RM_code.R")

#Load data
counts <- read.csv("data/counts.csv")

#Collate table of necessary parameters
input_data <- select(counts, stage, mid_ma, level, raw)
input_data <- left_join(input_data, formation_counts, by = "stage")

#Split table for analyses
genera <- filter(input_data, level == "genera")
species <- filter(input_data, level == "species")

#Correlation tests
cor.test(genera$formations, genera$raw, method = "spearman")
cor.test(species$formations, species$raw, method = "spearman")

#Run the model code
gen_results <- rockmodel.predictCI(genera$formations, genera$raw)
sp_results <- rockmodel.predictCI(species$formations, species$raw)

#Exploratory plots
plot(genera$formations, genera$raw)
lines(genera$formations, predict(gen_results$model, list(x=genera$formations)))
plot(species$formations, species$raw)
lines(species$formations, predict(sp_results$model, list(x=species$formations)))

#Convert predictions to residuals
gen_resid <- genera$raw - gen_results$predicted
sp_resid <- species$raw - sp_results$predicted
gen_sdlower <- genera$raw - gen_results$sdlowerCI
sp_sdlower <- species$raw - sp_results$sdlowerCI
gen_sdupper <- genera$raw - gen_results$sdupperCI
sp_sdupper <- species$raw - sp_results$sdupperCI

#Save to counts table (Graeme recommends standard deviation)
counts$RM_resid <- c(rbind(gen_resid, sp_resid))
counts$RM_sdlower <- c(rbind(gen_sdlower, sp_sdlower))
counts$RM_sdupper <- c(rbind(gen_sdupper, sp_sdupper))
write_csv(counts, "data/counts.csv")

#Plot sampled and modelled diversity
par(mar = c(5,5,1,1), mfrow = c(1,1))
plot(genera$mid_ma, gen_results$predicted, type = "l",
     xlim = c(max(genera$mid_ma), min(genera$mid_ma)),
     ylim = c(max(gen_results$predicted), min(gen_results$predicted)),
     xlab = "Time (Ma)",
     ylab = "Model Detrended Taxonomic Richness")
polygon(x = c(genera$mid_ma, rev(genera$mid_ma)),
        y = c(gen_results$predicted,
              rep(0, length(genera$mid_ma))),
              col="grey", border = 0)
points(genera$mid_ma, gen_results$predicted, type = "l")
lines(genera$mid_ma, rep(0, length(genera$mid_ma)))
lines(genera$mid_ma, sp_results$seupperCI-sp_results$predicted, lty = 2)
lines(genera$mid_ma, sp_results$selowerCI-sp_results$predicted, lty = 2)
lines(genera$mid_ma, sp_results$sdupperCI-sp_results$predicted, lty = 4)
lines(genera$mid_ma, sp_results$sdlowerCI-sp_results$predicted, lty = 4)
