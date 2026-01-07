#Bethany Allen   6th Jan 2026
#Code to generate DeepDive inputs, including palaeogeographic partitioning

#setwd("#####")

#Load packages
#library(remotes)
#remotes::install_github("DeepDive-project/DeepDiveR")
library(DeepDiveR)
library(palaeoverse)
library(tidyverse)
library(viridis)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")

##Palaeogeography
#Create midpoint age key
time_key <- select(GTS2020, interval_name, mid_ma)

#Generate a column of midpoint ages
fossils <- left_join(fossils, time_key,
                     by = join_by("stage_bin" == "interval_name"))

#Palaeorotate points
fossils <- palaeorotate(occdf = as.data.frame(fossils),
                        age = "mid_ma",
                        model = "MERDITH2021",
                        uncertainty = FALSE,
                        round = 2)

#Remove points which cannot be rotated
fossils <- filter(fossils, !is.na(p_lat))

#Add column to hold geographic realm information
fossils$realm <- NA
#Allocate points to geographic realms (loosely based on literature)
for (i in (1:nrow(fossils))) {
  if (fossils$p_lng[i] < -100 && fossils$p_lat[i] < -50) {
    fossils$realm[i] <- "A"
  } else if (-100 < fossils$p_lng[i] &
             fossils$p_lng[i] < -15 &
             -60 < fossils$p_lat[i] &
             fossils$p_lat[i] < 60) {
    fossils$realm[i] <- "B"
  } else if (fossils$p_lng[i] > 140 &
             fossils$p_lat[i] < -25) {
    fossils$realm[i] <- "C"
  } else if (60 < fossils$p_lng[i] &
             fossils$p_lng[i] < 120 &
             -10 < fossils$p_lat[i] &
             fossils$p_lat[i] < 30) {
    fossils$realm[i] <- "D"
  } else if (120 < fossils$p_lng[i] &
             20 < fossils$p_lat[i] &
             fossils$p_lat[i] < 60) {
    fossils$realm[i] <- "D"
  } else if (0 < fossils$p_lng[i] &
             fossils$p_lng[i] < 150 &
             -75 < fossils$p_lat[i] &
             fossils$p_lat[i] < -25) {
    fossils$realm[i] <- "E"
  } else if (55 < fossils$p_lng[i] &
             fossils$p_lng[i] < 75 &
             -25 < fossils$p_lat[i] &
             fossils$p_lat[i] < -20) {
    fossils$realm[i] <- "E"
  } else if (0 < fossils$p_lng[i] &
             fossils$p_lng[i] < 75 &
             -25 < fossils$p_lat[i] &
             fossils$p_lat[i] < 30) {
    fossils$realm[i] <- "F"
  } else if (20 < fossils$p_lng[i] &
             fossils$p_lng[i] < 80 &
             25 < fossils$p_lat[i] &
             fossils$p_lat[i] < 50) {
    fossils$realm[i] <- "F"
  } else if (50 < fossils$p_lng[i] &
             40 < fossils$p_lat[i]) {
    fossils$realm[i] <- "F"
  } else {fossils$realm[i] <- "G"}
}

#Plot localities by their palaeogeographic realms
ggplot(fossils, aes(x = p_lng, y = p_lat, group = realm, col = realm)) +
  geom_point() +
  scale_colour_viridis(discrete = T)+
  xlim(-180, 180) +
  ylim(-90, 90) +
  theme_classic()

##Create DeepDive input files
# Create dataframe
brach_data <- data.frame(Taxon = fossils$accepted_name,
                         Region = fossils$realm,
                         MinAge = fossils$min_ma,
                         MaxAge = fossils$max_ma,
                         Locality = fossils$collection_no)

# Check for NAs
NA %in% brach_data

# Check oldest possible maximum fossil age
max(brach_data$MaxAge)

# Check youngest possible minumum fossil age
min(brach_data$MinAge)

# Describe vector of bin boundaries - stages from Asselian to Carnian
bins <- c(298.9, 293.52, 290.1, 283.3, 274.4, 266.9, 264.28, 259.51, 254.14,
          251.90, 249.9, 246.7, 241.46, 237)

# Recalibrate time intervals (end of Carnian becomes "present")
brach_data$MinAge <- round(brach_data$MinAge - 237, digits = 3)
brach_data$MaxAge <- round(brach_data$MaxAge - 237, digits = 3)
bins <- round(bins - 237, digits = 3)

# Create input file for DeepDive (one rep for test)
prep_dd_input(
  # Specify occurrence data.frame
  dat = brach_data,
  # Specify vector containing time bin boundaries
  bins = bins,
  # Specify number of replicates
  r = 1,
  # Specify name of created file
  output_file = "data/brachiopod_deepdive_input.csv"
)

# Create config file
config <- create_config(
  # Specify the name for the simulations
  name = "brachiopod",
  # Specify the name of the data file
  data_file = "brachiopod_deepdive_input.csv",
  # Specify vector containing time bin boundaries
  bins = bins,
  # Specify the number of geographic regions to simulate
  n_regions = length(unique(brach_data$Region))
)

# [Bug in DeepDive(R)] Convert extant_sp from NA to 1
edit_config(config = config,
            module = "simulations",
            parameter = "extant_sp",
            value = 1)

# Change working directory
edit_config(config = config,
            module = "general",
            parameter = "wd",
            value = "inputs")

# Make origin older
538 - 237
487 - 237
edit_config(config = config,
            module = "simulations",
            parameter = "root_r",
            value = "301 250")

# Reduce reps to check functionality
edit_config(config = config,
            module = "simulations",
            parameter = "n_training_simulations",
            value = 12)
edit_config(config = config,
            module = "simulations",
            parameter = "n_test_simulations",
            value = 5)
edit_config(config = config,
            module = "empirical_predictions",
            parameter = "replicates",
            value = 5)

# Write the configuration file
config$write("data/brachiopod_config.ini")
