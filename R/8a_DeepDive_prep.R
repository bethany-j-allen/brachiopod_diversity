#Bethany Allen   1st May 2025
#Code to generate DeepDive inputs

#setwd("#####")

#Load packages
library(remotes)
remotes::install_github("DeepDive-project/DeepDiveR")
library(DeepDiveR)
library(countrycode)
library(tidyverse)

#Read in dataset
fossils <- read_csv("data/brachiopods_clean.csv")

# List unique country codes
sort(unique(fossils$cc))

# Take ISO 2 letter country codes and convert to continents
fossils$continent <- countrycode(fossils$cc, "iso2c", "continent")

# Label country 'UK' as being in Europe
fossils$continent[which(fossils$cc == "UK")] <- "Europe"

# One NA -> actually "North Atlantic", can also be labelled as Europe
fossils$continent[which(is.na(fossils$cc))] <- "Europe"

# Relabel "Americas" as "South America"
fossils$continent[which(fossils$continent == "Americas")] <-
  "South America"

# Relabel "CA", "US" and "MX" as "North America"
fossils$continent[which(fossils$cc == "CA")] <- "North America"
fossils$continent[which(fossils$cc == "US")] <- "North America"
fossils$continent[which(fossils$cc == "MX")] <- "North America"

# Count number of occurrences from each continent
count(fossils, continent)

# Create dataframe
brach_data <- data.frame(Taxon = fossils$accepted_name,
                         Region = fossils$continent,
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
