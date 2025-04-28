#Bethany Allen   27th April 2025
#Code to plot graph with all diversity metrics

#setwd("#####")

#Load packages
library(tidyverse)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Read in dataset
counts <- read_csv("data/counts.csv")

#Determine which rows have which taxonomic level
genus_rows <- which(counts$level == "genera")
species_rows <- which(counts$level == "species")

#Create table
results_table <- select(counts, stage, mid_ma, level)

for (i in 1:(length(genus_rows) - 1)){
  #Raw
  results_table[(i*2-1),4] <- ((counts$raw[genus_rows[i+1]] -
                           counts$raw[genus_rows[i]])/counts$raw[genus_rows[i]]) * 100
  results_table[(i*2),4] <- ((counts$raw[species_rows[i+1]] -
                           counts$raw[species_rows[i]])/counts$raw[species_rows[i]]) * 100

  #Range-through
  results_table[(i*2-1),5] <- ((counts$rt[genus_rows[i+1]] -
                                  counts$rt[genus_rows[i]])/counts$rt[genus_rows[i]]) * 100
  results_table[(i*2),5] <- ((counts$rt[species_rows[i+1]] -
                                counts$rt[species_rows[i]])/counts$rt[species_rows[i]]) * 100

  #Rarefaction
  results_table[(i*2-1),6] <- ((counts$rf_median[genus_rows[i+1]] -
                                  counts$rf_median[genus_rows[i]])/counts$rf_median[genus_rows[i]]) * 100
  results_table[(i*2),6] <- ((counts$rf_median[species_rows[i+1]] -
                                counts$rf_median[species_rows[i]])/counts$rf_median[species_rows[i]]) * 100

  #Squares
  results_table[(i*2-1),7] <- ((counts$squares[genus_rows[i+1]] -
                                  counts$squares[genus_rows[i]])/counts$squares[genus_rows[i]]) * 100
  results_table[(i*2),7] <- ((counts$squares[species_rows[i+1]] -
                                counts$squares[species_rows[i]])/counts$squares[species_rows[i]]) * 100

  #SQS
  results_table[(i*2-1),8] <- ((counts$qD[genus_rows[i+1]] -
                                  counts$qD[genus_rows[i]])/counts$qD[genus_rows[i]]) * 100
  results_table[(i*2),8] <- ((counts$qD[species_rows[i+1]] -
                                counts$qD[species_rows[i]])/counts$qD[species_rows[i]]) * 100
}

#Rename columns
colnames(results_table) <- c("stage", "mid_ma", "level", "raw", "rt", "rf",
                             "squares", "sqs")

#Write table
write_csv(results_table, "data/differences.csv")

#Split table
genus_results <- filter(results_table, level == "genera")
species_results <- filter(results_table, level == "species")
