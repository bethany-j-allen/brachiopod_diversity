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
  results_table[(i*2-1),4] <- round(((counts$raw[genus_rows[i+1]] -
                           counts$raw[genus_rows[i]])/counts$raw[genus_rows[i]]) * 100, digits = 2)
  results_table[(i*2),4] <- round(((counts$raw[species_rows[i+1]] -
                           counts$raw[species_rows[i]])/counts$raw[species_rows[i]]) * 100, digits = 2)

  #Range-through
  results_table[(i*2-1),5] <- round(((counts$rt[genus_rows[i+1]] -
                                  counts$rt[genus_rows[i]])/counts$rt[genus_rows[i]]) * 100, digits = 2)
  results_table[(i*2),5] <- round(((counts$rt[species_rows[i+1]] -
                                counts$rt[species_rows[i]])/counts$rt[species_rows[i]]) * 100, digits = 2)

  #Rarefaction
  results_table[(i*2-1),6] <- round(((counts$rf_median[genus_rows[i+1]] -
                                  counts$rf_median[genus_rows[i]])/counts$rf_median[genus_rows[i]]) * 100, digits = 2)
  results_table[(i*2),6] <- round(((counts$rf_median[species_rows[i+1]] -
                                counts$rf_median[species_rows[i]])/counts$rf_median[species_rows[i]]) * 100, digits = 2)

  #Squares
  results_table[(i*2-1),7] <- round(((counts$squares[genus_rows[i+1]] -
                                  counts$squares[genus_rows[i]])/counts$squares[genus_rows[i]]) * 100, digits = 2)
  results_table[(i*2),7] <- round(((counts$squares[species_rows[i+1]] -
                                counts$squares[species_rows[i]])/counts$squares[species_rows[i]]) * 100, digits = 2)

  #SQS
  results_table[(i*2-1),8] <- round(((counts$qD[genus_rows[i+1]] -
                                  counts$qD[genus_rows[i]])/counts$qD[genus_rows[i]]) * 100, digits = 2)
  results_table[(i*2),8] <- round(((counts$qD[species_rows[i+1]] -
                                counts$qD[species_rows[i]])/counts$qD[species_rows[i]]) * 100, digits = 2)
}

#Rename columns
colnames(results_table) <- c("stage", "mid_ma", "level", "raw", "rt", "rf",
                             "squares", "sqs")

#Write table
write_csv(results_table, "data/differences.csv")

#Split table
genus_results <- filter(results_table, level == "genera")
species_results <- filter(results_table, level == "species")
