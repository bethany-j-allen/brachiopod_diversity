#Bethany Allen   20th August 2024
#Code to clean PBDB brachiopod data

#setwd("#####")

#Load packages
library(tidyverse)

#Create a vector giving the chronological order of stages
stages <- c("Asselian", "Sakmarian", "Artinskian", "Kungurian", "Roadian",
            "Wordian", "Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
            "Olenekian", "Anisian", "Ladinian")

#Create a vector giving the chronological order of substages
substages <- c("Griesbachian", "Dienerian", "Smithian", "Spathian", "Aegean",
               "Bithynian", "Pelsonian", "Illyrian", "Fassanian", "Longobardian")


#Read in dataset
fossils <- read_csv("data/brachiopods_raw.csv", skip = 20)
glimpse(fossils)

#Add filters to remove uncertain IDs
#fossils <- fossils %>% filter(!str_detect(identified_name, " cf")) %>%
# filter(!str_detect(identified_name, " aff")) %>%
# filter(!str_detect(identified_name, '"')) %>%
# filter(!str_detect(identified_name, " \\?")) %>%
# filter(!str_detect(identified_name, "ex gr."))


#Bin occurrences by stage
#Create column for stage designation
fossils$stage_bin <- NA

#For each occurrence
for (i in 1:nrow(fossils)){
  #If occurrence is dated to a single stage, allocate it to that bin
  if (fossils$early_interval[i] %in% stages & is.na(fossils$late_interval[i])){
    fossils$stage_bin[i] <- fossils$early_interval[i]}
  #If occurrence is dated to Griesbachian/Dienerian or both, it is Induan
  if (fossils$early_interval[i] %in% substages[1:2] & is.na(fossils$late_interval[i])){
    fossils$stage_bin[i] <- "Induan"}
  if (fossils$early_interval[i] == substages[1] & !is.na(fossils$late_interval[i])){
    if(fossils$late_interval[i] == substages[2]){fossils$stage_bin[i] <- "Induan"}}
  #If occurrence is dated to Smithian/Spathian or both, it is Olenekian
  if (fossils$early_interval[i] %in% substages[3:4] & is.na(fossils$late_interval[i])){
    fossils$stage_bin[i] <- "Olenekian"}
  if (fossils$early_interval[i] == substages[3] & !is.na(fossils$late_interval[i])){
    if(fossils$late_interval[i] == substages[4]){fossils$stage_bin[i] <- "Olenekian"}}
  #If occurrence is dated to Aegean/Bithynian/Pelsonian/Illyrian or a combination, it is Anisian
  if (fossils$early_interval[i] %in% substages[5:8] & is.na(fossils$late_interval[i])){
    fossils$stage_bin[i] <- "Anisian"}
  if (fossils$early_interval[i] %in% substages[5:8] & !is.na(fossils$late_interval[i])){
    if(fossils$late_interval[i] %in% substages[5:8]){fossils$stage_bin[i] <- "Anisian"}}
  #If occurrence is dated to Fassanian/Longobardian or both, it is Ladinian
  if (fossils$early_interval[i] %in% substages[9:10] & is.na(fossils$late_interval[i])){
    fossils$stage_bin[i] <- "Ladinian"}
  if (fossils$early_interval[i] == substages[9] & !is.na(fossils$late_interval[i])){
    if(fossils$late_interval[i] == substages[10]){fossils$stage_bin[i] <- "Ladinian"}}
}

#Remove occurrences undated at stage resolution
fossils <- filter(fossils, !is.na(stage_bin))


#Retain uncatalogued species
#If an occurrence is to species level but the species hasn't been entered into the database, convert
# its accepted name/rank to the species rather than the genus
for (j in 1:nrow(fossils)){
  if(!is.na(fossils$difference[j]))
    (if (fossils$difference[j] == "species not entered"){
      fossils$accepted_name[j] <- fossils$identified_name[j]
      fossils$accepted_rank[j] <- "species"})
}


#Pool collections to produce unique spatio-temporal units
#Collapse together collections which have the same time bins and coordinates to
# 2dp (and are likely different beds from the same locality)
fossils <- mutate(fossils, lng = round(lng, digits = 2), lat = round(lat, digits = 2))
unique_points <- fossils %>%
  dplyr::select(collection_no_pooled = collection_no, lng, lat, stage_bin) %>%
  distinct(lng, lat, stage_bin, .keep_all = T)
fossils <- left_join(fossils, unique_points, by = c("lng", "lat", "stage_bin"))


#Remove synonymy repeats (combinations of the same *pooled* collection no. AND
# accepted name)
fossils <- distinct(fossils, accepted_name, collection_no_pooled, .keep_all = T)


#Summarise distribution of occurrences
count(fossils, stage_bin)


#Save cleaned dataset
write.csv(fossils, "data/brachiopods_clean.csv", row.names = F)
