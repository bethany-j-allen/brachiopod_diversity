#Rachel Warnock  10th October 2024
#Code to prepare data for input into PyRate

#Set options
#Choose number of replicates for age uncertainty
reps <- 50
#Set species = TRUE for species, species = FALSE for genera
species <- TRUE
#Set seed
set.seed(123)

#Read in data
data <- read.csv(file = "data/brachiopods_clean.csv", sep = ",",
                 header = TRUE)

#Print number of unique species and genera
length(unique(data$accepted_name))
length(unique(data$genus))

#Create an input file for pyrate
#Designate file name
file = ifelse(species, paste0("data/pyrate_species.py"),
              paste0("data/pyrate_genera.py"))

#Extract taxon names
if(species) names = unique(data$accepted_name) else
            names = unique(data$genus)

#Create python file
cat("#!/usr/bin/env python", "from numpy import * ", "",  sep = "\n",
    file = file, append = FALSE)

#For each age replicate
for(i in 1:reps){
  
  cat("data_", i, "=[", sep = "", file = file, append = TRUE)
  data_py = c()
  
  names_py = c()
  for(j in names){
    
    if(species) tmp <- data[which(data$accepted_name == j),] else 
                tmp <- data[which(data$genus == j),]
    
    times <- unlist(lapply(1:dim(tmp)[1], function(x) { runif(1, tmp$min_ma[x],
                                                              tmp$max_ma[x]) }))
    
    if(length(times) > 0){
      data_py = c(data_py,
                  paste0("array([", paste(times, collapse = ","),"])"))
      names_py = c(names_py, paste0("'", j, "'")) #some redundancy here
    }
  }
  
  # print fossil ages
  cat(paste(data_py, collapse = ",\n"), "]\n", sep = "\n", file = file,
      append = TRUE)
}

cat("d=[", paste0(paste0("data", "_", 1:reps), collapse = ", "), "]\n",
    sep = "", file = file, append = TRUE)
cat("names=[", paste0(paste0("'rep", "_", 1:reps, "'"), collapse = ", ") ,"]\n",
    sep = "", file = file, append = TRUE)
cat("def get_data(i): return d[i]", "def get_out_name(i): return  names[i]",
    sep = "\n", file = file, append = TRUE)
cat("taxa_names=[", paste(names_py, collapse = ","), "]\n", sep = "",
    file = file, append = TRUE)
cat("def get_taxa_names(): return taxa_names", sep = "\n", file = file,
    append = TRUE)

