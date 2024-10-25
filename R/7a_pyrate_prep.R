
# options
reps <- 2 # replicates for age uncertainty, try 10+
species <- TRUE # use FALSE for genera
input <- "../data/brachiopods_clean.csv"

set.seed(123)

data <- read.csv(file = input, sep = ",", header = TRUE)

# always only use samples known at the species level
dat.sp <- data[which(data$accepted_rank == "species"),]

# number of unique species and genera
length(unique(dat.sp$accepted_name)) #5287
length(unique(dat.sp$genus)) #1004

# create an input files for pyrate

# input file name
file = ifelse(species, paste0("species/species.py"), paste0("genus/genus.py")) 

# extract taxon names
if(species) names = unique(dat.sp$accepted_name) else names = unique(dat.sp$genus)

cat("#!/usr/bin/env python", "from numpy import * ", "",  sep = "\n", file = file, append = FALSE)

for(i in 1:reps){
  
  cat("data_", i, "=[", sep = "", file = file, append = TRUE)
  data_py = c()
  
  names_py = c()
  for(j in names){
    
    if(species)
      tmp <- dat.sp[which(dat.sp$accepted_name == j),]
    else 
      tmp <- dat.sp[which(dat.sp$genus == j),]
    
    times <- unlist(lapply(1:dim(tmp)[1], function(x) { runif(1, tmp$min_ma[x], tmp$max_ma[x]) }))
    
    if(length(times) > 0){
      data_py = c(data_py, paste0("array([", paste(times, collapse = ","), "])"))
      names_py = c(names_py, paste0("'", j, "'")) #some redundancy here
    }
  }
  
  # print fossil ages
  cat(paste(data_py, collapse = ",\n"), "]\n", sep = "\n", file = file, append = TRUE)
}

cat("d=[", paste0(paste0("data", "_", 1:reps), collapse = ", "), "]\n", sep = "", file = file, append = TRUE)
cat("names=[", paste0(paste0("'rep", "_", 1:reps, "'"), collapse = ", ") ,"]\n", sep = "", file = file, append = TRUE)
cat("def get_data(i): return d[i]", "def get_out_name(i): return  names[i]", sep = "\n", file = file, append = TRUE)
cat("taxa_names=[", paste(names_py, collapse = ","), "]\n", sep = "", file = file, append = TRUE)
cat("def get_taxa_names(): return taxa_names", sep = "\n", file = file, append = TRUE)

