#!/bin/bash

#SBATCH -A nsalamin_default
#SBATCH -p normal
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 0-12:00:00
#SBATCH --mem 1G
#SBATCH -o out_%A_%a.txt
#SBATCH -e err_%A_%a.txt
#SBATCH -D /scratch/wally/FAC/FBM/DBC/nsalamin/default/dsilvest/PyRate3/
# SBATCH --exclusive

hostname
module load Bioinformatics/Software/vital-it
python3 PyRate.py test1_FALA.${SLURM_ARRAY_TASK_ID}.py -A 0 -FBDrange 2 -n 5000000 -p 1000000 -s 5000

# I think you should be able to do something like the following
#python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -qShift ~/brachiopod_diversity/pyrate/epochs.txt -j ${SLURM_ARRAY_TASK_ID}

sleep 1
