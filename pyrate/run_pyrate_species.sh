#!/bin/bash

#SBATCH -n 1
#SBATCH --mem-per-cpu 4096
#SBATCH --time 24:00:00
#SBATCH --job-name pyrate
#SBATCH --mail-type=FAIL,END

module load stack/2024-06 gcc/12.2.0 python/3.9.18

python3 /cluster/home/allenb/PyRate-master/PyRate.py pyrate_species.py -A 2 -fixShift epochs.txt -qShift epochs.txt -se_gibbs -n 50000000 -s 5000 -j 1
