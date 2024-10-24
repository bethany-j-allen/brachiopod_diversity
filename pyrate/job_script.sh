#!/bin/bash

#SBATCH -n 1
#SBATCH --time 24:00:00
#SBATCH -J pyrate
#SBATCH --mail-type=END,FAIL

module load stack/2024-06 gcc/12.2.0 python/3.9.18

python3 PyRate.py genus.py -A 2 -fixShift epochs.txt -qShift epochs.txt -n 20000000
