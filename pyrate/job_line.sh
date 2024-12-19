#!/bin/bash

module load stack/2024-06 gcc/12.2.0 python/3.9.18

sbatch --time 24:00:00 --job-name pyrate --array=1-2 --wrap="python3 /cluster/home/allenb/PyRate-master/PyRate.py pyrate_genera.py -A 2 -fixShift epochs.txt -qShift epochs.txt -n 50000000 -j \$SLURM_ARRAY_TASK_ID"