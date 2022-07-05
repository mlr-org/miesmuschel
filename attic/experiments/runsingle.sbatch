#!/bin/bash

# run:
#   sbatch --c <cpus> -p <partitions> --dependency=afterany:<id> -J <name> --export=ALL,FILE=<file>

#SBATCH --time=4-00:00:00
#SBATCH --ntasks=1
#SBATCH -o ./%x.%J.%N.%t.out


# -u: unbuffered: forward output directly
# -n 1: one task
# batchtools -c option gives number of cores TOTAL on the machine, which is 48 at max
echo "DOING $FILE"
./rerun.sh Rscript run_mbo.R -b -c 128 -f $FILE 
