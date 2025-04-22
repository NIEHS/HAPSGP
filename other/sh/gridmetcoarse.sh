#!/bin/bash

#SBATCH --job-name=gmetcrse21
#SBATCH --error=gmetcrse21.error
#SBATCH --output=gmetcrse21.out
#SBATCH --mail-user=kassienma@nih.gov
#SBATCH --mail-type=START,END,FAIL
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --mem=200G
#SBATCH --cpus-per-task=50

Rscript gridmet_coarsegrid.R