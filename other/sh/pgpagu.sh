#!/bin/bash

#SBATCH --job-name=pgpagu_4y_f
#SBATCH --error=pgpagu_4yf.error
#SBATCH --output=pgpagu_4yf.out
#SBATCH --mail-user=kassienma@nih.gov
#SBATCH --mail-type=START,END,FAIL
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --mem=500G
#SBATCH --cpus-per-task=100

Rscript pgpmodelagu.r