#!/bin/bash

#SBATCH --job-name=gridmetcov
#SBATCH --error=gridmetcov.error
#SBATCH --mail-user=kassienma@nih.gov
#SBATCH --mail-type=START,END,FAIL
#SBATCH --ntasks=1

Rscript covariates_processing.R

