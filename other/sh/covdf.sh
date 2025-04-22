#!/bin/bash

#SBATCH --job-name=covdf
#SBATCH --error=covdf.error
#SBATCH --mail-user=kassienma@nih.gov
#SBATCH --mail-type=START,END,FAIL
#SBATCH --ntasks=1

Rscript covariates_fullgrid_agu.r