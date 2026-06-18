#!/bin/bash
#SBATCH --job-name=imputation_nc
#SBATCH --partition=highmem
#SBATCH --mem=100G
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=1
#SBATCH --output=imputation_nc.out
#SBATCH --error=imputation_nc.err
#SBATCH --mail-user=mariana.kassien@nih.gov
#SBATCH --mail-type=ALL

Rscript R/covs_imputed_pred.r