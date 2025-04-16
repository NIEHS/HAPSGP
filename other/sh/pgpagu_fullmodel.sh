#!/bin/bash

#SBATCH --job-name=pgpagu_fullmodel_21
#SBATCH --error=pgpagu_fullmodel_21.error
#SBATCH --output=pgpagu_fullmodel_21.out
#SBATCH --mail-user=kassienma@nih.gov
#SBATCH --mail-type=START,END,FAIL
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --mem=500G
#SBATCH --cpus-per-task=100

Rscript pgpmodelagu_fulldata.R