#!/bin/bash

# SLURM directives
#SBATCH --job-name=pgpagu_pred_21c
#SBATCH --error=pgpagu_pred_21c_%A_%a.error
#SBATCH --output=pgpagu_pred_21c_%A_%a.out
#SBATCH --mail-user=kassienma@nih.gov
#SBATCH --mail-type=ALL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=50G
#SBATCH --cpus-per-task=4

# Define the start and end dates
START_DATE="2021-01-01"
END_DATE="2021-12-31"

# Generate an array of dates in the range
DATES=()
d=$(date -I -d "$START_DATE")
while [ "$d" != "$(date -I -d "$END_DATE + 1 day")" ]; do 
  DATES+=("$d")
  d=$(date -I -d "$d + 1 day")
done

# Calculate the array range dynamically
NUM_TASKS=${#DATES[@]}
MAX_INDEX=$((NUM_TASKS - 1))

# Debugging output
echo "Dates array: ${DATES[@]}"
echo "Number of tasks: $NUM_TASKS"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"

# Get the specific date for the current task
if [ -z "$SLURM_ARRAY_TASK_ID" ]; then
  echo "Error: SLURM_ARRAY_TASK_ID is not set."
  exit 1
fi
PRED_DATE=${DATES[$SLURM_ARRAY_TASK_ID]}

echo "Date for this task: $PRED_DATE"

# Load the R module (if needed)
module load R

# Run the R script with the specified date
Rscript pgpmodelagu_fullpred.R "$PRED_DATE"
