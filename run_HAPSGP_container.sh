#!/bin/bash
#SBATCH --job-name=HAPSGP_container
#SBATCH --partition=geo
#SBATCH --mem=100G
#SBATCH --cpus-per-task=50
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=1
#SBATCH --output=slurm_messages/runHAPSGP_%j.out
#SBATCH --error=slurm_messages/runHAPSGP_%j.err
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=ALL

export OMP_NUM_THREADS=32
export OPENBLAS_NUM_THREADS=32
# Run the container
apptainer exec \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind $PWD/_targets:/opt/_targets \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/set_input \
  --bind /ddn/gs1/group/set/Projects/beethoven/targets/objects:/set_targets \
  --bind /ddn/gs1/home/messierkp/projects/beethoven:/beethoven \
  HAPSGP_container.sif \
  Rscript /mnt/run.R

