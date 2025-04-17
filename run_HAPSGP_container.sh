#!/bin/bash
#SBATCH --job-name=HAPSGP_container
#SBATCH --partition=geo
#SBATCH --mem=100G
#SBATCH --cpus-per-task=100
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=1
#SBATCH --output=runHAPSGP0417.out
#SBATCH --error=runHAPSGP0417.err
#SBATCH --mail-user=mariana.kassien@nih.gov
#SBATCH --mail-type=ALL

export OMP_NUM_THREADS=32
export OPENBLAS_NUM_THREADS=32

# Run the container
apptainer exec \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind $PWD/input:/input \
  --bind $PWD/output:/output \
  --bind $PWD/_targets:/opt/_targets \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/set_input \
  --bind /ddn/gs1/group/set/Projects/beethoven/targets/objects:/set_targets \
  --bind /ddn/gs1/home/kassienma/beethoven:/beethoven \
  HAPSGP_container.sif \
  Rscript /mnt/run.R

