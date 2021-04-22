#!/bin/bash
#SBATCH --time=0-12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --cpus-per-task=4
#SBATCH --job-name=ExtTSO
#SBATCH --output=METSO_%j.out
#SBATCH --mem=2GB
#SBATCH --mail-user=s.castro.alvarez@rug.nl
#SBATCH --mail-type=BEGIN,END,FAIL

pwd
cd $HOME/METSO
module add R/4.0.0-foss-2020a
module add Mplus/8.4
Rscript tso_cluster.R