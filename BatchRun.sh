#!/bin/bash

# set output and error output filenames, %j will be replaced by Slurm with the jobid
#SBATCH -o testing%j.out
#SBATCH -e testing%j.err 

# single node in the "short or defq" partition
#SBATCH -N 1
#SBATCH -p defq
#SBATCH -D /groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016
#SBATCH -t 12-00:00:00
#SBATCH --mail-user=mmann1123@gwu.edu
#SBATCH --mail-type=ALL

# Run the following in bash before starting R
if [ -e $HOME/.Renviron ]; then cp $HOME/.Renviron $HOME/.Renviron.bkp; fi
if [ ! -d $HOME/.Rtmp ] ; then mkdir $HOME/.Rtmp; fi
echo "TMP='$HOME/.Rtmp'" > $HOME/.Renviron


module load gcc/4.9.2
module load proj.4/4.7
module load gdal/gcc/2.2.0
module load R/3.3.3
R


srun R CMD BATCH ./extract_alleas_eta.R




