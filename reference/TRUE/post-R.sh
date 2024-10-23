#!/bin/bash --login
#SBATCH -J R-Post
#SBATCH -N 1
#SBATCH -n 7
#SBATCH --time=12:00:00
#SBATCH -p zhang
#SBATCH --mem=0
#SBATCH --exclusive

dir='WRF-only'

cd /scratch/${USER}/POST

conda activate rspatial

echo 'folder:' $dir

date

echo 'extracting METAR time-series...'

Rscript extract_metar.R $dir T2     3d &
Rscript extract_metar.R $dir P      4d &
Rscript extract_metar.R $dir Q2     3d &
Rscript extract_metar.R $dir U10    3d &
Rscript extract_metar.R $dir V10    3d &
Rscript extract_metar.R $dir RAINC  3d &
Rscript extract_metar.R $dir RAINNC 3d &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
rm metar.d0*
